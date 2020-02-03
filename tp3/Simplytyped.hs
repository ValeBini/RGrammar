module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)      = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)     = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)   = Lam t (conversion' (n:b) u)
conversion' b (LNat n)      = Nat n
conversion' b (LR t1 t2 t3) = R (conversion' b t1) (conversion' b t2) (conversion' b t3)
conversion' b (LSuc t)      = Suc (conversion' b t)
conversion' b (LNil)        = Nil
conversion' b (LCons x l)   = Cons (conversion' b x) (conversion' b l)
conversion' b (LRL t1 t2 t3)= RL (conversion' b t1) (conversion' b t2) (conversion' b t3)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (Nat n)               = Nat n
sub i t (R t1 t2 t3)          = R (sub i t t1) (sub i t t2) (sub i t t3)
sub i t (Suc t')              = Suc (sub i t t')
sub i t (Nil)                 = Nil
sub i t (Cons x l)            = Cons (sub i t x) (sub i t l)
sub i t (RL t1 t2 t3)         = RL (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos

eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval _ (Nat n)               = VNat n
eval _ Nil                   = VList VNil
eval e (Cons x l)            = case eval e x of
                 VNat n      -> case eval e l of
                                  VList l' -> VList (VCons n l')
                                  _        -> error "Error de tipo en run-time, verificar type checker"
                 _           -> error "Error de tipo en run-time, verificar type checker"
eval e (Lam _ u :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam _ u :@: Nat n)   = eval e (sub 0 (Nat n) u)
eval e (Lam _ u :@: Nil)     = eval e (sub 0 Nil u)
eval e (Lam _ u :@: (Cons n l)) = eval e (sub 0 (Cons n l) u)
eval e (Lam t u :@: v)       = case eval e v of
                 VLam t' u' -> eval e (Lam t u :@: Lam t' u')
                 VNat n     -> eval e (Lam t u :@: Nat n)
                 VList VNil -> eval e (Lam t u :@: Nil)
                 VList (VCons n l) -> eval e (Lam t u :@: Cons (Nat n) (quote (VList l)))
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (u :@: v)             = case eval e u of
                 VLam t u'  -> eval e (Lam t u' :@: v)
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (R t1 t2 t3)          = case eval e t3 of
                 VNat 0     -> eval e t1
                 VNat n     -> eval e (t2 :@: (R t1 t2 (Nat (n-1))) :@: (Nat (n-1)))
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (Suc t)               = case eval e t of
                 VNat n     -> VNat (n+1)
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (RL t1 t2 t3)         = case eval e t3 of
                 VList VNil -> eval e t1
                 VList (VCons n l) -> eval e (t2 :@: (Nat n) :@: (quote (VList l)) :@: (RL t1 t2 (quote (VList l))))
                 _          -> error "Error de tipo en run-time, verificar type checker"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote (VNat n)   = Nat n
quote (VList VNil) = Nil
quote (VList (VCons n l)) = Cons (Nat n) (quote (VList l))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i)    = ret (c !! i)
infer' _ e (Free n)     = case lookup n e of
                            Nothing -> notfoundError n
                            Just (_,t) -> ret t
infer' c e (t :@: u)    = (infer' c e t) >>= (\tt ->
                          (infer' c e u) >>= (\tu ->
                          case tt of
                            Fun t1 t2 -> if (tu == t1)
                                          then ret t2
                                          else matchError t1 tu
                            _         -> notfunError tt))
infer' c e (Lam t u)    = infer' (t:c) e u >>= \tu ->
                          ret $ Fun t tu
infer' c e (Nat n)      = ret N
infer' c e (Suc t)      = infer' c e t >>= (\tt ->
                          case tt of
                            N -> ret N
                            x -> matchError N x)
infer' c e Nil          = ret ListN
infer' c e (Cons x l)   = (infer' c e x) >>= (\tx ->
                          (infer' c e l) >>= (\tl ->
                          case tx of
                            N -> if (tl==ListN) then (ret ListN) else (matchError ListN tl)
                            _ -> matchError N tx))
infer' c e (R t1 t2 t3) = (infer' c e t1) >>= (\tt1 ->
                          (infer' c e t2) >>= (\tt2 ->
                          (infer' c e t3) >>= (\tt3 ->
                          case tt3 of
                            N -> case tt2 of
                               Fun t (Fun N t') -> if (t==t') then (if (t==tt1) then ret t else matchError t tt1)
                                                                else matchError t t'
                               Fun _ (Fun x _)  -> matchError N x
                               x -> notfunError x
                            x -> matchError N x)))
infer' c e (RL t1 t2 t3) = (infer' c e t1) >>= (\tt1 ->
                           (infer' c e t2) >>= (\tt2 ->
                           (infer' c e t3) >>= (\tt3 ->
                           case tt3 of
                             ListN -> case tt2 of
                                Fun N (Fun ListN (Fun t t')) -> if (t==t') then (if (t==tt1) then ret t else matchError t tt1)
                                                                           else matchError t t'
                                Fun N (Fun x (Fun _ _))  -> matchError ListN x
                                Fun x (Fun _ (Fun _ _))  -> matchError N x
                                x -> notfunError x
                             x -> matchError ListN x)))

----------------------------------
