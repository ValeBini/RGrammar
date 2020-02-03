module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Prelude hiding ((<>))
import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['x','y','z'] ++ ['a'..'w'] ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  _  (Free (Global s)) = text s
pp ii vs (i :@: c)         = sep [parensIf (isLam i) (pp ii vs i),
                                  nest 1 (parensIf (or [isLam c, isR c, isSuc c, isRL c, isCons c, isApp c])  (pp ii vs c))]
pp ii vs (Lam t c)         = text "\\" <>
                                text (vs !! ii) <>
                                text ":" <>
                                printType t <>
                                text ". " <>
                                pp (ii+1) vs c
pp ii vs (Nat x)           = text (show x)
pp ii vs (R t1 t2 t3)      = text "R " <>
                                parensIf (or [isLam t1, isR t1, isSuc t1, isRL t1, isCons t1, isApp t1]) (pp ii vs t1) <>
                                text " " <>
                                parensIf (or [isLam t2, isR t2, isSuc t2, isRL t2, isCons t2, isApp t2]) (pp ii vs t2) <>
                                text " " <>
                                parensIf (or [isLam t3, isR t3, isSuc t3, isRL t3, isCons t3, isApp t3]) (pp ii vs t3)
pp ii vs (Suc t)           = text "suc " <> pp ii vs t
pp ii vs (Nil)             = text "nil"
pp ii vs (Cons x l)        = text "cons " <>
                                parensIf (or [isLam x, isR x, isSuc x, isRL x, isCons x, isApp x]) (pp ii vs x) <>
                                text " " <>
                                parensIf (or [isLam l, isR l, isSuc l, isRL l, isCons l, isApp l]) (pp ii vs l)
pp ii vs (RL t1 t2 t3)     = text "RL " <>
                                parensIf (or [isLam t1, isR t1, isSuc t1, isRL t1, isCons t1, isApp t1]) (pp ii vs t1) <>
                                text " " <>
                                parensIf (or [isLam t2, isR t2, isSuc t2, isRL t2, isCons t2, isApp t2]) (pp ii vs t2) <>
                                text " " <>
                                parensIf (or [isLam t3, isR t3, isSuc t3, isRL t3, isCons t3, isApp t3]) (pp ii vs t3)

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam  _      = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isR :: Term -> Bool
isR (R _ _ _) = True
isR _ = False

isSuc :: Term -> Bool
isSuc (Suc _) = True
isSuc _ = False

isRL :: Term -> Bool
isRL (RL _ _ _) = True
isRL _ = False

isCons :: Term -> Bool
isCons (Cons _ _) = True
isCons _ = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType N            = text "Nat"
printType ListN        = text "List Nat"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1),
                               text "->",
                               printType t2]
isFun :: Type -> Bool
isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Nat _)           = []
fv (R t1 t2 t3)      = (fv t1) ++ (fv t2) ++ (fv t3)
fv (Suc t)           = fv t
fv (Nil)             = []
fv (Cons n l)        = fv n ++ fv l
fv (RL t1 t2 t3)     = fv t1 ++ fv t2 ++ fv t3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t
