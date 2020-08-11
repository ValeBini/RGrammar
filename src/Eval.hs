module Eval (
       eval,
       evalStmt
       )
       where

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Prelude
import PrettyPrinter
import Common
import FA

----------------------------------

-- Mónada estado, con manejo de errores
newtype StateError a = StateError { runStateError :: Env -> Either String (a, Env) }

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM

instance Applicative StateError where
    pure   = return
    (<*>)  = ap

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Name -> m GDFA
    -- Cambia el valor de una variable
    update :: Name -> GDFA -> m ()

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: String -> m a

instance Monad StateError where
  return x = StateError (\s -> Right (x,s))
  StateError g >>= f = StateError (\s-> case (g s) of
                                          Left s -> Left s
                                          Right (a,s') -> runStateError (f a) s')

instance MonadError StateError where
  throw s = StateError (\_ -> Left s)

instance MonadState StateError where
  lookfor v = StateError (\s -> case lookfor' v s of
                                  Nothing -> Left ("Variable " ++ v ++ " no encontrada.")
                                  Just n -> Right (n, s))
              where lookfor' v [] = Nothing
                    lookfor' v ((u, j):ss) | v == u = Just j
                                           | v /= u = lookfor' v ss
  update v i = StateError (\s -> Right ((), update' v i s))
               where update' v i [] = [(v, i)]
                     update' v i ((u, _):ss) | v == u = (v, i):ss
                     update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)



evalG :: (MonadState m, MonadError m) => SGrammar -> m GDFA
evalG (SGram n) = lookfor n
evalG (SUnion g0 g1) = do d0 <- evalG g0
                          d1 <- evalG g1
                          return (dfaUnite d0 d1)
evalG (SInter g0 g1) = do d0 <- evalG g0
                          d1 <- evalG g1
                          return (dfaIntersect d0 d1)
evalG (SRever g) = do d <- evalG g 
                      return (dfaReverse d)
evalG (SConcat g0 g1) = do d0 <- evalG g0
                           d1 <- evalG g1
                           return (dfaConcat d0 d1)
evalG (SComp g) = do d <- evalG g
                     return (dfaComplement d)
evalG (SDiff g0 g1) = do d0 <- evalG g0
                         d1 <- evalG g1
                         return (dfaDiff d0 d1)    

eval :: Env -> SGrammar -> Either String GDFA
eval state sgrammar = case runStateError (evalG sgrammar) state of
                         Left e -> Left e
                         Right (d, st) -> Right d

evalBool :: (MonadState m, MonadError m) => Stmt -> m Bool
evalBool (SEq g0 g1) = do d0 <- evalG g0
                          d1 <- evalG g1
                          return (dfaEq d0 d1)
evalBool (SAsk str g) = do d <- evalG g
                           return (dfaAsk d str)

evalStmt :: Env -> Stmt -> Either String Bool
evalStmt state stmt = case runStateError (evalBool stmt) state of
                        Left e -> Left e
                        Right (b, st) -> Right b
