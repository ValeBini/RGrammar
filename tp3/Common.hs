module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)

  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = Base
            | Fun Type Type
            | N
            | ListN
            deriving (Show, Eq)


  -- Términos con nombres
  data LamTerm  =  LVar String
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  LNat Int
                |  LR LamTerm LamTerm LamTerm
                |  LSuc LamTerm
                |  LNil
                |  LCons LamTerm LamTerm
                |  LRL LamTerm LamTerm LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name
             | Term :@: Term
             | Lam Type Term
             | Nat Int
             | R Term Term Term
             | Suc Term
             | Nil
             | Cons Term Term
             | RL Term Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term
             | VUnit
             | VNat Int
             | VList Lv
             deriving (Show, Eq)

  data Lv = VNil
          | VCons Int Lv
          deriving (Show, Eq)

  -- Contextos del tipado
  type Context = [Type]