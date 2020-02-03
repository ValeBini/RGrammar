module Common where

data GramTerm = GNT String
              | GT String
              | GTNT String String
              | GEmpty
              | GOr GramTerm GramTerm
              | GRule GramTerm GramTerm
              | GProd GramTerm GramTerm
    deriving (Eq, Show)

newtype T = T {runT :: String}
    deriving (Eq, Ord, Show)

newtype NT = NT {runNT :: String}
    deriving (Eq, Ord, Show)

data Prod = PT NT T | PN NT T NT | PE NT
    deriving (Eq, Ord, Show)

data Gram = G [NT] [T] [Prod] NT
    deriving Show

newtype Sym = Sym {runSym :: Maybe String} -- el simbolo Nothing es la palabra vacia
    deriving (Eq, Ord, Show)

newtype State a = State {runState :: a}
    deriving (Eq, Ord, Show)

data R a = R [(State a, Sym, State a)]
    deriving Show

data F a = F [(State a, Sym, State a)]
    deriving Show

data DFA a = DA [Sym] [State a] (F a) [State a] (State a)

data NFA a = NA [Sym] [State a] (R a) [State a] (State a)
    deriving Show
