module Common where

data GramTerm = GNT String
              | GSigma
              | GT String
              | GTNT String String
              | GTSigma String 
              | GEmpty
              | GOr GramTerm GramTerm
              | GRule GramTerm GramTerm
              | GProd GramTerm GramTerm
    deriving (Eq, Show)

-- terminal
newtype T = T {runT :: String}
    deriving (Eq, Ord, Show)

-- no terminal
newtype NT = NT {runNT :: String}
    deriving (Eq, Ord, Show)

-- regla de produccion
data Prod = PT NT T | PN NT T NT | PE NT
    deriving (Eq, Ord, Show)

-- gramatica
data Gram = G [NT] [T] [Prod] NT
    deriving Show

-- simbolos automatas no deterministas
newtype NSym = NSym {runNSym :: Maybe String} -- el simbolo Nothing es la palabra vacia
    deriving (Eq, Ord, Show)

-- simbolos automatas deterministas
newtype DSym = DSym {runDSym :: String}
    deriving (Eq, Ord, Show)

newtype State a = State {runState :: a}
    deriving (Eq, Ord, Show)

-- relacion
data R a = R [(State a, NSym, State a)]
    deriving Show

-- funcion
data F a = F [(State a, DSym, State a)]
    deriving Show

-- deterministic finite automata
data DFA a = DA [DSym] [State a] (F a) [State a] (State a)

-- non deterministic finite automata
data NFA a = NA [NSym] [State a] (R a) [State a] (State a)
    deriving Show

-- nombres
type Name = String

-- entorno de gramaticas con sus respectivos identificadores
type NameEnv = [(Name, Gram)]

-- un statement es:
data Stmt = SGrammar            -- un grammar
          | SDef Name Grammar   -- una asignacion de un grammar aun nombre
          | SEq Grammar Grammar -- una consulta de equivalencia de dos grammar

-- un grammar puede ser:          
data SGrammar = SGram -- una gramatica simple
              | SUnion Grammar Grammar  -- la union de dos gramaticas
              | SInter Grammar Grammar  -- la inteseccion de dos gramaticas
              | SRever Grammar          -- la reversa de una gramatica
              | SConcat Grammar Grammar -- la concatenacion de dos gramaticas
