module Common where

import Data.List.NonEmpty (NonEmpty)

data RGramTerm = RNT String
               | RSigma
               | RT String
               | RTNT String String
               | RTSigma String 
               | REmpty
               | ROr RGramTerm RGramTerm
               | RRule RGramTerm RGramTerm
               | RProd RGramTerm RGramTerm
    deriving (Eq, Show)

data LGramTerm = LNT String
               | LSigma
               | LT String
               | LNTT String String
               | LSigmaT String 
               | LEmpty
               | LOr LGramTerm LGramTerm
               | LRule LGramTerm LGramTerm
               | LProd LGramTerm LGramTerm
    deriving (Eq, Show)

type GramTerm = Either LGramTerm RGramTerm

-- terminal
newtype T = T {runT :: String}
    deriving (Eq, Ord, Show)

-- no terminal
newtype NT = NT {runNT :: String}
    deriving (Eq, Ord, Show)

-- regla de produccion derecha
data RProd = RPT NT T | RPN NT T NT | RPE NT
    deriving (Eq, Ord, Show)

-- regla de produccion izquierda
data LProd = LPT NT T | LPN NT NT T | LPE NT
    deriving (Eq, Ord, Show)

-- gramatica derecha
data RGram = RG [NT] [T] [RProd] NT
    deriving Show

-- gramatica derecha
data LGram = LG [NT] [T] [LProd] NT
    deriving Show

-- gramatica
type Gram = Either LGram RGram 

-- simbolos automatas no deterministas
newtype NSym = NSym {runNSym :: String} 
    deriving (Eq, Ord, Show)

-- simbolos automatas deterministas
newtype DSym = DSym {runDSym :: NonEmpty Char} -- no debe existir la palabra vacía en un DFA
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
    deriving Show

-- non deterministic finite automata
data NFA a = NA [NSym] [State a] (R a) [State a] (State a)
    deriving Show

-- nombres
type Name = String

type GDFA = DFA Int

-- entorno de gramaticas con sus respectivos identificadores
type Env = [(Name, GDFA)]

-- un statement es:
data Stmt = SDef Name SGrammar    -- una asignacion de un grammar aun nombre
          | SEq SGrammar SGrammar -- una consulta de equivalencia de dos grammar
          | SAsk String SGrammar  -- una consulta de si un string es aceptado por

-- un grammar puede ser:          
data SGrammar = SGram Name                -- el nombre de una gramática cargada
              | SUnion SGrammar SGrammar  -- la union de dos gramaticas
              | SInter SGrammar SGrammar  -- la inteseccion de dos gramaticas
              | SRever SGrammar           -- la reversa de una gramatica
              | SConcat SGrammar SGrammar -- la concatenacion de dos gramaticas
              | SComp SGrammar            -- el complemento de una gramatica
              | SDiff SGrammar SGrammar   -- la diferencia entre dos gramaticas
