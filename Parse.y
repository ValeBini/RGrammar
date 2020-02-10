{
module Parse where
import Common
import Data.Char
}

%monad { P } { thenP } { returnP }
%name parse Gram

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '\\'    { TEmpty }
    '|'     { TOr }
    '->'    { TArrow }
    '&'     { TSigma }
    ';'     { TEnd }
    '"'     { TQuote }
    T       { TT $$ }
    NT      { TNT $$ }



%%

LeftSide    :  NT                          { GNT $1 }

Right       :  '"' T '"'                   { Common.GT $2 }
            |  '"' T '"' NT                { GTNT $2 $4 }
            |  '\\'                        { GEmpty }

RightSide   : Right                        { $1 }
            | Right '|' RightSide          { GOr $1 $3 }

Prod        : LeftSide '->' RightSide      { GRule $1 $3 }

Gram        : Prod                         { $1 }
            | Prod ';' Gram                { GProd $1 $3 }

{

data ParseResult a = Ok a | Failed String
                     deriving Show

type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)


data Token = TArrow
                | TOr
                | TT String
                | TNT String
                | TSigma
                | TEnd
                | TQuote
                | TEmpty
                | TEOF
               deriving Show

----------------------------------

lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexNT (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs
      	            ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('|':cs) -> cont TOr cs
                    ('&':cs) -> cont TSigma cs
                    ('/':cs) -> cont TEmpty cs
                    (';':cs) -> cont TEnd cs
                    ('"':cs) -> lexT cs
                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexNT cs = let (nt, rest) = span isAlpha cs 
                                        in cont (TNT nt) rest
                          lexT cs = let (t, rest) = span (/= '"') cs
                                        in cont (TT t) (tail rest)
                          consumirBK anidado cl cont s = case s of
                                                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                                                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs
                                                              ('-':('}':cs)) -> case anidado of
			                                                                         0 -> \line -> lexer cont cs (line+cl)
			                                                                         _ -> consumirBK (anidado-1) cl cont cs
                                                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                                                              (_:cs) -> consumirBK anidado cl cont cs

gram_parse s = parse s 1
}
