{
module Parse where
import Common
import Data.Char
import Prelude hiding (LT)
}

%monad { P } { thenP } { returnP }
%name parse_lGram LGram
%name parse_rGram RGram
%name parse_Stmt Stmt

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '\\'    { TEmpty }
    '|'     { TOr }
    '->'    { TArrow }
    '&'     { TSigma }
    ';'     { TEnd }
    '{'     { TOpen }
    '}'     { TClose }
    '('     { TO }
    ')'     { TC }
    T       { TT $$ }
    NT      { TNT $$ }
    '='     { TDef }
    '=='    { TEq }
    '+'     { TUnion }
    '!'     { TIntersec }
    '~'     { TReverse }
    '.'     { TConcat }
    '-'     { TComplem }

%left '=='
%left '='
%left '+' 
%left '!'
%left '.'
%nonassoc '~'
%nonassoc '-'
%nonassoc '{' '}'
%nonassoc '->'
%nonassoc ';'
%left '|'
%nonassoc '&'


%%

RLeftSide    :  NT                            { RNT $1 }
             |  '&'                           { RSigma }

LLeftSide    :  NT                            { LNT $1 }
             |  '&'                           { LSigma }
 
RRight       :  T                             { RT $1 }
             |  T NT                          { RTNT $1 $2 }
             |  T '&'                         { RTSigma $1 }
             |  '\\'                          { REmpty }

LRight       :  T                             { LT $1 }
             |  NT T                          { LNTT $1 $2 }
             |  '&' T                         { LSigmaT $2 }
             |  '\\'                          { LEmpty }

RRightSide   : RRight '|' RRightSide          { ROr $1 $3 }
             | RRight                         { $1 }

LRightSide   : LRight '|' LRightSide          { LOr $1 $3 }
             | LRight                         { $1 }

RRule        : RLeftSide '->' RRightSide      { RRule $1 $3 }

LRule        : LLeftSide '->' LRightSide      { LRule $1 $3 }

RProd        : RRule ';' RProd                { RProd $1 $3 }
             | RRule ';'                      { $1 }

LProd        : LRule ';' LProd                { LProd $1 $3 }
             | LRule ';'                      { $1 }

RGram        : '{' RProd '}'                  { $2 }

LGram        : '{' LProd '}'                  { $2 }

Grammar     : NT                              { SGram $1 }
            | Grammar '+' Grammar             { SUnion $1 $3 }
            | Grammar '!' Grammar             { SInter $1 $3 }
            | Grammar '~'                     { SRever $1 }
            | Grammar '.' Grammar             { SConcat $1 $3}
            | '-' Grammar                     { SComp $2 }
            | '(' Grammar ')'                 { $2 }

Stmt        : NT '=' Grammar                  { SDef $1 $3 }
            | Grammar '==' Grammar            { SEq $1 $3 }

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
                | TEmpty
                | TEOF
                | TOpen
                | TClose
                | TO
                | TC
                | TDef
                | TEq
                | TUnion
                | TIntersec
                | TReverse
                | TConcat
                | TComplem
               deriving Show

----------------------------------

lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlphaNum c -> lexNT (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs
      	            ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('|':cs) -> cont TOr cs
                    ('&':cs) -> cont TSigma cs
                    ('\\':cs) -> cont TEmpty cs
                    (';':cs) -> cont TEnd cs
                    ('"':cs) -> lexT cs
                    ('{':cs) -> cont TOpen cs
                    ('}':cs) -> cont TClose cs
                    ('(':cs) -> cont TO cs
                    (')':cs) -> cont TC cs
                    ('=':('=':cs)) -> cont TEq cs
                    ('=':cs) -> cont TDef cs
                    ('+':cs) -> cont TUnion cs
                    ('!':cs) -> cont TIntersec cs
                    ('~':cs) -> cont TReverse cs
                    ('.':cs) -> cont TConcat cs
                    ('-':cs) -> cont TComplem cs
                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexNT cs = let (nt, rest) = span isAlphaNum cs 
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

lgram_parse s = parse_lGram s 1
rgram_parse s = parse_rGram s 1
stmt_parse s = parse_Stmt s 1
}
