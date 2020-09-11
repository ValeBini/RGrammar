{
module Parse where
import Common
import Data.Char
import Prelude hiding (LT, GT)
}

%monad { P } { thenP } { returnP }
%name parse_Gram Gram
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
    '-'     { TDiff }
    '\''    { TComplem }
    '?'     { TAsk }

%left '==' 
%nonassoc '?'
%nonassoc '='
%left '+' '-' 
%left '!' 
%left '.'
%nonassoc '~' '\''
%nonassoc '{' '}'
%nonassoc '->'
%nonassoc ';'
%left '|'
%nonassoc '&'


%%

LeftSide     :  NT                            { GNT $1 }
             |  '&'                           { GSigma }

GRight       : T                              { GT $1 }
             | '\\'                           { GEmpty }

RRight       :  T NT                          { RTNT $1 $2 }
             |  T '&'                         { RTSigma $1 }

LRight       :  NT T                          { LNTT $1 $2 }
             |  '&' T                         { LSigmaT $2 }

GRightSide   : GRight '|' GRightSide          { GOr $1 $3 }
             | GRight                         { $1 }

RRightSide   : GRight '|' RRightSide          { ROr $1 $3 }
             | RRight '|' GRightSide          { ROr $1 $3 }
             | RRight '|' RRightSide          { ROr $1 $3 }
             | RRight                         { $1 }

LRightSide   : GRight '|' LRightSide          { LOr $1 $3 }
             | LRight '|' GRightSide          { LOr $1 $3 }
             | LRight '|' LRightSide          { LOr $1 $3 }
             | LRight                         { $1 }

GRule        : LeftSide '->' GRightSide      { GRule $1 $3 }

RRule        : LeftSide '->' RRightSide      { RRule $1 $3 }

LRule        : LeftSide '->' LRightSide      { LRule $1 $3 }

GProd        : GRule ';' GProd                { GProd $1 $3 }
             | GRule ';'                      { $1 }

RProd        : GRule ';' RProd                { RProd $1 $3 }
             | RRule ';' GProd                { RProd $1 $3 }
             | RRule ';' RProd                { RProd $1 $3 }
             | RRule ';'                      { $1 }

LProd        : GRule ';' LProd                { LProd $1 $3 }
             | LRule ';' GProd                { LProd $1 $3 }
             | LRule ';' LProd                { LProd $1 $3 }
             | LRule ';'                      { $1 }

RGram        :  RProd                         { $1 }
             |  GProd                         { $1 }

LGram        :  LProd                         { $1 }

Gram         :  RGram                         { Right $1 }
             |  LGram                         { Left $1 }

Grammar     : NT                              { SGram $1 }
            | Grammar '+' Grammar             { SUnion $1 $3 }
            | Grammar '!' Grammar             { SInter $1 $3 }
            | Grammar '~'                     { SRever $1 }
            | Grammar '.' Grammar             { SConcat $1 $3}
            | Grammar '\''                    { SComp $1 }
            | '(' Grammar ')'                 { $2 }
            | Grammar '-' Grammar             { SDiff $1 $3 }

Stmt        : NT '=' Grammar                  { SDef $1 $3 }
            | Grammar '==' Grammar            { SEq $1 $3 }
            | T '?' Grammar                   { SAsk $1 $3 }

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
happyError = \ s i -> Failed $ "Line "++(show (i::LineNumber))++": Parse error\n"++(s)

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
                | TDiff
                | TAsk
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
                    ('-':cs) -> cont TDiff cs
                    ('\'':cs) -> cont TComplem cs
                    ('?':cs) -> cont TAsk cs
                    unknown -> \line -> Failed $ "Line "++(show line)++": Unable to recognize "++(show $ take 10 unknown)++ "..."
                    where lexNT cs = let (nt, rest) = span isAlphaNum cs 
                                        in cont (TNT nt) rest
                          lexT cs = let (t, rest) = span (/= '"') cs
                                        (s, rest') = span (== ' ') (tail rest)
                                        in if (head rest') == '?' then cont (TT t) rest'
                                           else if t /= [] then cont (TT (filter (/= ' ') t)) rest'
                                                           else \line -> Failed $ "Line "++(show line)++": Empty terminal"
                          consumirBK anidado cl cont s = case s of
                                                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                                                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs
                                                              ('-':('}':cs)) -> case anidado of
			                                                                         0 -> \line -> lexer cont cs (line+cl)
			                                                                         _ -> consumirBK (anidado-1) cl cont cs
                                                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                                                              (_:cs) -> consumirBK anidado cl cont cs

gram_parse s = parse_Gram s 1
stmt_parse s = parse_Stmt s 1
}
