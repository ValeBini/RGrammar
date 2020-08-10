module PrettyPrinter (
       printGram,     -- pretty printer para gramaticas
       )
       where

import Prelude hiding ((<>))
import Common
import Text.PrettyPrint.HughesPJ

extractRNT :: RProd -> NT
extractRNT (RPT nt t) = nt
extractRNT (RPN nt t nt') = nt
extractRNT (RPE nt) = nt

extractLNT :: LProd -> NT
extractLNT (LPT nt t) = nt
extractLNT (LPN nt nt' t) = nt
extractLNT (LPE nt) = nt

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de gramaticas

ppR :: RGram -> Doc
ppR (RG nts ts ps i) = text "\n" <> foldr (<>) (text "") (map printRProds (map selectProds nts)) 
       where selectProds nt = filter (\p -> let nt' = extractRNT p in nt==nt') ps

ppL :: LGram -> Doc
ppL (LG nts ts ps i) = text "\n" <> foldr (<>) (text "") (map printLProds (map selectProds nts)) 
       where selectProds nt = filter (\p -> let nt' = extractLNT p in nt==nt') ps

printRProds :: [RProd] -> Doc
printRProds [] = text ""
printRProds ps = let nt = extractRNT (head ps) 
                     in printNT nt <> text " -> " <> printRProds' ps False 

printRProds' :: [RProd] -> Bool -> Doc
printRProds' [] b = text ";\n"
printRProds' (p:ps) b = let i = if b then text " | " else text ""
                         in i <> case p of 
                                    RPT nt t -> printT t
                                    RPN nt t nt' -> printT t <> text " " <> printNT nt'
                                    RPE nt -> text "\\"
                              <> printRProds' ps True

printLProds :: [LProd] -> Doc
printLProds [] = text ""
printLProds ps = let nt = extractLNT (head ps) 
                     in printNT nt <> text " -> " <> printLProds' ps False 

printLProds' :: [LProd] -> Bool -> Doc
printLProds' [] b = text ";\n"
printLProds' (p:ps) b = let i = if b then text " | " else text ""
                         in i <> case p of 
                                    LPT nt t -> printT t
                                    LPN nt nt' t -> printNT nt' <> text " " <> printT t
                                    LPE nt -> text "\\"
                              <> printLProds' ps True


printT :: T -> Doc
printT (T t) = text "\"" <> text t <> text "\"" 

printNT :: NT -> Doc
printNT (NT nt) = text nt 

printAlph :: [T] -> Doc
printAlph [] = text ""
printAlph [t] = printT t
printAlph (t:ts) = printT t <> text "," <> printAlph ts

alph :: Gram -> [T]
alph (Left (LG _ ts _ _)) = ts
alph (Right (RG _ ts _ _)) = ts

printGram :: Gram -> Doc
printGram g = text "\n{" <> printAlph (alph g) <> text "}\n" <>
              case g of 
                   Left lg -> ppL lg
                   Right rg -> ppR rg

