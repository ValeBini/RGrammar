module PrettyPrinter (
       printGram,     -- pretty printer para gramaticas
       )
       where

import Prelude hiding ((<>))
import Common
import Text.PrettyPrint.HughesPJ

extractNT :: Prod -> NT
extractNT (PT nt t) = nt
extractNT (PN nt t nt') = nt
extractNT (PE nt) = nt

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de gramaticas

pp :: Gram -> Doc
pp (G nts ts ps i) = text "{\n" <> foldr (<>) (text "") (map printProds (map selectProds nts)) <> text "}"
       where selectProds nt = filter (\p -> let nt' = extractNT p in nt==nt') ps

printProds :: [Prod] -> Doc
printProds ps = let nt = extractNT (head ps) 
                     in printNT nt <> text " -> " <> printProds' ps False 

printProds' :: [Prod] -> Bool -> Doc
printProds' [] b = text ";\n"
printProds' (p:ps) b = let i = if b then text " | " else text ""
                        in i <> case p of 
                                   PT nt t -> printT t
                                   PN nt t nt' -> printT t <> text " " <> printNT nt'
                                   PE nt -> text "\\"
                             <> printProds' ps True


printT :: T -> Doc
printT (T t) = text "\"" <> text t <> text "\"" 

printNT :: NT -> Doc
printNT (NT nt) = text nt 

printGram :: Gram -> Doc
printGram g = pp g
