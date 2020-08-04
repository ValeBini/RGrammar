module Grammar (
    gramTermToDFA,
    dfaToLGram,
    dfaToRGram
    )
    where

import Data.Set (fromList, fold, isSubsetOf, intersection, empty, toAscList, toList, unions, Set) 
import qualified Data.Set as S (map, union)
import Data.List (union, (\\), delete, elemIndex, intersect)
import Data.Lists (unionOf)
import Common
import FA
import Prelude hiding (LT)



----------------------------------------------------------
-------- FUNCIONES PARA CONVERTIR GramTerm A Gram --------
----------------------------------------------------------

-- Transforma un RGramTerm en una RGram
-- A partir del RGramTerm define cuáles son los conjuntos de No Terminales, Terminales, 
-- Reglas de Producción y NT Inicial
rgramTermToGram :: RGramTerm -> RGram
rgramTermToGram (RProd p ps) = let RG nts ts prods _ = rgramTermToGram ps
                                   RG nts' ts' prods' _ = rgramTermToGram p
                               in RG (union nts' nts) (union ts' ts) (union prods' prods) (NT "&")
rgramTermToGram gt@(RRule l r) = let prods = rgramTermToProd gt
                                     (ts, nts) = tntFromProd prods
                                 in RG nts ts prods (NT "&")
    where tntFromProd [] = ([],[])
          tntFromProd (r:rs) = let (t',nt') = tntFromProd rs 
                               in case r of
                                    RPT nt t -> (union [t] t', union [nt] nt')
                                    RPN nt0 t nt1 -> (union [t] t', union [nt0,nt1] nt')
                                    RPE nt -> (t', union [nt] nt')

-- Transforma un RGramTerm que corresponde a un conjunto de Reglas de Producción derechas
-- de un mismo NT separadas por '|' y las transforma en una lista de RProd 
rgramTermToProd :: RGramTerm -> [RProd]
rgramTermToProd (RRule (RNT l) (ROr r rs)) = (convertR (NT l) r):(rgramTermToProd (RRule (RNT l) rs))
rgramTermToProd (RRule RSigma (ROr r rs)) = (convertR (NT "&") r):(rgramTermToProd (RRule RSigma rs))
rgramTermToProd (RRule (RNT l) r) =  [convertR (NT l) r]
rgramTermToProd (RRule RSigma r) = [convertR (NT "&") r]

-- Toma un NT y un RGramTerm que corresponde a un lado derecho de una Regla de 
-- Producción derecha y devuelve la RProd correspondiente
convertR :: NT -> RGramTerm -> RProd
convertR nt REmpty = RPE nt 
convertR nt (RT t) = RPT nt (T t)
convertR nt (RTNT t nt') = RPN nt (T t) (NT nt')
convertR nt (RTSigma t) = RPN nt (T t) (NT "&")

-- Idem rgramTermToGram para gramáticas izquierdas
lgramTermToGram :: LGramTerm -> LGram
lgramTermToGram (LProd p ps) = let LG nts ts prods _ = lgramTermToGram ps
                                   LG nts' ts' prods' _ = lgramTermToGram p
                               in LG (union nts' nts) (union ts' ts) (union prods' prods) (NT "&")
lgramTermToGram gt@(LRule l r) = let prods = lgramTermToProd gt
                                     (ts, nts) = tntFromProd prods
                                 in LG nts ts prods (NT "&")
    where tntFromProd [] = ([],[])
          tntFromProd (r:rs) = let (t',nt') = tntFromProd rs 
                               in case r of
                                    LPT nt t -> (union [t] t', union [nt] nt')
                                    LPN nt0 nt1 t -> (union [t] t', union [nt0,nt1] nt')
                                    LPE nt -> (t', union [nt] nt')

-- Idem rgramTermToProd para Reglas de Producción izquierdas
lgramTermToProd :: LGramTerm -> [LProd]
lgramTermToProd (LRule (LNT l) (LOr r rs)) = (convertL (NT l) r):(lgramTermToProd (LRule (LNT l) rs))
lgramTermToProd (LRule LSigma (LOr r rs)) = (convertL (NT "&") r):(lgramTermToProd (LRule LSigma rs))
lgramTermToProd (LRule (LNT l) r) =  [convertL (NT l) r]
lgramTermToProd (LRule LSigma r) = [convertL (NT "&") r]

-- Idem convertR para Reglas de Producción izquierdas
convertL :: NT -> LGramTerm -> LProd
convertL nt LEmpty = LPE nt
convertL nt (LT t) = LPT nt (T t)
convertL nt (LNTT nt' t) = LPN nt (NT nt') (T t)
convertL nt (LSigmaT t) = LPN nt (NT "&") (T t)

-- Toma un GramTerm y según si es derecha o izquierda usa 
-- rgramTermToGram o lgramTermToGram
gramTermToGram :: GramTerm -> Gram 
gramTermToGram (Left gram) = Left (lgramTermToGram gram)
gramTermToGram (Right gram) = Right (rgramTermToGram gram)


----------------------------------------------------------
------- FUNCIONES PARA CONVERTIR Gramáticas A NFA --------
----------------------------------------------------------

-- Toma una gramática derecha y devuelve un NFA equivalente (cuyos estados son Int)
rgramToNFA :: RGram -> NFA (Maybe String)
rgramToNFA (RG nts ts ps nt) = NA syms states r ac i
  where syms = map (\t -> NSym (Just (runT t))) ts
        states = ((State Nothing):(map (\nt -> State (Just (runNT nt))) nts))
        r =  R (union [(State (Just (runNT s)), NSym (Just (runT x)), State (Just (runNT b))) | s<-nts, x<-ts, b<-nts, elem (RPN s x b) ps]
                        [(State (Just (runNT s)), NSym (Just (runT x)), State Nothing) | s<-nts, x<-ts, elem (RPT s x) ps])
        ac = (State Nothing):[State (Just (runNT s)) | s<-nts, elem (RPE s) ps]
        i = (State (Just (runNT nt)))

-- Toma una gramática izquierda y devuelve un NFA equivalente (cuyos estados son Int)
-- Para hacer esto lo transformamos como si fuera una gramática derecha y luego hacemos
-- reverse del NFA obtenido.
lgramToNFA :: LGram -> NFA (Maybe (Maybe String)) 
lgramToNFA (LG nts ts ps nt) = nfaReverse (NA syms states r ac i)
  where syms = map (\t -> NSym (Just (runT t))) ts
        states = ((State Nothing):(map (\nt -> State (Just (runNT nt))) nts))
        r =  R (union [(State (Just (runNT s)), NSym (Just (runT x)), State (Just (runNT b))) | s<-nts, x<-ts, b<-nts, elem (LPN s b x) ps]
                        [(State (Just (runNT s)), NSym (Just (runT x)), State Nothing) | s<-nts, x<-ts, elem (LPT s x) ps])
        ac = (State Nothing):[State (Just (runNT s)) | s<-nts, elem (LPE s) ps]
        i = (State (Just (runNT nt)))



----------------------------------------------------------
-------- FUNCION PARA CONVERTIR GramTerm EN DFA ----------         
----------------------------------------------------------

-- Función que toma una gramática izquierda, la transforma en NFA,
-- luego en DFA y, por último, la renombra a Int.
lgramToDFA :: LGram -> DFA Int
lgramToDFA lg = dfaStandar (nfaToDFA (lgramToNFA lg))

-- Idem lgramToDFA pero para gramáticas derechas.
rgramToDFA :: RGram -> DFA Int
rgramToDFA rg = dfaStandar (nfaToDFA (rgramToNFA rg))

-- Función que toma un GramTerm y lo transforma en DFA Int.
-- Usaremos esta función para transformar la gramática recién
-- parseada en el DFA que almacenaremos.
gramTermToDFA :: GramTerm -> DFA Int
gramTermToDFA g = either lgramToDFA rgramToDFA (gramTermToGram g)


----------------------------------------------------------
---------- FUNCIONES PARA VOLVER DE DFA A Gram -----------
----------------------------------------------------------

-- Toma un DFA minimal y elimina los estados estancados, es decir, de los
-- que no se puede salir y no son de aceptación.
-- Obs: el resultado de esta operación no será un DFA teóricamente. 
-- Se usará solo para transformarlo a gramática.
minDfaClean :: Eq a => DFA a -> DFA a
minDfaClean dfa@(DA xs st (F f) ac i) = DA xs st' (F f') ac i
    where st' = union exitStates ac 
          exitStates = [s | s<-st, length (inmediateSt s) > 1]
          inmediateSt s = union [s] [s' | s'<-st, elem (s,s') trans]
          trans = map (\(s0,x,s1) -> (s0,s1)) f
          f' = [(s0,x,s1) | (s0,x,s1)<-f, elem s0 st', elem s1 st']


-- Esta función devuelve una lista de NT posibles empezando 
-- por '&' (sigma) y siguiendo por el alfabeto. Consideramos
-- que no se necesitan mas de 27 NT.
ntsList :: Int -> [NT]
ntsList n = let intList = [0 .. (n-1)]
                stList = map (\i -> NT (show i)) intList
            in (NT "&"):stList

-- Convierte un DFA en una gramática derecha para luego poder
-- imprimirla en pantalla.
dfaToRGram :: (Eq a, Ord a) => DFA a -> Gram
dfaToRGram dfa = Right (RG nts ts ps nt)
    where DA xs st (F f) ac i = minDfaClean (minimizeDFA dfa)
          nts = let l = length st
                in ntsList l
          ts = map (\x -> T (runDSym x)) xs
          ps = (map fToProd f) ++ finalprods
          nt = NT "&"
          fToProd (s0,DSym x,s1) = let nt0 = findNT s0
                                       nt1 = findNT s1
                                    in RPN nt0 (T x) nt1
          finalprods = map (\s -> RPE (findNT s)) ac
          findNT s = let Just p = elemIndex s st'
                     in nts !! p
          st' = i:(delete i st)

-- Convierte un DFA en una gramática izquierda para poder
-- impimirla en pantalla. Para esto, hace el reverse del
-- DFA, luego lo convierte a una gramática derecha, y 
-- finalmente invierte las reglas para que sean izquierdas.
dfaToLGram :: (Eq a, Ord a) => DFA a -> Gram 
dfaToLGram dfa = Left (LG nts ts ps nt)
    where Right (RG nts ts rps nt) = dfaToRGram (dfaReverse dfa)
          ps = map invertProds rps
          invertProds p = case p of
                               RPT nt t -> LPT nt t
                               RPN nt0 t nt1 -> LPN nt0 nt1 t
                               RPE nt -> LPE nt  
               
