module FA (
    nfaReverse,
    nfaToDFA,
    dfaStandar,
    dfaComplement,
    dfaUnite, 
    dfaIntersect,
    dfaReverse,
    dfaConcat,
    dfaDiff,
    dfaEq,
    minimizeDFA,
    dfaAsk
    )
    where

import Data.Set (fromList, fold, isSubsetOf, intersection, empty, toAscList, toList, unions, Set) 
import qualified Data.Set as S (map, union)
import Data.List (union, (\\), delete, elemIndex, intersect, words, all)
import qualified Data.List.NonEmpty as NE (fromList, toList, NonEmpty)
import Common

----------------------------------------------------------
----------- FUNCIONES PARA CONVERTIR NFA A DFA -----------
----------------------------------------------------------

-- Toma dos relaciones cuyos estados son conjuntos y devuelve True 
-- si la primera está contenida en la segunda, False caso contrario. 
included :: (Ord a) => R (Set a) -> R (Set a) -> Bool
included (R xs) (R ys) = isSubsetOf (fromList xs) (fromList ys)

-- Toma dos listas de estados (los cuales son conjuntos) y se fija si son 
-- equivalentes sin tener en cuenta el orden.
equal :: (Ord a) => [State (Set a)] -> [State (Set a)] -> Bool
equal xs ys = (fromList xs) == (fromList ys)

-- Da el conjunto de los estados (sin el State) a los que se llega 
-- partiendo del conjunto que es dado como primer argumento, con el 
-- símbolo x, en la relación r. 
-- Obs: los direct son los estados a los que llegamos con el símbolo 
-- dado. Los indirect son los estados a los que se llega desde los
-- direct con la cadena vacía.
goesTo :: Ord a => Set a -> NSym -> [State a] -> R a -> Set a
goesTo st x sts (R r) = fold S.union empty (S.map nextSet st)
    where nextSet s = let direct = fromList [runState s' | s'<-sts, elem (State s, x, s') r]
                          indirect = goesTo direct (NSym "") sts (R r)
                      in S.union direct indirect

-- Toma el conjunto de los símbolos, la relación original, la lista de los estados
-- y también dos relaciones (de conjuntos) que las usa de la siguiente manera: en la 
-- primera acumula las relaciones que hasta ahora "consiguió" en total, y en la segunda
-- lleva las relaciones que "consiguió" en la última llamada. Cuando las nuevas relaciones
-- están incluidas en las que lleva acumuladas el algoritmo termina y devuelve la relación
-- acumulada.
dfaF' :: (Ord a) => [NSym] -> R a -> [State a] -> R (Set a) -> R (Set a) -> R (Set a)
dfaF' xs (R rs) sts (R fall) (R flast) = 
    let f = concat (map (\(s1, x1, s1') -> map (\x -> (s1', x, t s1' x)) xs) flast)
    in if included (R f) (R fall) then (R fall) 
                                  else (dfaF' xs (R rs) sts (R (union fall f)) (R f))
         where t st x = State (goesTo (runState st) x sts (R rs)) 

-- Dados el conjunto de símbolos, la relación original, el conjunto de los estados originales
-- y el nuevo estado inicial, devuelve la nueva relación (que tendrá característica de función) 
-- utilizando dfaF'.
dfaF :: (Ord a) => [NSym] -> R a -> [State a] -> State (Set a) -> R (Set a)
dfaF xs (R rs) sts i = let f = map (\x -> (i, x, t i x)) xs
                       in dfaF' xs (R rs) sts (R f) (R f)
        where t st x = State (goesTo (runState st) x sts (R rs))

-- Toma un NFA con estados de tipo 'a' y devuelve un DFA con estados de tipo '[a]'
-- Los símbolos son los mismos (excepto que ya no existe la cadena vacía), los 
-- estados serán los posibles subconjuntos del conjunto de estados original que 
-- sean alcanzados por la función de transición y la función de transición se obtiene 
-- con dfaF. Los estados aceptados serán todos aquellos que incluyan al menos a un estado
-- de aceptación original y el estado inicial será el que contiene al estado inicial 
-- original y a todos los cuales se llega desde este con la cadena vacía.
--nfaToDFA :: NFA (Maybe String) -> DFA [Maybe String]
nfaToDFA :: (Eq a, Ord a) => NFA a -> DFA (Set a)
nfaToDFA (NA xs st (R rs) ac i) = DA xs' st' (F f) ac' i'
    where xs' = map (\(NSym x) -> DSym (NE.fromList x)) (xs \\ [NSym ""])
          i' = State (fromList (union [runState i] [runState s | s<-st, elem (i, NSym "", s) rs]))
          f = map (\(s0,NSym x,s1) -> (s0,DSym (NE.fromList x),s1)) f'
          R f' = (dfaF (xs \\ [NSym ""]) (R rs) st i')
          st' = removeRep ((map (\(a,b,c)->a) f) ++ (map (\(a,b,c)->c) f))
          ac' = [s | s<-st', (intersection (runState s) prevAc) /= empty]
          prevAc = fromList (map runState ac)
          removeRep l = (toList.fromList) l


-- Función para llevar cualquier DFA a un DFA cuyos estados son Int
-- Utilizamos esta función ya que guardamos todas las gramáticas como
-- DFAs y deben tener todas el mismo tipo.
dfaStandar :: Eq a => DFA a -> DFA Int
dfaStandar (DA xs st (F f) ac i) = DA xs st' (F f') ac' i'
    where st' = let l = (length st) - 1
                in map (\x -> State x) [0 .. l]
          f' = map (\(s0, x, s1) -> (rename s0, x, rename s1)) f
          ac' = map rename ac
          i' = rename i
          rename (State s) = let Just pos = elemIndex (State s) st
                             in State pos


----------------------------------------------------------
------------ FUNCIONES PARA MINIMIZAR UN DFA -------------         
----------------------------------------------------------

-- Toma un DFA y devuelve otro quitando todos los estados inalcanzables
-- desde el estado inicial.
removeUnreachable :: Ord a => DFA a -> DFA a
removeUnreachable dfa@(DA xs st (F f) ac i) = DA xs st' (F f') ac' i
    where st' = reachedStates dfa [i]
          f' = [(s0,x,s1) | (s0,x,s1)<-f, elem s0 st', elem s1 st']
          ac' = intersect ac st'

-- Devuelve la partición de estados de la siguiente iteración del algoritmo para conseguir los
-- estados mínimos de un DFA
nextPartition :: (Eq a) => [State a] -> [[State a]] -> [DSym] -> F a -> [[State a]]
nextPartition sts last xs (F f) = concat (map (partition []) last)
    where partition new [] = new
          partition [] (s:ss) = partition [[s]] ss
          partition (p:ps) (s:ss) = if distinguishable (head p) s 
                                        then partition (p:(partition ps [s])) ss
                                        else partition ((s:p):ps) ss
          distinguishable s1 s2 = or (map (\x -> (numOfPart [s | s<-sts, elem (s1,x,s) f] last 0) /= 
                                                 (numOfPart [s | s<-sts, elem (s2,x,s) f] last 0)) xs)
          numOfPart [s] [] i = -1
          numOfPart [s] (p:ps) i = if (elem s p) then i 
                                                 else numOfPart [s] ps (i+1)

-- Devuelve el conjunto de los estados mínimos del DFA
minimumStates :: (Eq a, Ord a) => [DSym] -> [State a] -> [[State a]] -> F a -> [State [a]]
minimumStates xs sts p (F f) = let nextp = nextPartition sts p xs (F f)
                               in if (fromList (map fromList p)) == (fromList (map fromList nextp)) 
                                     then (map (\ss -> State ((map runState ss))) p)
                                     else minimumStates xs sts nextp (F f)

-- Toma un DFA y devuelve otro sin estados indistinguibles
removeUndist :: (Eq a, Ord a) => DFA a -> DFA [a]
removeUndist (DA xs st (F f) ac i) = DA xs st' (F f') ac' i'
    where st' = minimumStates xs st [ac, st \\ ac] (F f)
          f' = [(s0, x, s1) | s0<-st', x<-xs, s1<-st', connect s0 x s1]
          connect s0 x s1 = or (concat (map (\ss0-> (map (\ss1-> elem (State ss0, x, State ss1) f) 
                                                         (runState s1))) 
                                            (runState s0)))
          ac' = [s | s<-st', [ss | ss<-ac, elem (runState ss) (runState s)] /= []]
          i' = let [s] = [s | s<-st', elem (runState i) (runState s)] in s

     
-- Toma un DFA y lo minimiza
minimizeDFA :: (Eq a, Ord a) => DFA a -> DFA [a]
minimizeDFA dfa = removeUndist (removeUnreachable dfa)

----------------------------------------------------------
--------------- FUNCIONES PARA OPERAR DFAS ---------------
----------------------------------------------------------

-- Complemento de un DFA, es el mismo DFA pero cambiando su conjunto de 
-- estados de aceptación por el complemento de este en el conjunto total
-- de estados.
dfaComplement :: Eq a => DFA a -> DFA a
dfaComplement (DA xs st (F f) ac i) = DA xs st (F f) (st \\ ac) i
          
-- UNION E INTERSECCIÓN. En ambos casos el conjunto de estados del nuevo DFA  
-- es el producto cartesiano de los conjuntos de estados de los DFAs originales. 
-- Aceptaremos ciertos estados según el caso.

-- Agrega a un DFA una lista de símbolos que no estan en este. Lo utilizamos
-- para la union e intersección. Agrega un nuevo estado basura a donde van
-- las transiciones de los nuevos símbolos.
addSym :: DFA a -> [DSym] -> DFA (Maybe a)
addSym (DA xs st (F f) ac i) new = DA xs' st' (F f') ac' i'
    where xs' = xs ++ new
          st' = (State Nothing):(map toMaybe st)
          f' = (map (\(s0, x, s1) -> ((toMaybe s0), x, (toMaybe s1))) f) ++ newT
          newT = [(s, x, State Nothing) | s<-st', x<-new] ++ 
                 [(State Nothing, x, State Nothing) | x<-xs]
          ac' = map toMaybe ac
          i' = toMaybe i
          toMaybe s = State (Just (runState s))

-- Unifica el alfabeto de dos DFA. Agrega a cada uno los símbolos que el otro 
-- tiene y el no. El conjunto de símbolos de cada uno será la unión de los
-- originales.
unifyAlph :: GDFA -> GDFA -> (GDFA, GDFA)
unifyAlph dfa0@(DA xs0 _ _ _ _) dfa1@(DA xs1 _ _ _ _) =
    let new0 = xs1 \\ xs0
        new1 = xs0 \\ xs1 
        dfa0' = case new0 of 
                     [] -> dfa0
                     _ -> dfaStandar (addSym dfa0 new0)
        dfa1' = case new1 of
                     [] -> dfa1
                     _ -> dfaStandar (addSym dfa1 new1)
    in (dfa0', dfa1')

-- Intersección de dos DFAs. En esta caso aceptamos los estados para los 
-- cuales ambos elementos del par son aceptados en sus respectivos DFA.
-- Asumimos que tienen el mismo alfabeto.
dfaIntersection :: (Eq a, Eq b) => DFA a -> DFA b -> DFA (a,b)
dfaIntersection (DA xs0 st0 (F f0) ac0 i0) (DA xs1 st1 (F f1) ac1 i1) =
    DA xs st (F f) ac i
    where xs = xs0
          st = [State (s0,s1) | (State s0) <- st0, (State s1) <- st1]
          f = [(State (s0,s1), x, State (s0',s1')) | x<-xs, (State (s0,s1))<-st, (State (s0',s1'))<-st, 
                                                     elem (State s0, x, State s0') f0,
                                                     elem (State s1, x, State s1') f1]
          ac = [State (s0,s1) | (State (s0,s1))<-st, elem (State s0) ac0, elem (State s1) ac1]
          i = State (runState i0, runState i1)

-- Unifica los alfabetos, realiza la intersección y estandariza los estados
dfaIntersect :: GDFA -> GDFA -> GDFA 
dfaIntersect dfa0 dfa1 = let (dfa0', dfa1') = unifyAlph dfa0 dfa1
                        in dfaStandar (dfaIntersection dfa0' dfa1') 

-- Unión de dos DFAs. En este caso aceptamos los estados para los cuales
-- al menos uno de sus elementos es aceptado en su DFA original.
-- Asumimos que tienen el mismo alfabeto.
dfaUnion :: (Eq a, Eq b, Ord a, Ord b) => DFA a -> DFA b -> DFA (a,b)
dfaUnion (DA xs0 st0 (F f0) ac0 i0) (DA xs1 st1 (F f1) ac1 i1) = DA xs st (F f) ac i
    where xs = xs0
          st = [State (s0,s1) | (State s0) <- st0, (State s1) <- st1]
          f = [(State (s0,s1), x, State (s0',s1')) | x<-xs, (State (s0,s1))<-st, (State (s0',s1'))<-st, 
                                                     elem (State s0, x, State s0') f0,
                                                     elem (State s1, x, State s1') f1]
          ac = [State (s0,s1) | (State (s0,s1))<-st, (elem (State s0) ac0) || (elem (State s1) ac1)]
          i = State (runState i0, runState i1)

-- Unifica los alfabetos, realiza la unión y estandariza los estados
dfaUnite :: GDFA -> GDFA -> GDFA
dfaUnite dfa0 dfa1 = let (dfa0', dfa1') = unifyAlph dfa0 dfa1
                    in dfaStandar (dfaUnion dfa0' dfa1')

-- Esta función transforma un DFA en un NFA, cambianod sus símbolos 
-- de DSym a NSym para que exista la cadena vacía.
dfaToNFA :: DFA a -> NFA a
dfaToNFA (DA xs st (F f) ac i) = NA xs' st (R r) ac i
    where xs' = map (\(DSym x) -> NSym (NE.toList x)) xs
          r = map (\(s0, (DSym x), s1) -> (s0, (NSym (NE.toList x)), s1)) f

-- Esta función toma un NFA con estados de cualquier tipo 'a' y un entero.
-- Renombra los estados del NFA en todas sus componentes llevándolo a un 
-- NFA idéntico salvo nombres donde su conjunto de estados es un conjunto
-- de enteros consecutivos comenzando por el entero pasado por parámetro.
nfaRename :: Eq a => NFA a -> Int -> NFA Int
nfaRename (NA xs st (R r) ac i) n = NA xs st' (R r') ac' i'
    where st' = let l = n + (length st) - 1
                in map (\x -> State x) (drop n [0 .. l])
          r' = map (\(s0, x, s1) -> (rename s0, x, rename s1)) r
          ac' = map rename ac
          i' = rename i
          rename (State s) = let (Just pos) = elemIndex (State s) st
                             in State (n+pos)

-- Esta función se encarga de la concatenación de dos NFA. Simplemente
-- agrega transiciones vacías de los estados aceptados del primer NFA al
-- estado inicial del segundo NFA. El nuevo conjunto de estados será la 
-- unión de los dos anteriores. Para esto deben ser del mismo tipo y no
-- deben repetirse. Por esto renombramos los estados de ambos NFA a enteros.
nfaConcatenation :: (Eq a, Eq b) => NFA a -> NFA b -> NFA Int
nfaConcatenation nfa0 nfa1 =
    let (NA xs0 st0 (R r0) ac0 i0) = nfaRename nfa0 0
        (NA xs1 st1 (R r1) ac1 i1) = nfaRename nfa1 (length st0)
        transitions = [(s0, NSym "", i1) | s0 <- ac0]
        r = r0 ++ r1 ++ transitions
    in NA (union xs0 xs1) (st0 ++ st1) (R r) ac1 i0

-- Concatenación de dos DFA. Para realizar la concatenación necesitamos
-- la cadena vacía, por lo que no puede hacerse con DFA directamente. 
-- Convertimos ambos DFA a NFA y luego reconvertimos el resultado de la 
-- concatenación a DFA y estandarizamos sus estados.
dfaConcat :: (Eq a, Eq b) => DFA a -> DFA b -> DFA Int
dfaConcat dfa0 dfa1 = let nfa0 = dfaToNFA dfa0
                          nfa1 = dfaToNFA dfa1
                          nConcat = nfaConcatenation nfa0 nfa1
                          dConcat = nfaToDFA nConcat
                      in dfaStandar dConcat


-- Esta función devuelve la reversa de un NFA. Los estados del resultado 
-- serán 'Maybe a' puesto que necesitamos agregar un estado inicial nuevo
-- que sea distinto de todos los demás. De este sacaremos transiciones 
-- vacías a todos los estaos de aceptación originales. Esto se hace ya que
-- el estado inicial es único pero los estaos de aceptación pueden ser varios.
nfaReverse :: NFA a -> NFA (Maybe a)
nfaReverse (NA xs st (R r) ac i) = NA xs st' (R r') ac' i'
    where st' = (State Nothing) : (map (\(State s) -> State (Just s)) st)
          r' = r0 ++ r1
          r0 = (map (\(State s0, x, State s1) -> (State (Just s1), x, State (Just s0))) r) 
          r1 = [(State Nothing, NSym "", State (Just s)) | (State s) <- ac]
          ac' = [State (Just (runState i))]
          i' = State Nothing

-- Reversa de un DFA. Para realizar esta operación se necesitan utilizar 
-- transiciones vacías, por lo que debemos convertir el DFA en NFA y luego
-- volver a convertir el resultado en DFA. Finalmente estandarizamos los estados.
dfaReverse :: Ord a => DFA a -> DFA Int
dfaReverse dfa = let nfa = dfaToNFA dfa
                     nReverse = nfaReverse nfa
                     dReverse = nfaToDFA nReverse
                 in dfaStandar dReverse

-- Unifica los alfabetos y obtiene la diferencia haciendo la intersección del 
-- primero con el complemento del segundo.
dfaDiff :: GDFA -> GDFA -> GDFA 
dfaDiff dfa0 dfa1 = let (dfa0', dfa1') = unifyAlph dfa0 dfa1
                    in dfaStandar (dfaIntersection dfa0' (dfaComplement dfa1'))

----------------------------------------------------------
------------- FUNCIONES EQUIVALENCIA DE DFAs -------------
----------------------------------------------------------

-- Esta función toma un DFA y devuelve True si el lenguaje que este
-- reconoce es vacío. False en caso contrario. Esto sucede si ningún
-- estado de aceptación es alcanzado.
emptyLanguage :: (Eq a, Ord a) => DFA a -> Bool
emptyLanguage dfa@(DA xs st (F f) ac i) = let rs = reachedStates dfa [i]
                                          in rs \\ ac == rs

-- Esta función toma un DFA y un conjunto de estados devuelve el conjunto
-- de todos los estados alcanzados por el autómata partiendo de los estados
-- del conjunto dado.
reachedStates :: (Eq a, Ord a) => DFA a -> [State a] -> [State a]
reachedStates dfa@(DA xs st (F f) ac i) rs = 
    let old = fromList rs
        new = fromList newStates
    in if isSubsetOf new old then rs
                             else reachedStates dfa (toList (S.union old new))
        where newStates = concat (map (\s -> map (\x -> ff s x) xs) rs)
              ff s x = ff' s x f
              ff' s x ((s0,x0,s1):ss) = if s==s0 && x==x0 then s1
                                                          else ff' s x ss 
  

-- Equivalencia de DFAs. Para realizar esto, minimiza ambos DFA
-- y luego checkea que sean isomorfos viendo que las intersecciones
-- del complemento del primero con el segundo y el primero con el 
-- complemento del segundo acepten el lenguaje vacío. 
dfaEq :: (Eq a, Eq b, Ord a, Ord b) => DFA a -> DFA b -> Bool
dfaEq dfa0 dfa1 = let min0 = minimizeDFA dfa0
                      min1 = minimizeDFA dfa1
                      l0 = dfaIntersection (dfaComplement min0) min1
                      l1 = dfaIntersection (dfaComplement min1) min0
                  in (emptyLanguage l0) && (emptyLanguage l1)


--------------- Funciones para ver si un string --------------
------------------- es aceptado por un DFA -------------------

-- Dado un DFA, un String y un estado, toma el estado y el primer caracter del string
-- y pasa al estado al que este le lleva. Cuando ya no hay mas caracteres devuelve
-- True si el estado en el que está parado es de aceptación.
dfaAccept :: Eq a => DFA a -> [NE.NonEmpty Char] -> State a -> Bool
dfaAccept dfa@(DA xs st (F f) ac i) [] s = elem s ac
dfaAccept dfa@(DA xs st (F f) ac i) (c:cs) s = let next = [s' | s'<-st, elem (s, (DSym c), s') f]
                                               in dfaAccept dfa cs (head next) 
    -- next tendrá solo un elemento por la propiedad de determinismo de los DFA


-- Dado un DFA y un String, usa dfaAccept para determinar si el DFA acepta el string.
-- Primero revisamos que todos los caracteres pertenezcan a los símbolos del DFA.
dfaAsk :: Eq a => DFA a -> String -> Bool
dfaAsk dfa@(DA xs st (F f) ac i) str = let str' = map NE.fromList (words str) 
                                       in if all (\s -> elem (DSym s) xs) str'
                                        then dfaAccept dfa str' i
                                        else False