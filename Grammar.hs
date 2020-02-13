module Grammar (
    gramTermToGram,
    gramToNFA,
    nfaToDFA,
    minimizeDFA
    )
    where

import Data.Set (fromList, isSubsetOf, intersection, empty)
import Data.List (union ,(\\))
import Common


gramTermToGram :: GramTerm -> Gram
gramTermToGram (GProd p ps) = case ps of
                                GProd r rs -> let G nts ts prods sigma = gramTermToGram (GProd r rs)
                                              in G (union nts' nts) (union ts' ts) (union prods' prods) (NT "&")
                                GRule l r -> let prods = gramTermToProd ps
                                                 (ts, nts) = tntFromProd prods
                                              in G (union nts' nts) (union ts' ts) (union prods' prods) (NT "&")
        where prods' = gramTermToProd p
              (ts', nts') = tntFromProd prods'
              tntFromProd [] = ([],[])
              tntFromProd (r:rs) = let (t',nt') = tntFromProd rs 
                                    in case r of
                                        PT nt t -> (union [t] t', union [nt] nt')
                                        PN nt0 t nt1 -> (union [t] t', union [nt0,nt1] nt')
                                        PE nt -> (t', union [nt] nt')

              
gramTermToProd :: GramTerm -> [Prod]
gramTermToProd (GRule (GNT l) (GOr r rs)) = case r of
                                              GEmpty -> (PE (NT l)):(gramTermToProd (GRule (GNT l) rs))
                                              Common.GT t -> (PT (NT l) (T t)):(gramTermToProd (GRule (GNT l) rs))
                                              GTNT t nt -> (PN (NT l) (T t) (NT nt)):(gramTermToProd (GRule (GNT l) rs))
gramTermToProd (GRule GSigma (GOr r rs)) = case r of
                                            GEmpty -> (PE (NT "&")):(gramTermToProd (GRule GSigma rs))
                                            Common.GT t -> (PT (NT "&") (T t)):(gramTermToProd (GRule GSigma rs))
                                            GTNT t nt -> (PN (NT "&") (T t) (NT nt)):(gramTermToProd (GRule GSigma rs))
gramTermToProd (GRule (GNT l) r) =  case r of
                                      GEmpty -> [PE (NT l)]
                                      Common.GT t -> [PT (NT l) (T t)]
                                      GTNT t nt -> [PN (NT l) (T t) (NT nt)]
gramTermToProd (GRule GSigma r) = case r of
                                    GEmpty -> [PE (NT "&")]
                                    Common.GT t -> [PT (NT "&") (T t)]
                                    GTNT t nt -> [PN (NT "&") (T t) (NT nt)]

gramToNFA :: Gram -> NFA (Maybe String)
gramToNFA (G nts ts ps nt) = NA syms states r ac i
  where syms = map (\t -> Sym (Just (runT t))) ts
        states = ((State Nothing):(map (\nt -> State (Just (runNT nt))) nts))
        r =  R (union [(State (Just (runNT s)), Sym (Just (runT x)), State (Just (runNT b))) | s<-nts, x<-ts, b<-nts, elem (PN s x b) ps]
                        [(State (Just (runNT s)), Sym (Just (runT x)), State Nothing) | s<-nts, x<-ts, elem (PT s x) ps])
        ac = (State Nothing):[State (Just (runNT s)) | s<-nts, elem (PE s) ps]
        i = (State (Just (runNT nt)))

{-}
included :: [State [Maybe String]] -> [State [Maybe String]] -> Bool
included xs ys = let xs' = map (\x -> fromList (runState x)) xs
                     ys' = map (\y -> fromList (runState y)) ys
                 in isSubsetOf (fromList xs') (fromList ys')
-}

included :: F [Maybe String] -> F [Maybe String] -> Bool
included (F xs) (F ys) = let xs' = map (\(s1, s, s2) -> (fromList (runState s1), s, fromList (runState s2))) xs
                             ys' = map (\(s1, s, s2) -> (fromList (runState s1), s, fromList (runState s2))) ys
                         in isSubsetOf (fromList xs') (fromList ys')

equal :: [State [Maybe String]] -> [State [Maybe String]] -> Bool
equal xs ys = let xs' = map (\x -> fromList (runState x)) xs
                  ys' = map (\y -> fromList (runState y)) ys
              in (fromList xs') == (fromList ys')


dfaF' :: [Sym] -> R (Maybe String) -> [State (Maybe String)] -> F [Maybe String] -> F [Maybe String] -> F [Maybe String]
dfaF' xs (R rs) sts (F fall) (F flast) = let f = concat (map (\(s1, x1, s1') -> map (\x -> (s1', x, t s1' x)) xs) flast)
                                         in if included (F f) (F fall) then (F fall) else (dfaF' xs (R rs) sts (F (union fall f)) (F f))
         where t st x = State (concat (map (\s -> [runState s' | s'<-sts, elem (State s, x, s') rs]) (runState st)))

dfaF :: [Sym] -> R (Maybe String) -> [State (Maybe String)] -> State [Maybe String] -> F [Maybe String]
dfaF xs (R rs) sts i = let f = map (\x -> (i, x, t i x)) xs
                       in dfaF' xs (R rs) sts (F f) (F f)
        where t st x = State (concat (map (\s -> [runState s' | s'<-sts, elem (State s, x, s') rs]) (runState st)))

nfaToDFA :: NFA (Maybe String) -> DFA [Maybe String]
nfaToDFA (NA xs st (R rs) ac i) = DA xs st' (F f) ac' i'
    where i' = State (union [runState i] [runState s | s<-st, elem (i, Sym Nothing, s) rs])
          (F f) = dfaF xs (R rs) st i'
          st' = union (map (\(a,b,c)->a) f) (map (\(a,b,c)->c) f)
          ac' = [s | s<-st', (intersection (fromList (runState s)) (fromList (map runState ac))) /= empty]


nextPartition :: [State [Maybe String]] -> [[State [Maybe String]]] -> [Sym] -> F [Maybe String] -> [[State [Maybe String]]]
nextPartition sts last xs (F f) = concat (map (partition []) last)
    where partition new [] = new
          partition [] (s:ss) = partition [[s]] ss
          partition (p:ps) (s:ss) = if distinguishable (head p) s then partition (p:(partition ps [s])) ss
                                                                  else partition ((s:p):ps) ss
          distinguishable s1 s2 = or (map (\x -> (numOfPart [s | s<-sts, elem (s1,x,s) f] last 0) /= (numOfPart [s | s<-sts, elem (s2,x,s) f] last 0)) xs)
          numOfPart [s] [] i = -1
          numOfPart [s] (p:ps) i = if (elem s p) then i 
                                                 else numOfPart [s] ps (i+1)

minimumStates :: [Sym] -> [State [Maybe String]] -> [[State [Maybe String]]] -> F [Maybe String] -> [State [[Maybe String]]]
minimumStates xs sts p (F f) = let nextp = nextPartition sts p xs (F f)
                            in if p == nextp then (map (\ss -> State ((map runState ss))) p)
                                             else minimumStates xs sts nextp (F f)

minimizeDFA :: DFA [Maybe String] -> DFA [[Maybe String]]
minimizeDFA (DA xs st (F f) ac i) = DA xs st' (F f') ac' i'
    where st' = minimumStates xs st [ac, st \\ ac] (F f)
          f' = [(s0, x, s1) | s0<-st', x<-xs, s1<-st', connect s0 x s1]
          connect s0 x s1 = or (concat (map (\ss0-> (map (\ss1-> elem (State ss0, x, State ss1) f) (runState s1))) (runState s0)))
          ac' = [s | s<-st', [ss | ss<-ac, elem (runState ss) (runState s)] /= []]
          i' = let [s] = [s | s<-st', elem (runState i) (runState s)] in s

          
complementDFA :: Eq a => DFA a -> DFA a
complementDFA (DA xs st (F f) ac i) = DA xs st (F f) (st \\ ac) i
          

--dfaIntersection :: DFA a -> DFA b -> DFA (a,b)
--dfaIntersection (DA xs0 st0 (F f0) ac0 i0) (DA xs1 st1 (F f1) ac1 i1) =

{-}
nfaToDFA :: NFA (Maybe String) -> DFA [Maybe String]
nfaToDFA (NA xs st (R rs) ac i) = DA xs st' f ac' i'
    where i' = State (union [runState i] [runState s | s<-st, elem (i, Sym Nothing, s) rs])
          st' = dfaStates xs rs [i']
          dfaStates sx sr sst = let sst' = union sst (concat (map (\s -> map (\x -> t s x) sx) sst))
                                 in if (equal sst sst') then sst else dfaStates sx sr sst'
          t sts x = concat (map (\s -> State [s' | s'<-st', elem (State s, x, State s') rs]) (runState sts))
          f = F [(s, x, t s x) | s<-st', x<-xs]
          ac' = [s | s<-st', (intersection (fromList (runState s)) (fromList (runState ac))) /= empty]
-}
