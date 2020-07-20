module Grammar (
    gramTermToGram,
    gramToNFA,
    nfaToDFA,
    minimizeDFA
    )
    where

import Data.Set (fromList, isSubsetOf, intersection, empty)
import Data.List (union ,(\\), delete, elemIndex)
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
  where syms = map (\t -> NSym (Just (runT t))) ts
        states = ((State Nothing):(map (\nt -> State (Just (runNT nt))) nts))
        r =  R (union [(State (Just (runNT s)), NSym (Just (runT x)), State (Just (runNT b))) | s<-nts, x<-ts, b<-nts, elem (PN s x b) ps]
                        [(State (Just (runNT s)), NSym (Just (runT x)), State Nothing) | s<-nts, x<-ts, elem (PT s x) ps])
        ac = (State Nothing):[State (Just (runNT s)) | s<-nts, elem (PE s) ps]
        i = (State (Just (runNT nt)))

{-}
included :: [State [Maybe String]] -> [State [Maybe String]] -> Bool
included xs ys = let xs' = map (\x -> fromList (runState x)) xs
                     ys' = map (\y -> fromList (runState y)) ys
                 in isSubsetOf (fromList xs') (fromList ys')
-}

included :: R [Maybe String] -> R [Maybe String] -> Bool
included (R xs) (R ys) = let xs' = map (\(s1, s, s2) -> (fromList (runState s1), s, fromList (runState s2))) xs
                             ys' = map (\(s1, s, s2) -> (fromList (runState s1), s, fromList (runState s2))) ys
                         in isSubsetOf (fromList xs') (fromList ys')

equal :: [State [Maybe String]] -> [State [Maybe String]] -> Bool
equal xs ys = let xs' = map (\x -> fromList (runState x)) xs
                  ys' = map (\y -> fromList (runState y)) ys
              in (fromList xs') == (fromList ys')


dfaF' :: [NSym] -> R (Maybe String) -> [State (Maybe String)] -> R [Maybe String] -> R [Maybe String] -> R [Maybe String]
dfaF' xs (R rs) sts (R fall) (R flast) = let f = concat (map (\(s1, x1, s1') -> map (\x -> (s1', x, t s1' x)) xs) flast)
                                         in if included (R f) (R fall) then (R fall) else (dfaF' xs (R rs) sts (R (union fall f)) (R f))
         where t st x = State (concat (map (\s -> [runState s' | s'<-sts, elem (State s, x, s') rs]) (runState st)))

dfaF :: [NSym] -> R (Maybe String) -> [State (Maybe String)] -> State [Maybe String] -> R [Maybe String]
dfaF xs (R rs) sts i = let f = map (\x -> (i, x, t i x)) xs
                       in dfaF' xs (R rs) sts (R f) (R f)
        where t st x = State (concat (map (\s -> [runState s' | s'<-sts, elem (State s, x, s') rs]) (runState st)))

nfaToDFA :: NFA (Maybe String) -> DFA [Maybe String]
nfaToDFA (NA xs st (R rs) ac i) = DA xs' st' (F f) ac' i'
    where xs' = concat (map (\(NSym x) -> case x of
                                            Just s -> [DSym s]
                                            Nothing -> []) xs)
          i' = State (union [runState i] [runState s | s<-st, elem (i, NSym Nothing, s) rs])
          f = map (\(s0,NSym (Just x),s1) -> (s0,DSym x,s1)) f'
          R f' = (dfaF xs (R rs) st i')
          st' = union (map (\(a,b,c)->a) f) (map (\(a,b,c)->c) f)
          ac' = [s | s<-st', (intersection (fromList (runState s)) (fromList (map runState ac))) /= empty]


nextPartition :: [State [Maybe String]] -> [[State [Maybe String]]] -> [DSym] -> F [Maybe String] -> [[State [Maybe String]]]
nextPartition sts last xs (F f) = concat (map (partition []) last)
    where partition new [] = new
          partition [] (s:ss) = partition [[s]] ss
          partition (p:ps) (s:ss) = if distinguishable (head p) s then partition (p:(partition ps [s])) ss
                                                                  else partition ((s:p):ps) ss
          distinguishable s1 s2 = or (map (\x -> (numOfPart [s | s<-sts, elem (s1,x,s) f] last 0) /= (numOfPart [s | s<-sts, elem (s2,x,s) f] last 0)) xs)
          numOfPart [s] [] i = -1
          numOfPart [s] (p:ps) i = if (elem s p) then i 
                                                 else numOfPart [s] ps (i+1)

minimumStates :: [DSym] -> [State [Maybe String]] -> [[State [Maybe String]]] -> F [Maybe String] -> [State [[Maybe String]]]
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
          
--asumo que tienen el mismo alfabeto
dfaIntersection :: (Eq a, Eq b) => DFA a -> DFA b -> DFA (a,b)
dfaIntersection (DA xs0 st0 (F f0) ac0 i0) (DA xs1 st1 (F f1) ac1 i1) =
    DA xs st (F f) ac i
    where xs = xs0
          st = union (union [s | s <- map first f] [s | s <- map third f]) [i]
          f = [(State (s0,s1), x, State (s0',s1')) | x<-xs, (State s0)<-st0, (State s0')<-st0,
                                                     (State s1)<-st1, (State s1')<-st1, 
                                                     elem (State s0, x, State s0') f0,
                                                     elem (State s1, x, State s1') f1]
          ac = [State (s0,s1) | (State (s0,s1))<-st, elem (State s0) ac0, elem (State s1) ac1]
          i = State (runState i0, runState i1)
          first (a,b,c) = a
          third (a,b,c) = c

dfaUnion :: (Eq a, Eq b) => DFA a -> DFA b -> DFA (a,b)
dfaUnion (DA xs0 st0 (F f0) ac0 i0) (DA xs1 st1 (F f1) ac1 i1) = DA xs st (F f) ac i
    where xs = xs0
          st = union (union [s | s <- map first f] [s | s <- map third f]) [i]
          f = [(State (s0,s1), x, State (s0',s1')) | x<-xs, (State s0)<-st0, (State s0')<-st0,
                                                     (State s1)<-st1, (State s1')<-st1, 
                                                     elem (State s0, x, State s0') f0,
                                                     elem (State s1, x, State s1') f1]
          ac = [State (s0,s1) | (State (s0,s1))<-st, (elem (State s0) ac0) || (elem (State s1) ac1)]
          i = State (runState i0, runState i1)
          first (a,b,c) = a
          third (a,b,c) = c


nfaRename :: Eq a => NFA a -> Int -> NFA Int
nfaRename (NA xs st (R r) ac i) n = NA xs st' (R r') ac' i'
    where st' = let l = n + (length st) - 1
                in map (\x -> State x) (drop n [0 .. l])
          r' = map (\(s0, x, s1) -> (rename s0, x, rename s1)) r
          ac' = map rename ac
          i' = rename i
          rename (State s) = let (Just pos) = elemIndex (State s) st
                             in State (n+pos)

nfaConcatenation :: (Eq a, Eq b) => NFA a -> NFA b -> NFA Int
nfaConcatenation nfa0 nfa1 =
    let (NA xs0 st0 (R r0) ac0 i0) = nfaRename nfa0 0
        (NA xs1 st1 (R r1) ac1 i1) = nfaRename nfa1 (length st0)
        transitions = [(s0, NSym Nothing, i1) | s0 <- ac0]
        r = r0 ++ r1 ++ transitions
    in NA xs0 (st0 ++ st1) (R r) ac1 i0

nfaReverse :: NFA a -> NFA (Maybe a)
nfaReverse (NA xs st (R r) ac i) = NA xs st' (R r') ac' i'
    where st' = (State Nothing) : (map (\(State s) -> State (Just s)) st)
          r' = r0 ++ r1
          r0 = (map (\(State s0, x, State s1) -> (State (Just s1), x, State (Just s0))) r) 
          r1 = [(State Nothing, NSym Nothing, State (Just s)) | (State s) <- ac]
          ac' = [State (Just (runState i))]
          i' = State Nothing

emptyLanguage :: (Eq a, Ord a) => DFA a -> Bool
emptyLanguage dfa@(DA xs st (F f) ac i) = let rs = reachedStates dfa [i]
                                          in rs \\ ac == rs

reachedStates :: (Eq a, Ord a) => DFA a -> [State a] -> [State a]
reachedStates dfa@(DA xs st (F f) ac i) rs = let old = fromList rs
                                                 new = fromList newStates
                                             in if isSubsetOf new old then rs
                                                                      else reachedStates dfa (union rs newStates)
    where newStates = concat (map (\s -> map (\x -> ff s x) xs) rs)
          ff s x = ff' s x f
          ff' s x ((s0,x0,s1):ss) = if s==s0 && x==x0 then s1
                                                      else ff' s x ss 


eqGrammar :: Gram -> Gram -> Bool
eqGrammar g0@(G nt0 t0 p0 i0) g1@(G nt1 t1 p1 i1) = if t0 == t1 then checkEq 
                                                                else False
    where checkEq = let dfa0 = (minimizeDFA (nfaToDFA (gramToNFA g0)))
                        dfa1 = (minimizeDFA (nfaToDFA (gramToNFA g1)))
                        l0 = dfaIntersection (complementDFA dfa0) dfa1
                        l1 = dfaIntersection (complementDFA dfa1) dfa0
                        in (emptyLanguage l0) && (emptyLanguage l1)

ntsList :: [NT]
ntsList = map (\s -> NT [s]) ('&' : ['A' .. 'Z'])

dfaToGram :: Eq a => DFA a -> Gram
dfaToGram dfa@(DA xs st (F f) ac i) = G nts ts ps nt
    where nts = let l = length st
                in take l ntsList
          ts = map (\x -> T (runDSym x)) xs
          ps = (map fToProd f) ++ finalprods
          nt = NT "&"
          fToProd (s0,DSym x,s1) = let nt0 = findNT s0
                                       nt1 = findNT s1
                                    in PN nt0 (T x) nt1
          finalprods = map (\s -> PE (findNT s)) ac
          findNT s = let Just i = elemIndex s st'
                     in nts !! i
          st' = i:(delete i st)
          

