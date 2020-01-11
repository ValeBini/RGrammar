import Data.Set (fromList, isSubsetOf, intersection, empty)
import Data.List (union)

newtype T = T {runT :: String}
    deriving (Eq, Ord, Show)

newtype NT = NT {runNT :: String}
    deriving (Eq, Ord, Show)

data Prod = PT NT T | PN NT T NT | PE NT
    deriving (Eq, Ord, Show)

data Gram = G [NT] [T] [Prod] NT
    deriving Show

newtype Sym = Sym {runSym :: Maybe String} -- el simbolo Nothing es la palabra vacia
    deriving (Eq, Ord, Show)

newtype State a = State {runState :: a}
    deriving (Eq, Ord, Show)

data R a = R [(State a, Sym, State a)]
    deriving Show

data F a = F [(State a, Sym, State a)]
    deriving Show

data DFA a = DA [Sym] [State a] (F a) [State a] (State a)

data NFA a = NA [Sym] [State a] (R a) [State a] (State a)
    deriving Show

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


dfaF :: [Sym] -> R (Maybe String) -> [State (Maybe String)] -> F [Maybe String] -> F [Maybe String] -> F [Maybe String]
dfaF xs (R rs) sts (F fall) (F flast) = let f = concat (map (\(s1, x1, s1') -> map (\x -> (s1', x, t s1' x)) xs) flast)
                                        in if included (F f) (F fall) then (F fall) else (dfaF xs (R rs) sts (F (union fall f)) (F f))
        where t st x = State (concat (map (\s -> [runState s' | s'<-sts, elem (State s, x, s') rs]) (runState st)))

nfaToDFA :: NFA (Maybe String) -> DFA [Maybe String]
nfaToDFA (NA xs st rs ac i) = DA xs st' f ac' i'
    where i' = State (union [runState i] [runState s | s<-st, elem (i, Sym Nothing, s) rs])
          f = dfaF xs rs st (F [])
          st' = dfaStates xs rs [i']
          dfaStates sx sr sst = let sst' = union sst (concat (map (\s -> map (\x -> t s x) sx) sst))
                                 in if (equal sst sst') then sst else dfaStates sx sr sst'
          t sts x = concat (map (\s -> State [s' | s'<-st', elem (State s, x, State s') rs]) (runState sts))
          ac' = [s | s<-st', (intersection (fromList (runState s)) (fromList (runState ac))) /= empty]

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
