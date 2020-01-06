import Data.List as L
import Data.Set as S

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
  where syms = L.map (\t -> Sym (Just (runT t))) ts
        states = ((State Nothing):(L.map (\nt -> State (Just (runNT nt))) nts))
        r =  R (L.union [(State (Just (runNT s)), Sym (Just (runT x)), State (Just (runNT b))) | s<-nts, x<-ts, b<-nts, elem (PN s x b) ps]
                        [(State (Just (runNT s)), Sym (Just (runT x)), State Nothing) | s<-nts, x<-ts, elem (PT s x) ps])
        ac = (State Nothing):[State (Just (runNT s)) | s<-nts, elem (PE s) ps]
        i = (State (Just (runNT nt)))


included :: [State [Maybe String]] -> [State [Maybe String]] -> Bool
included xs ys = let xs' = L.map (\x -> fromList (runState x)) xs
                     ys' = L.map (\y -> fromList (runState y)) ys
                 in isSubsetOf (fromList xs') (fromList ys')


dfaF :: [Sym] -> R -> [State [Maybe String]] -> (F, [States [Maybe String]])
dfaF sx sr (F fall) (F flast) = let f = (concat (L.map (\(s1, x1, s1') -> L.map (\x -> (s1', x, t s1' x)) sx) flast))
                    in if included f fall then (F fall) else F (dfaF sx sr (F (L.union fall f)) (F f))




nfaToDFA :: NFA (Maybe String) -> DFA [Maybe String]
nfaToDFA (NA xs st (R rs) ac i) = DA xs st' f ac' i'
    where i' = State (L.union [runState i] [runState s | s<-st, elem (i, Sym Nothing, s) rs])
          st' = dfaStates xs rs [i']
          dfaStates sx sr sst = let sst' = L.union sst (concat (L.map (\s -> L.map (\x -> t s x) sx) sst))
                                 in if (equal sst sst') then sst else dfaStates sx sr sst'
          t sts x = concat (L.map (\s -> State [s' | s'<-st', elem (State s, x, State s') rs]) (runState sts))
          f = F [(s, x, t s x) | s<-st', x<-xs]
          ac' = [s | s<-st', (S.intersection (fromList (runState s)) (fromList (runState ac))) /= S.empty]
