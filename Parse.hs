{-# OPTIONS_GHC -w #-}
module Parse where
import Common
import Data.Char
import Prelude hiding (LT)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,101) ([0,256,0,16384,0,0,576,1,32768,0,0,0,0,57344,3,0,0,0,16528,0,16384,0,0,1033,0,0,0,33024,0,0,0,0,2064,0,512,0,0,2,0,512,0,0,0,0,0,0,128,0,32768,0,0,128,0,0,0,0,0,0,0,0,0,1033,0,61568,0,36864,64,0,4132,0,2304,4,0,0,0,16528,0,0,4,0,768,0,57344,0,0,60,0,0,0,49152,3,0,0,0,129,0,4104,0,0,0,0,516,0,49440,0,4096,0,0,0,0,0,0,0,64,0,0,0,0,4,0,0,0,64,0,0,0,0,0,0,1024,2,0,0,0,0,0,0,0,32768,256,0,0,0,0,0,0,3090,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse_lGram","%start_parse_rGram","%start_parse_Stmt","RLeftSide","LLeftSide","RRight","LRight","RRightSide","LRightSide","RRule","LRule","RProd","LProd","RGram","LGram","Grammar","Stmt","'\\\\'","'|'","'->'","'&'","';'","'{'","'}'","'('","')'","T","NT","'='","'=='","'+'","'!'","'~'","'.'","'-'","%eof"]
        bit_start = st * 38
        bit_end = (st + 1) * 38
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..37]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (25) = happyShift action_13
action_0 (17) = happyGoto action_12
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (25) = happyShift action_11
action_1 (16) = happyGoto action_10
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (27) = happyShift action_7
action_2 (30) = happyShift action_8
action_2 (37) = happyShift action_9
action_2 (18) = happyGoto action_5
action_2 (19) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (30) = happyShift action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (32) = happyShift action_27
action_5 (33) = happyShift action_28
action_5 (34) = happyShift action_29
action_5 (35) = happyShift action_30
action_5 (36) = happyShift action_31
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (38) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (27) = happyShift action_7
action_7 (30) = happyShift action_24
action_7 (37) = happyShift action_9
action_7 (18) = happyGoto action_26
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (31) = happyShift action_25
action_8 _ = happyReduce_27

action_9 (27) = happyShift action_7
action_9 (30) = happyShift action_24
action_9 (37) = happyShift action_9
action_9 (18) = happyGoto action_23
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (38) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (23) = happyShift action_22
action_11 (30) = happyShift action_4
action_11 (6) = happyGoto action_19
action_11 (12) = happyGoto action_20
action_11 (14) = happyGoto action_21
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (38) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (23) = happyShift action_17
action_13 (30) = happyShift action_18
action_13 (7) = happyGoto action_14
action_13 (13) = happyGoto action_15
action_13 (15) = happyGoto action_16
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (22) = happyShift action_43
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (24) = happyShift action_42
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (26) = happyShift action_41
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_6

action_18 _ = happyReduce_5

action_19 (22) = happyShift action_40
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (24) = happyShift action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (26) = happyShift action_38
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_4

action_23 _ = happyReduce_32

action_24 _ = happyReduce_27

action_25 (27) = happyShift action_7
action_25 (30) = happyShift action_24
action_25 (37) = happyShift action_9
action_25 (18) = happyGoto action_37
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (28) = happyShift action_36
action_26 (33) = happyShift action_28
action_26 (34) = happyShift action_29
action_26 (35) = happyShift action_30
action_26 (36) = happyShift action_31
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (27) = happyShift action_7
action_27 (30) = happyShift action_24
action_27 (37) = happyShift action_9
action_27 (18) = happyGoto action_35
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (27) = happyShift action_7
action_28 (30) = happyShift action_24
action_28 (37) = happyShift action_9
action_28 (18) = happyGoto action_34
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (27) = happyShift action_7
action_29 (30) = happyShift action_24
action_29 (37) = happyShift action_9
action_29 (18) = happyGoto action_33
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_30

action_31 (27) = happyShift action_7
action_31 (30) = happyShift action_24
action_31 (37) = happyShift action_9
action_31 (18) = happyGoto action_32
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (35) = happyShift action_30
action_32 _ = happyReduce_31

action_33 (35) = happyShift action_30
action_33 (36) = happyShift action_31
action_33 _ = happyReduce_29

action_34 (34) = happyShift action_29
action_34 (35) = happyShift action_30
action_34 (36) = happyShift action_31
action_34 _ = happyReduce_28

action_35 (33) = happyShift action_28
action_35 (34) = happyShift action_29
action_35 (35) = happyShift action_30
action_35 (36) = happyShift action_31
action_35 _ = happyReduce_35

action_36 _ = happyReduce_33

action_37 (33) = happyShift action_28
action_37 (34) = happyShift action_29
action_37 (35) = happyShift action_30
action_37 (36) = happyShift action_31
action_37 _ = happyReduce_34

action_38 _ = happyReduce_25

action_39 (23) = happyShift action_22
action_39 (30) = happyShift action_4
action_39 (6) = happyGoto action_19
action_39 (12) = happyGoto action_20
action_39 (14) = happyGoto action_55
action_39 _ = happyReduce_22

action_40 (20) = happyShift action_53
action_40 (29) = happyShift action_54
action_40 (8) = happyGoto action_51
action_40 (10) = happyGoto action_52
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_26

action_42 (23) = happyShift action_17
action_42 (30) = happyShift action_18
action_42 (7) = happyGoto action_14
action_42 (13) = happyGoto action_15
action_42 (15) = happyGoto action_50
action_42 _ = happyReduce_24

action_43 (20) = happyShift action_46
action_43 (23) = happyShift action_47
action_43 (29) = happyShift action_48
action_43 (30) = happyShift action_49
action_43 (9) = happyGoto action_44
action_43 (11) = happyGoto action_45
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (21) = happyShift action_61
action_44 _ = happyReduce_18

action_45 _ = happyReduce_20

action_46 _ = happyReduce_14

action_47 (29) = happyShift action_60
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_11

action_49 (29) = happyShift action_59
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_23

action_51 (21) = happyShift action_58
action_51 _ = happyReduce_16

action_52 _ = happyReduce_19

action_53 _ = happyReduce_10

action_54 (23) = happyShift action_56
action_54 (30) = happyShift action_57
action_54 _ = happyReduce_7

action_55 _ = happyReduce_21

action_56 _ = happyReduce_9

action_57 _ = happyReduce_8

action_58 (20) = happyShift action_53
action_58 (29) = happyShift action_54
action_58 (8) = happyGoto action_51
action_58 (10) = happyGoto action_63
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_12

action_60 _ = happyReduce_13

action_61 (20) = happyShift action_46
action_61 (23) = happyShift action_47
action_61 (29) = happyShift action_48
action_61 (30) = happyShift action_49
action_61 (9) = happyGoto action_44
action_61 (11) = happyGoto action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_17

action_63 _ = happyReduce_15

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TNT happy_var_1))
	 =  HappyAbsSyn6
		 (RNT happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn6
		 (RSigma
	)

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyTerminal (TNT happy_var_1))
	 =  HappyAbsSyn7
		 (LNT happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (LSigma
	)

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyTerminal (TT happy_var_1))
	 =  HappyAbsSyn8
		 (RT happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyTerminal (TNT happy_var_2))
	(HappyTerminal (TT happy_var_1))
	 =  HappyAbsSyn8
		 (RTNT happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	(HappyTerminal (TT happy_var_1))
	 =  HappyAbsSyn8
		 (RTSigma happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn8
		 (REmpty
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal (TT happy_var_1))
	 =  HappyAbsSyn9
		 (LT happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  9 happyReduction_12
happyReduction_12 (HappyTerminal (TT happy_var_2))
	(HappyTerminal (TNT happy_var_1))
	 =  HappyAbsSyn9
		 (LNTT happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  9 happyReduction_13
happyReduction_13 (HappyTerminal (TT happy_var_2))
	_
	 =  HappyAbsSyn9
		 (LSigmaT happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn9
		 (LEmpty
	)

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 (ROr happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (LOr happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 (RRule happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn13
		 (LRule happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  14 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn14
		 (RProd happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  14 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (LProd happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  15 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  17 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  18 happyReduction_27
happyReduction_27 (HappyTerminal (TNT happy_var_1))
	 =  HappyAbsSyn18
		 (SGram happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  18 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (SUnion happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  18 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (SInter happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  18 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (SRever happy_var_1
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  18 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (SConcat happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  18 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (SComp happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  18 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_3)
	_
	(HappyTerminal (TNT happy_var_1))
	 =  HappyAbsSyn19
		 (SDef happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  19 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 (SEq happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 38 38 tk (HappyState action) sts stk;
	TEmpty -> cont 20;
	TOr -> cont 21;
	TArrow -> cont 22;
	TSigma -> cont 23;
	TEnd -> cont 24;
	TOpen -> cont 25;
	TClose -> cont 26;
	TO -> cont 27;
	TC -> cont 28;
	TT happy_dollar_dollar -> cont 29;
	TNT happy_dollar_dollar -> cont 30;
	TDef -> cont 31;
	TEq -> cont 32;
	TUnion -> cont 33;
	TIntersec -> cont 34;
	TReverse -> cont 35;
	TConcat -> cont 36;
	TComplem -> cont 37;
	_ -> happyError' (tk, [])
	})

happyError_ explist 38 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
parse_lGram = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

parse_rGram = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

parse_Stmt = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
