{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, QuasiQuotes #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Test.QuickCheck.Checkers
import Data.List
--import Data.Generics.Uniplate.Data
import Control.Applicative ((<$>))    
import Control.Monad.Error
import Language.PiEtaEpsilon hiding (term, Term, Iso, Value)
import qualified Language.PiEtaEpsilon as P
import Language.PiEtaEpsilon.Pretty.Debug
import Debug.Trace
import Language.PiEtaEpsilon.BNFMeta.Term
import Language.PiEtaEpsilon.BNFMeta.Value
import qualified Language.LBNF.Grammar as G
import Language.Haskell.TH.Quote
import Control.Applicative
import Control.Unification
import Control.Unification.IntVar
import Control.Monad.Logic
import Debug.Trace.Helpers

--main = quickCheck $ roundTrip ppr   (fromRight . parseType)

main = defaultMain tests

mkParserTest f input expected = case f input of
        Prelude.Right x -> x @?= expected
        Prelude.Left  x -> assertFailure $ show x 

testParseType = mkParserTest parseType

fromRight :: (Show a) => Either a b -> b
fromRight (Prelude.Right x) = x
fromRight (Prelude.Left x)         = error $ "okay here's what's wrong " ++ show x

roundTrip f g x = if (g $ f x) == x 
                    then True
                    else trace (show x) False

roundTripBack f g x = result where
        y = f x
        result = if (f $ g y) == y
                    then True
                    else trace (show x) False 
  

sp' :: (x -> b -> c) -> (y -> z -> b) -> x -> y -> z ->  c
sp' f g x y z = f x $ g y z 
     
sp f g (x, y, z) = sp' f g x y z

choose' :: (Int, Int) -> Gen Int
choose' = choose
--------------arbs
instance Arbitrary BaseIso where
    arbitrary = do
        i <- choose'(0, 9)
        case i of
            0 -> return BIdentityS       
            1 -> return BIdentityP        
            2 -> return BCommutativeS
            3 -> return BCommutativeP     
            4 -> return BAssociativeS     
            5 -> return BAssociativeP     
            6 -> return BSplitS           
            7 -> return BSplitP           
            8 -> return BDistributiveZero
            9 -> return BDistributivePlus 
                
instance Arbitrary Iso where
    arbitrary = do
        i <- choose'(0, 1)
        case i of
            0 -> IEliminate <$> arbitrary
            1 -> IIntroduce <$> arbitrary

instance Arbitrary Term where
    arbitrary = sized arb where
        arb depth = do
            i <- choose'(if depth < 1 then 0 else 4, 4)
            case i of
                0 -> TCompose      <$> arb (depth `div` 2) <*> arb (depth `div` 2)
                1 -> TPlus         <$> arb (depth `div` 2) <*> arb (depth `div` 2)
                2 -> TTimes        <$> arb (depth `div` 2) <*> arb (depth `div` 2)
                3 -> TBase         <$> arbitrary
                4 -> (TId . Ident) <$> arbitrary


---------------------------

instance Arbitrary Value where
    arbitrary = sized arb where
        arb depth = do
            i <- choose'(if depth < 1 then 0 else 5, 5)
            case i of
                0 -> VTuple       <$> arb (depth `div` 2) <*> arb (depth `div` 2)
                1 -> VLeft        <$> arb (depth - 1)
                2 -> VRight       <$> arb (depth - 1) 
                3 -> VNegate      <$> arb (depth - 1) 
                4 -> VReciprocate <$> arb (depth - 1) 
                5 -> return VUnit        




--TODO find a better home :(
parseTTerm :: String -> Term
parseTTerm  x = error "I can only using the quasiquoter for parsing :("  -- $((quoteExp G.grammar)  x)

pprTTerm :: Term -> String
pprTTerm       = error "I haven't bothered to do to write pprTTerm but is should be easy"

parseBaseIso :: String -> BaseIso
parseBaseIso x = error "I can only using the quasiquoter for parsing :(" -- $([| (quoteExp G.grammar) x |])

pprBaseIso :: BaseIso -> String
pprBaseIso     = error "I haven't bothered to do to write pprBaseIso but is should be easy"

parseVValue :: String -> Value
parseVValue x  = error "I can only using the quasiquoter for parsing :(" -- $([| (quoteExp G.grammar) x |])

pprVValue :: Value -> String
pprVValue      = error "I haven't bothered to do to write pprVValue but is should be easy"

parseIso :: String -> Iso
parseIso x     = error "I can only using the quasiquoter for parsing :(" -- $([| (quoteExp G.grammar) x |])

pprIso :: Iso -> String
pprIso         = error "I haven't bothered to do to write pprIso but is should be easy"

--pprParserInvPropL :: String -> (a -> String) -> (String -> a) -> Test.QuickCheck.Test
pprParserInvPropL name ppr parser = testProperty ("a ppr "++ name ++" is a parsed " ++ name) (roundTrip ppr parser)


--pprParserInvPropR :: String -> (a -> String) -> (String -> a) -> Test.QuickCheck.Test
pprParserInvPropR name ppr parser = testProperty ("a ppr "++ name ++" is a parsed " ++ name) (roundTripBack ppr parser)

                             
tests = [
            testGroup "Type Parser" $ concat [
                    map (sp testCase testParseType) [
                        ("test_pZero"       , "0",     Zero),
                        ("test_pOne"        , "1",     One),
                        ("test_pSum"        , "1 + 0", Sum        One Zero),
                        ("test_pProduct"    , "1 * 0", Product    One Zero),
                        ("test_pNegative"   , "- 1"  , Negative   One),
                        ("test_pReciprocal" , "/ 1"  , Reciprocal One)
                    ], 
                    --properities
                    [
                        testProperty "a ppr type is a parsed type"   $ roundTrip     ppr (fromRight . parseType), 
                        testProperty "a parsed string is a ppr type" $ roundTripBack ppr (fromRight . parseType)
                    ]
                ],
                    
            testGroup "Type QuasiQuoter" [
                testCase "test_expression_0" $ [typ| (1 + 0) * (1 * 1)|] @?= Product (Sum One Zero) (Product One One) 
            ],

--
--  Term Tests -------------------------------------------------------------------
-- 
            
            testGroup "Term BNFC" [
-- | IsoBase Tests            
--------------------------------------------------------------------------------
                testCase "test_parseTermIsobase_0" test_parseTermIsobase_0,
                testCase "test_parseTermIsobase_1" test_parseTermIsobase_1,
                testCase "test_parseTermIsobase_2" test_parseTermIsobase_2,
                testCase "test_parseTermIsobase_3" test_parseTermIsobase_3,
                testCase "test_parseTermIsobase_4" test_parseTermIsobase_4,
                testCase "test_parseTermIsobase_5" test_parseTermIsobase_5,
                testCase "test_parseTermIsobase_6" test_parseTermIsobase_6,
                testCase "test_parseTermIsobase_7" test_parseTermIsobase_7,
                testCase "test_parseTermIsobase_8" test_parseTermIsobase_8,
                testCase "test_parseTermIsobase_9" test_parseTermIsobase_9,
                --pprParserInvPropL "BaseIso" pprBaseIso parseBaseIso, 
                --pprParserInvPropR "BaseIso" pprBaseIso parseBaseIso,
-- | Iso Tests                
--------------------------------------------------------------------------------
                testCase "test_parseTermIsobase_10" test_parseTermIso_10,
                testCase "test_parseTermIsobase_11" test_parseTermIso_11,
                --pprParserInvPropL "Iso" pprIso parseIso, 
                --pprParserInvPropR "Iso" pprIso parseIso,
-- | Term Tests                
--------------------------------------------------------------------------------
                testCase "test_parseTermIsobase_12" test_parseTermTerm_12,
                testCase "test_parseTermIsobase_13" test_parseTermTerm_13,
                testCase "test_parseTermIsobase_14" test_parseTermTerm_14,
                testCase "test_parseTermIsobase_15" test_parseTermTerm_15,
                testCase "test_parseTermIsobase_16" test_parseTermTerm_16
                --pprParserInvPropL "Term" pprTTerm parseTTerm
                --pprParserInvPropR "Term" pprTTerm parseTTerm -- this test diverges for some reason
            ],
            
--            
-- | Value Tests -----------------------------------------------------------------
--            
            testGroup "Value BNFC" [
                testCase "test_parseValue_0" test_parseValue_0,
                testCase "test_parseValue_1" test_parseValue_1,
                testCase "test_parseValue_2" test_parseValue_2,
                testCase "test_parseValue_3" test_parseValue_3,
                testCase "test_parseValue_4" test_parseValue_4,
                testCase "test_parseValue_5" test_parseValue_5
                --pprParserInvPropL "Value" pprVValue parseVValue, 
                --pprParserInvPropR "Value" pprVValue parseVValue
            ],
            testGroup "machine tests " [
                testCase "test_evalEliminateIdentityS" test_evalEliminateIdentityS
            ]
            
        ]
        
-- | IsoBase Tests            
--------------------------------------------------------------------------------                                                                
test_parseTermIsobase_0 = BIdentityS         @?= [baseIso| <=+=>             |]    
test_parseTermIsobase_1 = BIdentityP         @?= [baseIso| <=*=>             |]
test_parseTermIsobase_2 = BCommutativeS      @?= [baseIso| commutativeS      |]
test_parseTermIsobase_3 = BCommutativeP      @?= [baseIso| commutativeP      |]
test_parseTermIsobase_4 = BAssociativeS      @?= [baseIso| |+|+|             |]
test_parseTermIsobase_5 = BAssociativeP      @?= [baseIso| |*|*|             |]
test_parseTermIsobase_6 = BSplitS            @?= [baseIso| -+<               |]
test_parseTermIsobase_7 = BSplitP            @?= [baseIso| -*<               |]
test_parseTermIsobase_8 = BDistributiveZero  @?= [baseIso| distributiveZero  |]
test_parseTermIsobase_9 = BDistributivePlus  @?= [baseIso| distributivePlus  |]

-- | Iso Tests                
--------------------------------------------------------------------------------
test_parseTermIso_10 = IEliminate BAssociativeS @?= [iso| #  |+|+| |] 
test_parseTermIso_11 = IIntroduce BAssociativeP @?= [iso| '  |*|*| |] 

-- | Crap I just realized i can use splices to test the ppr is inverse relationship...
-- | My bad.
-- | Term Tests                
--------------------------------------------------------------------------------
test_parseTermTerm_12 = TCompose (TBase $ IEliminate $ BAssociativeS) (TBase $ IIntroduce $ BAssociativeS)    @?= [term| (< (# |+|+|)) ; (< (' |+|+|)) |] 
test_parseTermTerm_13 = TTimes   (TBase $ IEliminate $ BIdentityS)    (TBase $ IIntroduce $ BAssociativeS)     @?= [term| (< (# <=+=>)) * (< (' |+|+|)) |] 
test_parseTermTerm_14 = TPlus    (TBase $ IEliminate $ BIdentityS)    (TBase $ IIntroduce $ BAssociativeS)     @?= [term| (< (# <=+=>)) + (< (' |+|+|)) |] 
test_parseTermTerm_15 = (TBase $ IEliminate $ BAssociativeP)                                                  @?= [term| < # |*|*|                     |] 
test_parseTermTerm_16 = TId      (Ident "i")                                                                  @?= [term| i                             |] 

-- | Value Tests
--------------------------------------------------------------------------------
------------------------------------------I am really testing the Unit here. Watch this guys. Watch this. This is what Unit testing is all about!
test_parseValue_0 = VTuple  VUnit VUnit @?= [value| ( u, u ) |] 
test_parseValue_1 = VLeft   VUnit       @?= [value| L u      |] 
test_parseValue_2 = VRight  VUnit       @?= [value| R u      |] 
test_parseValue_3 = VNegate VUnit       @?= [value| - u      |] 
test_parseValue_4 = VReciprocate VUnit  @?= [value| / u      |] 
test_parseValue_5 = VUnit               @?= [value| u        |] 



-- evalIso 
------------------------------------------------------------------------------
termEval :: P.Term -> [UValue]
termEval x = topLevel x unit

isoEval :: P.Iso -> [UValue]
isoEval = termEval . P.Base

test_evalEliminateIdentityS = do
    let actual   = isoEval $ to [iso| # <=+=> |]
        expected = [UTerm (P.Left (UTerm Unit))]
    
    assertBool "test_evalEliminateIdentityS" $ all fst $ traceIt $ observeAll $ 
        runIntBindingT ((all id) <$> (mapM (\(x, y) -> x === y) $ zip actual expected))

