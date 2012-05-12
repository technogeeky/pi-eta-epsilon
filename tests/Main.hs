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
import Data.Generics.Uniplate.Data
import Control.Applicative ((<$>))    
import Control.Monad.Error
import Language.PiEtaEpsilon
import Language.PiEtaEpsilon.Pretty.Debug
import Debug.Trace
import Language.PiEtaEpsilon.BNFMeta.Term
import Language.PiEtaEpsilon.BNFMeta.Value


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
-- | Iso Tests                
--------------------------------------------------------------------------------
                testCase "test_parseTermIsobase_10" test_parseTermIso_10,
                testCase "test_parseTermIsobase_11" test_parseTermIso_11,
-- | Term Tests                
--------------------------------------------------------------------------------
                testCase "test_parseTermIsobase_12" test_parseTermTerm_12,
                testCase "test_parseTermIsobase_13" test_parseTermTerm_13,
                testCase "test_parseTermIsobase_14" test_parseTermTerm_14,
                testCase "test_parseTermIsobase_15" test_parseTermTerm_15,
                testCase "test_parseTermIsobase_16" test_parseTermTerm_16
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
test_parseTermIso_10 = IEliminate @?= [iso| X |] 
test_parseTermIso_11 = IIntroduce @?= [iso| V |] 

-- | Crap I just realized i can use splices to test the ppr is inverse relationship...
-- | My bad.
-- | Term Tests                
--------------------------------------------------------------------------------
test_parseTermTerm_12 = TCompose @?= [term| (X <=+=>) . (Y |+|+| )|] 
test_parseTermTerm_13 = TPlus    @?= [term| (X <=+=>) . (Y |+|+| )|] 
test_parseTermTerm_14 = TTimes   @?= [term| (X <=+=>) . (Y |+|+| )|] 
test_parseTermTerm_15 = TBase    @?= [term|@ (Y -+<)              |] 
test_parseTermTerm_16 = TId      @?= [term| i                     |] 

-- | Value Tests
--------------------------------------------------------------------------------
test_parseValue_0 = VTuple       @?= [value| ((), ()) |] 
test_parseValue_1 = VLeft        @?= [value| L  |] 
test_parseValue_2 = VRight       @?= [value| R  |] 
test_parseValue_3 = VNegate      @?= [value| -  |] 
test_parseValue_4 = VReciprocate @?= [value| /  |] 
test_parseValue_5 = VUnit        @?= [value| () |] 












--TODO write all the unit tests and the quickcheck properities
--that stuff works
    
      
