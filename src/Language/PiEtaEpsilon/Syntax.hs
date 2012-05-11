-- boilerplate {{{1
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, TypeSynonymInstances #-}
module Language.PiEtaEpsilon.Syntax where
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (Product(..), Sum(..))
import Control.Unification

import Data.Foldable
import Data.Functor.Fixedpoint
import Data.Traversable
import Data.Data
import Data.Maybe
import Data.Typeable

import Data.Unfoldable
import Data.Unfolder
import Test.QuickCheck hiding (choose)
import System.Random

import Prelude hiding (Either(..), negate)
import GHC.Generics hiding ((:*:))
import qualified Test.QuickCheck as QC

-- types {{{1
-- Type {{{2
data Type a
	= Zero
	| One
	| Sum        (Type a) (Type a)
	| Product    (Type a) (Type a)
	| Negative   (Type a)
	| Reciprocal (Type a)
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

-- values and unification variables {{{2
data ValueF t
	= Unit
	| Left        t
	| Right       t
	| Tuple       t t
	| Negate      t
	| Reciprocate t
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable)

type Value = Fix ValueF


instance Unfoldable ValueF where
     unfold fa = choose
          [ pure Unit
          , Left         <$> fa
          , Right        <$> fa
          , Tuple        <$> fa <*> fa
          , Negate       <$> fa
          , Reciprocate  <$> fa
          ]

valuefShapes :: [ValueF ()]
valuefShapes = take 100 unfoldBF_

valuefRand :: IO (ValueF Bool)
valuefRand = getStdRandom randomDefault

instance Unfoldable Type where
     unfold fa = choose
          [ pure Zero
          -- ...
          , pure One
          -- ...
          , Sum <$> unfold fa <*> unfold fa
          , Product <$> unfold fa <*> unfold fa
          , Negative <$> unfold fa
          , Reciprocal <$> unfold fa
          ]

typeShapes :: [Type ()]
typeShapes = take 30 unfoldBF_

typeRand :: IO (Type Bool)
typeRand = getStdRandom randomDefault

-- ^ somewhat amusing... I got back:
--  Zero
--  One
--  <... 100 lines ... >


-- isomorphisms {{{2
data IsoBase a
	= IdentityS (Type a) | CommutativeS (Type a) (Type a) | AssociativeS (Type a) (Type a) (Type a)
	| IdentityP (Type a) | CommutativeP (Type a) (Type a) | AssociativeP (Type a) (Type a) (Type a)
	| DistributiveZero (Type a)
	| DistributivePlus (Type a) (Type a) (Type a)
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Unfoldable IsoBase where
     unfold fa = choose
          [ IdentityS <$> unfold fa
          , IdentityP <$> unfold fa
          , CommutativeS <$> unfold fa <*> unfold fa
          , CommutativeP <$> unfold fa <*> unfold fa
          , AssociativeS <$> unfold fa <*> unfold fa <*> unfold fa
          , AssociativeP <$> unfold fa <*> unfold fa <*> unfold fa
          , DistributiveZero <$> unfold fa
          , DistributivePlus <$> unfold fa <*> unfold fa <*> unfold fa
          ]
data Iso a
	= Eliminate (IsoBase a)
	| Introduce (IsoBase a)
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


instance Unfoldable Iso where
     unfold fa = choose
          [ Eliminate <$> unfold fa
          , Introduce <$> unfold fa
          ]

isoShapes :: [Iso ()]
isoShapes = take 300 unfoldBF_

isoRand :: IO (Iso Bool)
isoRand = getStdRandom randomDefault

-- Term {{{2
data Term a
	= Base (Iso a)
	| Id (Type a)
	| (Term a) ::: (Term a)
	| (Term a) :+: (Term a)
	| (Term a) :*: (Term a)
	deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


instance Unfoldable Term where
     unfold fa = choose
          [ Base    <$> unfold fa
          , Id      <$> unfold fa
          , (:::)   <$> (unfold fa) <*> (unfold fa)
          , (:+:)   <$> (unfold fa) <*> (unfold fa)
          , (:*:)   <$> (unfold fa) <*> (unfold fa)
          ]

termShapes :: [Term ()]
termShapes = take 100 unfoldBF_
randomTerm :: IO (Term Bool)
randomTerm = getStdRandom randomDefault

-- convenience names for Values {{{1
class Particle a where
	unit :: a
	left, right, negate, reciprocate :: a -> a
	tuple :: a -> a -> a

instance Particle (Fix ValueF) where
	unit        = Fix Unit
	left        = Fix . Left
	right       = Fix . Right
	negate      = Fix . Negate
	reciprocate = Fix . Reciprocate
	tuple t1 t2 = Fix (Tuple t1 t2)

instance Particle (UTerm ValueF v) where
	unit        = UTerm Unit
	left        = UTerm . Left
	right       = UTerm . Right
	negate      = UTerm . Negate
	reciprocate = UTerm . Reciprocate
	tuple t1 t2 = UTerm (Tuple t1 t2)

-- doesn't need to be part of a type class yet, I guess
var = UVar

-- arbitrary instances {{{1
----------------------------------------------------------------------------------------------------
----------                          Arbitrary Instances                                  -----------
----------------------------------------------------------------------------------------------------

{-
instance Arbitrary Type where
    arbitrary = sized arb' where
        arb' size = do
            let maxChoice = if size == (0 :: Int) then 1 :: Int else 5
            c <- QC.choose(0, maxChoice)
            case c of
                0 -> return Zero
                1 -> return One
                2 -> Sum        <$> arb' (size `div` 2) <*> arb' (size `div` 2)
                3 -> Product    <$> arb' (size `div` 2) <*> arb' (size `div` 2)
                4 -> Negative   <$> arb' (size - 1)
                5 -> Reciprocal <$> arb' (size - 1)

    shrink Zero           = []
    shrink One            = []
    shrink (Sum x y)      = [x, y]
    shrink (Product x y)  = [x, y]
    shrink (Negative x)   = [x]
    shrink (Reciprocal x) = [x]

instance Arbitrary Value where
    arbitrary = sized arb' where
        arb' size = do
            let maxChoice = if size == 0 then 1 else 5
            c <- QC.choose(0 :: Int, maxChoice)
            case c of
                0 -> return unit
                1 -> left   <$>  arb' (size - 1)
                2 -> right  <$>  arb' (size - 1)
                3 -> tuple  <$>  arb' (size `div` 2) <*> arb' (size `div` 2)
                4 -> negate <$>  arb' (size - 1)
                5 -> reciprocate <$> arb' (size - 1)

-}
-- pretty printer {{{1
------------------------------------------------------------------------------------
----                              PrettyPrint                               --------
------------------------------------------------------------------------------------
{-
pprType :: Type -> String
pprType Zero = "0"
pprType One  = "1"
pprType (Sum     x y) = "(" ++ pprType x ++ " + " ++ pprType y ++ ")"
pprType (Product x y) = "(" ++ pprType x ++ " * " ++ pprType y ++ ")"
pprType (Negative   x) = "(" ++ " - " ++ pprType x ++ ")"
pprType (Reciprocal x) = "(" ++ " / " ++ pprType x ++ ")"
-}
