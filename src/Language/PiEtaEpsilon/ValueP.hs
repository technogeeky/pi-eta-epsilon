{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, TypeSynonymInstances #-}
module Language.PiEtaEpsilon.ValueP where

-- 0) Prelude
import Prelude hiding (Either(..), negate, recip)

-- 1) Applicative
import Control.Applicative
import Control.Applicative.Backwards
-- import Control.Applicative.Lift

-- 2) deriving (Foldable, Traversable)
import Data.Foldable
import Data.Traversable

-- 3) deriving (Data, Typeable) 
-- import Data.Data
-- import Data.Typeable
-- import Test.QuickCheck

-- 4) All the Functor Goodness
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Fixedpoint
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse

-- 5) Generics
-- import GHC.Generics hiding ((:*:))
import Data.Generics.Multiplate

-- 6) Monads
-- import Control.Monad.State
-- import Control.Monad.Trans
-- import Control.Monad.Writer hiding (Product(..), Sum(..))
-- import Control.Unification

-- 7) p-e-e specific
import Language.PiEtaEpsilon.Syntax (ValueF(..))

--import Language.PiEtaEpsilon.Syntax (Type(..), ValueF(..), IsoBase(..), Iso(..), Term(..), adjoint, adjointIso)
import Language.PiEtaEpsilon.Evaluator (UValue, Context(..), MachineState(..), PEET(..))

-- 8) just to make types easier to read
import Control.Unification.IntVar
import Control.Monad.Logic


data Plate t f  = Plate
     { 
       unit    ::                f (ValueF t)
     , l       :: ValueF t    -> f (ValueF t)
     , r       :: ValueF t    -> f (ValueF t)
     , lr      :: ValueF t    -> ValueF t  -> f (ValueF t)
     , neg     :: ValueF t    -> f (ValueF t)
     , recip   :: ValueF t    -> f (ValueF t)
     }


valueF :: Plate t f -> ValueF t -> f (ValueF t)
valueF plate (Unit)           = unit  plate
valueF plate (Left t)         = l     plate (Left t)
valueF plate (Right t)        = r     plate (Right t)
valueF plate (Negate t)       = neg   plate (Negate t)
valueF plate (Reciprocate t)  = recip plate (Reciprocate t)


valueFtuple :: Plate t f -> ValueF t -> ValueF t -> f (ValueF t)
valueFtuple plate (Tuple x y) = lr plate (Tuple x y)
valueFtuple plate ___________ = error "valueFtuple doesn't define this"

instance Multiplate (Plate t) where
     multiplate plate = Plate
          (pure Unit)
          (\t -> Left <$> l plate <*> pure t)
          (\t -> Right <$> r plate t)
          (\t -> undefined)
          (\t -> undefined)
          (\t -> undefined)
     mkPlate build = Plate
          (build valueF Unit)
          (\t -> undefined)
          (\t -> undefined)
          (\t -> undefined)
          (\t -> undefined)
          (\t -> undefined)

{-
instance Multiplate Plate where
  multiplate plate = Plate
     (\i -> pure (Base i))
     (pure (Id))     
     (\l r -> (:::) <$> term1 plate l <*> term1 plate r)
     (\l r -> (:+:) <$> term1 plate l <*> term1 plate r)
     (\l r -> (:*:) <$> term1 plate l <*> term1 plate r)
  mkPlate build = Plate
     (\i -> build term1 (Base i))
     (build term1 (Id))     
     (\l r -> build term1 ((:::) l r))
     (\l r -> build term1 ((:+:) l r))
     (\l r -> build term1 ((:*:) l r))

--   
--             Examples for testing go here
--
traceS f = prepareS ::: (Id :+: f) ::: adjoint prepareS 
traceP f = prepareP ::: (Id :*: f) ::: adjoint prepareP 
prepareP = Base (Introduce IdentityP) ::: (Base (Eliminate SplitP) :+: Id) ::: Base (Introduce AssociativeP)
prepareS = Base (Introduce IdentityS) ::: (Base (Eliminate SplitS) :+: Id) ::: Base (Introduce AssociativeS)



getIsosP :: Plate (Constant [Iso])
getIsosP = purePlate { base = \v -> Constant [v] }


preordFoldIsos :: Plate (Constant [Iso])
preordFoldIsos = preorderFold getIsosP

postordFoldIsos :: Plate (Constant [Iso])
postordFoldIsos = postorderFold getIsosP



getTermsP :: Plate Identity
getTermsP = purePlate
     { comp = doComp
     , plus = doPlus
     , times = doTimes
     }
   where
     doComp (Id)     (Base i) = pure $ Id     ::: Base i
     doComp (Base i) (Id)     = pure $ Base i ::: Id
     doComp (Base x) (Base y) = pure $ Base x ::: Base y
     doTimes (Id)     (Base i) = pure $ Id     :*: Base i
     doTimes (Base i) (Id)     = pure $ Base i :*: Id
     doTimes (Base x) (Base y) = pure $ Base x :*: Base y
     doPlus (Id)     (Base i) = pure $ Id     :+: Base i
     doPlus (Base i) (Id)     = pure $ Base i :+: Id
     doPlus (Base x) (Base y) = pure $ Base x :+: Base y

constFoldPlate :: Plate Identity
constFoldPlate = mapFamily getTermsP

testCF = traverseFor term1 constFoldPlate
-}

