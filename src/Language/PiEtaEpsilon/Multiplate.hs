{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveTraversable, FlexibleInstances, TypeSynonymInstances #-}
module Language.PiEtaEpsilon.Multiplate where

-- 0) Prelude
-- import Prelude hiding (Either(..), negate)

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
import Language.PiEtaEpsilon.Syntax (Type(..), ValueF(..), IsoBase(..), Iso(..), Term(..), adjoint, adjointIso)
import Language.PiEtaEpsilon.Evaluator (UValue, Context(..), MachineState(..), PEET(..))

-- 8) just to make types easier to read
import Control.Unification.IntVar
import Control.Monad.Logic

-- just to get the contexts out, we'll need:
--
--   context
--   term
--   uvalue
--
data Plate f = Plate
     {
     -- resulting in terms
       base    :: Iso          -> f Term
     , idT     ::                 f Term       
     , comp    :: Term -> Term -> f Term
     , plus    :: Term -> Term -> f Term
     , times   :: Term -> Term -> f Term
     -- resulting in uvalues
     -- resulting in contexts
--     , first   :: Context -> Term -> f Context
--     , lsum    :: Context -> Term -> f Context     
     , second  :: Term -> Context -> f Context
     , rsum    :: Term -> Context -> f Context
--     , lprod   :: Context -> Term -> UValue -> f Context
--     , rprod   :: Term -> UValue -> Context -> f Context
     }


traverseTuple fa fb (a,b) = (,) <$> fa a <*> fb b

term1 :: Plate f -> Term -> f Term
term1 plate (Base i)     = base    plate i
term1 plate (Id)         = idT     plate
term1 plate ((:::) l r)  = comp    plate l r
term1 plate ((:+:) l r)  = plus    plate l r
term1 plate ((:*:) l r)  = times   plate l r



{-
cxtL :: Plate f -> Context -> Term -> f Context
cxtL plate (Fst c ti) t0 = first plate (Fst c ti) t0
ctxL plate (LSum c ti) t0 = lsum plate (LSum c ti) t0
-}


cxtR :: Plate f -> Term -> Context -> f Context
cxtR plate t (Snd _ c) = second plate t c
cxtR plate t (RSum _ c) = rsum plate t c



-- I guess i'll write the last two manually? maybe not:
instance Multiplate Plate where
  multiplate plate = Plate
     (\i -> pure (Base i))
     (pure (Id))     
     (\l r -> (:::) <$> term1 plate l <*> term1 plate r)
     (\l r -> (:+:) <$> term1 plate l <*> term1 plate r)
     (\l r -> (:*:) <$> term1 plate l <*> term1 plate r)

     -- resulting in contexts:
--     (\c t -> Fst <$> cxtL plate c t <*> term1 plate t)
--     (\c t -> LSum <$> cxtL plate c t <*> term1 plate t)
     (\t0 c -> Snd  <$> term1 plate t0 <*> cxtR plate t0 c)
     (\t0 c -> RSum <$> term1 plate t0 <*> cxtR plate t0 c)
  mkPlate build = Plate
     (\i -> build term1 (Base i))
     (build term1 (Id))     
     (\l r -> build term1 ((:::) l r))
     (\l r -> build term1 ((:+:) l r))
     (\l r -> build term1 ((:*:) l r))
     (\t c -> undefined)
     (\t c -> undefined)

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
-- {{{2 Preorder Folds

testIsos = foldFor term1 preordFoldIsos

-- |
-- >>> testIsos $ prepareP
-- [Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP]

-- * Note: if you swap the l and r in multiplate plate, then you get a reverse ordered list:
-- >>> testIsos $ prepareP
-- [Introduce AssociativeP,Eliminate SplitP,Introduce IdentityP]

-- |
-- >>> testIsos $ adjoint prepareP
-- [Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP]
 
-- |
-- >>> testIsos $ prepareS
-- [Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS]
 
-- |
-- >>> testIsos $ adjoint prepareS
-- [Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS]
 
-- |
-- >>> testIsos $ traceS Id
-- [Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS,Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS]
 
-- |
-- >>> testIsos $ traceP Id
-- [Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP,Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP]
 
-- |
-- >>> testIsos $ traceP (traceS Id)
-- [Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP,Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS,Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS,Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP]
 
-- |
-- >>> testIsos $ traceS (traceP Id)
-- [Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS,Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP,Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP,Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS]


-- {{{2 Postorder Folds
-- Postorder Folds are (evidently) the same thing here:
--
testIsosAlt = foldFor term1 postordFoldIsos

-- |
-- >>> testIsosAlt $ prepareP
-- [Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP]
 
-- |
-- >>> testIsosAlt $ adjoint prepareP
-- [Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP]
 
-- |
-- >>> testIsosAlt $ prepareS
-- [Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS]
 
-- |
-- >>> testIsosAlt $ adjoint prepareS
-- [Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS]
 
-- |
-- >>> testIsosAlt $ traceS Id
-- [Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS,Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS]
 
-- |
-- >>> testIsosAlt $ traceP Id
-- [Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP,Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP]
 
-- |
-- >>> testIsosAlt $ traceP (traceS Id)
-- [Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP,Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS,Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS,Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP]
 
-- |
-- >>> testIsosAlt $ traceS (traceP Id)
-- [Introduce IdentityS,Eliminate SplitS,Introduce AssociativeS,Introduce IdentityP,Eliminate SplitP,Introduce AssociativeP,Eliminate AssociativeP,Introduce SplitP,Eliminate IdentityP,Eliminate AssociativeS,Introduce SplitS,Eliminate IdentityS]



-- {{{1 Constant Folds

{-
doConstFold :: Plate Identity
doConstFold = purePlate 
          { base  = undefined
          ,  idT  = undefined
          , comp  = undefined
          , plus  = undefined
          , times = undefined
          }
-}

