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
import Language.PiEtaEpsilon.Syntax (Type(..), ValueF(..), IsoBase(..), Iso(..), Term(..))
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
       base    :: Iso -> f Term
     , idT     ::        f Term
     , comp    :: Term -> Term -> f Term
     , plus    :: Term -> Term -> f Term
     , times   :: Term -> Term -> f Term
     -- resulting in uvalues
     -- resulting in contexts
--     , first   :: Context -> Term -> f Context
--     , lsum    :: Context -> Term -> f Context     
--     , second  :: Term -> Context -> f Context
--     , rsum    :: Term -> Context -> f Context
--     , lprod   :: Context -> Term -> UValue -> f Context
--     , rprod   :: Term -> UValue -> Context -> f Context
     }



term1 :: Plate f -> Term -> f Term
term1 plate (Base i) = base plate i
--term1 plate (Id) = idT plate
term1 plate ((:::) l r) = comp plate l r
term1 plate ((:+:) l r) = plus plate l r
term1 plate ((:*:) l r) = times plate l r


-- I think this is wrong... I think these should be curried over one argument
term2 :: Plate f -> Term -> Term -> f Term
term2 plate (l ::: r) = comp plate  ((:::) l r)
term2 plate (l :+: r) = plus plate  ((:+:) l r)
term2 plate (l :*: r) = times plate ((:*:) l r)

{-

cxtL :: Plate f -> Context -> Term -> f Context
cxtL plate (Fst c _) t0 = first plate (Fst c t0) t0
ctxL plate (LSum c _) t0 = lsum plate (LSum c t0) t0
-}

{-
cxtR :: Plate f -> Term -> Context -> f Context
cxtR plate t (Snd _ c) = second plate t c
cxtR plate t (RSum _ c) = rsum plate t c
-}


-- I guess i'll write the last two manually? maybe not:

{-
instance Multiplate Plate where
  multiplate plate = Plate
     (\i -> pure (Base i))
--     (\t -> pure (Id))
     (\l r -> (:::) <$> term2 plate l r <*> term2 plate r l)
     (\l r -> (:+:) <$> term2 plate l r <*> term2 plate r l)
     (\l r -> (:*:) <$> term2 plate l r <*> term2 plate r l)
     -- resulting in contexts:
--     (\c t0 -> Fst <$> cxtL plate c t0 <*> term2 plate t0 t0)
--     (\c t0 -> LSum <$> cxtL plate c t0 <*> term2 plate t0 t0)
--     (\t0 c -> Snd  <$> cxtR plate t0 c <*> term2 plate t0 t0)
--     (\t0 c -> RSum <$> cxtR plate t0 c <*> term2 plate t0 t0)
  mkPlate build = Plate
     (\i -> build term1 (Base i))
--     (\t -> build term1 id)
     (\l r -> build term1 ((:::) l r))
     (\l r -> build term1 ((:+:) l r))
     (\l r -> build term1 ((:*:) l r))
--     (\c t -> build cxtL (Fst c t) t)
--     (\c t -> build cxtL (LSum c t) t)



getVPlate :: Plate (Constant [Iso])
getVPlate = purePlate { base = \v -> Constant [v] }

varsPlate :: Plate (Constant [Iso])
varsPlate = preorderFold getVPlate

prepareP = Base (Introduce IdentityP) ::: (Base (Eliminate SplitP) :+: Id) ::: Base (Introduce AssociativeP)


--testIsos :: [Iso]
testIsos = foldFor term1 varsPlate
-}
