module Data.Dissectable where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))

-- Largely cribbed from Phil Freeman's "Stack-Safe Traversals via Dissection"
-- http://blog.functorial.com/posts/2017-06-18-Stack-Safe-Traversals-via-Dissection.html

class (Traversable f, Bifunctor d) <= Dissectable f d | f -> d where
  moveRight :: forall c j. Either (f j) (Tuple (d c j) c) -> Either (f c) (Tuple (d c j) j)
  moveLeft  :: forall c j. Either (f c) (Tuple (d c j) j) -> Either (f j) (Tuple (d c j) c)

mapD :: forall f d a b. Dissectable f d => (a -> b) -> f a -> f b
mapD f = tailRec step <<< Left
  where
  step = moveRight >>> case _ of
           Left xs -> Done xs
           Right (Tuple dba a) -> Loop (Right (Tuple dba (f a)))

traverseRec
  :: forall m f d a b
   . Dissectable f d
  => MonadRec m
  => (a -> m b) -> f a -> m (f b)
traverseRec f = tailRecM step <<< Left
  where
  step = moveRight >>> case _ of
           Left xs -> pure $ Done xs
           Right (Tuple dba a) -> Loop <<< Right <<< Tuple dba <$> f a

sequenceRec
  :: forall m f d a
   . Dissectable f d
  => MonadRec m
  => f (m a) -> m (f a)
sequenceRec = tailRecM step <<< Left
  where
  step = moveRight >>> case _ of
           Left xs -> pure $ Done xs
           Right (Tuple dba a) -> Loop <<< Right <<< Tuple dba <$> a
