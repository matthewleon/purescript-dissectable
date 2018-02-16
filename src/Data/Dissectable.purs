module Data.Dissectable where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM)
import Data.Array as A
import Data.Bifunctor (class Bifunctor)
import Data.Bifunctor.Clown (Clown(..))
import Data.Bifunctor.Joker (Joker(..))
import Data.Bifunctor.Product (Product(..))
import Data.Either (Either(..))
import Data.List (List(..), reverse, tail, (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- Largely cribbed from Phil Freeman's "Stack-Safe Traversals via Dissection"
-- http://blog.functorial.com/posts/2017-06-18-Stack-Safe-Traversals-via-Dissection.html

-- TODO: moveLeft
class (Traversable f, Bifunctor d) <= Dissectable f d | f -> d where
  moveRight :: forall c j. Either (f j) (Tuple (d c j) c) -> Either (f c) (Tuple (d c j) j)

-- TODO: compare implementation with lazy list
-- This might gain speed by not actually reversing the list when that's unnecessary
instance dissectableArray :: Dissectable Array (Product (Clown List) (Joker List)) where
  moveRight (Left xs) =
    case A.head xs of
      Nothing -> Left []
      Just x  -> Right (Tuple (Product (Clown Nil) (Joker $ unsafePartial $ fromJust $ tail $ L.fromFoldable xs)) x)
  moveRight (Right (Tuple (Product (Clown cs) (Joker js)) c)) =
    case js of
      Nil -> Left $ A.reverse $ A.fromFoldable $ c : cs
      j : js' -> Right (Tuple (Product (Clown (c : cs)) (Joker js')) j)

instance dissectableList :: Dissectable List (Product (Clown List) (Joker List)) where
  moveRight (Left Nil) = Left Nil
  moveRight (Left (j : js)) = Right (Tuple (Product (Clown Nil) (Joker js)) j)
  moveRight (Right (Tuple (Product (Clown cs) (Joker js)) c)) =
    case js of
      Nil -> Left (reverse (c : cs))
      j : js' -> Right (Tuple (Product (Clown (c : cs)) (Joker js')) j)

mapP :: forall f d a b. Dissectable f d => (a -> b) -> f a -> f b
mapP f xs = tailRec step (Left xs) where
  step = moveRight >>> case _ of
           Left ys -> Done ys
           Right (Tuple dba a) -> Loop (Right (Tuple dba (f a)))

traverseP :: forall m f d a b. Dissectable f d => MonadRec m => (a -> m b) -> f a -> m (f b)
traverseP f xs = tailRecM step (Left xs) where
  step = moveRight >>> case _ of
           Left ys -> pure (Done ys)
           Right (Tuple dba a) -> Loop <<< Right <<< Tuple dba <$> f a
