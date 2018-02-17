module Data.Dissectable where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM)
import Data.Array as A
import Data.Bifunctor (class Bifunctor)
import Data.Bifunctor.Clown (Clown(..))
import Data.Bifunctor.Joker (Joker(..))
import Data.Bifunctor.Product (Product(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

-- Largely cribbed from Phil Freeman's "Stack-Safe Traversals via Dissection"
-- http://blog.functorial.com/posts/2017-06-18-Stack-Safe-Traversals-via-Dissection.html

class (Traversable f, Bifunctor d) <= Dissectable f d | f -> d where
  moveRight :: forall c j. Either (f j) (Tuple (d c j) c) -> Either (f c) (Tuple (d c j) j)
  moveLeft  :: forall c j. Either (f c) (Tuple (d c j) j) -> Either (f j) (Tuple (d c j) c)

-- | The `Dissectable` instance of `Array` uses `Lists` to represent its zipper.
-- | A more canonical instance would use `Arrays` for this purpose, at the cost
-- | of O(n^2) traversal.
instance dissectableArray :: Dissectable Array (Product (Clown List) (Joker List)) where
  moveRight (Left js) =
    case L.uncons $ L.fromFoldable js of
      Nothing                   -> Left []
      Just {head: j, tail: js'} -> Right $ Tuple
        (Product
          (Clown Nil)
          (Joker js'))
        j
  moveRight (Right (Tuple (Product (Clown cs) (Joker js)) c)) =
    case js of
      Nil     -> Left  $ A.reverse $ A.fromFoldable $ c : cs
      j : js' -> Right $ Tuple
        (Product
          (Clown $ c : cs)
          (Joker js'))
        j

  moveLeft (Left cs) =
    case L.uncons $ L.fromFoldable cs of
      Nothing                   -> Left []
      Just {head: c, tail: cs'} -> Right $ Tuple
        (Product
          (Clown cs')
          (Joker Nil))
        c
  moveLeft (Right (Tuple (Product (Clown cs) (Joker js)) j)) =
    case cs of
      Nil     -> Left  $ A.reverse $ A.fromFoldable $ j : js
      c : cs' -> Right $ Tuple
        (Product
          (Clown cs')
          (Joker (j : js)))
        c

instance dissectableList :: Dissectable List (Product (Clown List) (Joker List)) where
  moveRight (Left Nil) = Left Nil
  moveRight (Left (j : js)) = Right (Tuple (Product (Clown Nil) (Joker js)) j)
  moveRight (Right (Tuple (Product (Clown cs) (Joker js)) c)) =
    case js of
      Nil -> Left (L.reverse (c : cs))
      j : js' -> Right (Tuple (Product (Clown (c : cs)) (Joker js')) j)

  moveLeft (Left Nil) = Left Nil
  moveLeft (Left (c : cs)) = Right (Tuple (Product (Clown cs) (Joker Nil)) c)
  moveLeft (Right (Tuple (Product (Clown cs) (Joker js)) j)) =
    case cs of
      Nil -> Left (L.reverse (j : js))
      c : cs' -> Right (Tuple (Product (Clown cs') (Joker (j : js))) c)

mapD :: forall f d a b. Dissectable f d => (a -> b) -> f a -> f b
mapD f = tailRec step <<< Left
  where
  step = moveRight >>> case _ of
           Left xs -> Done xs
           Right (Tuple dba a) -> Loop (Right (Tuple dba (f a)))

foldD :: forall f d a b. Dissectable f d => (b -> a -> b) -> b -> f a -> b
foldD f b = tailRec step <<< Tuple b <<< Left
  where
  step (Tuple acc dis) = case moveRight dis of
           Left  _                 -> Done acc
           r@(Right (Tuple dba a)) -> Loop $ Tuple (f acc a) r

foldMapD :: forall f d a b. Dissectable f d => Monoid b => (a -> b) -> f a -> b
foldMapD f = foldD (\b a -> b `append` f a) mempty

-- | Convert a `Dissectable` to any `Unfoldable` structure.
toUnfoldable :: forall f g d. Dissectable f d => Unfoldable g => f ~> g
toUnfoldable = unfoldr step <<< Left
  where
  step = moveRight >>> case _ of
           Left xs                 -> Nothing
           r@(Right (Tuple dba a)) -> Just $ Tuple a r

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
