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

class (Traversable f, Bifunctor d) <= Dissectable f d | f -> d where
  moveRight :: forall c j. Either (f j) (Tuple (d c j) c) -> Either (f c) (Tuple (d c j) j)
  moveLeft  :: forall c j. Either (f c) (Tuple (d c j) j) -> Either (f j) (Tuple (d c j) c)

instance dissectableArray :: Dissectable Array (Product (Clown List) (Joker List)) where
  moveRight (Left js) =
    case A.head js of
      Nothing -> Left []
      Just j  -> Right $ Tuple
        (Product
          (Clown Nil)
          (Joker $ unsafePartial $ fromJust $ tail $ L.fromFoldable js))
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
    case A.head cs of
      Nothing -> Left []
      Just c  -> Right $ Tuple
        (Product
          (Clown $ unsafePartial $ fromJust $ tail $ L.fromFoldable cs)
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
      Nil -> Left (reverse (c : cs))
      j : js' -> Right (Tuple (Product (Clown (c : cs)) (Joker js')) j)

  moveLeft (Left Nil) = Left Nil
  moveLeft (Left (c : cs)) = Right (Tuple (Product (Clown cs) (Joker Nil)) c)
  moveLeft (Right (Tuple (Product (Clown cs) (Joker js)) j)) =
    case cs of
      Nil -> Left (reverse (j : js))
      c : cs' -> Right (Tuple (Product (Clown cs') (Joker (j : js))) c)

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
