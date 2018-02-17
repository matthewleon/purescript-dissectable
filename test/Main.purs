module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as A
import Data.Dissectable as D
import Data.Foldable (sum)
import Data.List (List)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (ala)
import Data.Unfoldable as U
import Test.Assert (ASSERT, assert)

main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
  log "check mapD"
  let arr = A.range 0 9
  assert $ D.mapD (_ + 1) arr == A.range 1 10
  assert $ D.mapD (_ + 1) arr == map (_ + 1) arr

  log "check toUnfoldable"
  assert $ D.toUnfoldable arr == (U.range 0 9 :: List Int)
  assert $ D.toUnfoldable arr == (A.toUnfoldable arr :: List Int)

  log "check foldD"
  assert $ D.foldD (+) 0 arr == sum arr

  log "check foldMapD"
  assert $ ala Additive D.foldMapD arr == sum arr
