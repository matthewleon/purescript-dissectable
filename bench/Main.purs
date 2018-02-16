module Bench.Main where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Safely (traverse_)
import Data.Dissectable (traverseP)
import Data.List (List)
import Data.Traversable (traverse)
import Data.Unfoldable as A
import Data.Unfoldable as U
import Performance.Minibench (bench)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let longLength = 1000
      longArray  = A.range 1 longLength
      noop = pure $ pure unit

  log "foreachE traversal of array"
  bench \_ -> unsafePerformEff $ foreachE longArray noop

  log "traverseP traversal of array"
  bench \_ -> unsafePerformEff $ traverseP noop longArray

  log "safely traversal of array"
  bench \_ -> unsafePerformEff $ traverse_ noop longArray

  let longList = U.range 1 longLength :: List Int

  log "traverse traversal of list"
  bench \_ -> unsafePerformEff $ traverse noop longList

  log "traverseP traversal of list"
  bench \_ -> unsafePerformEff $ traverseP noop longList

  log "safely traversal of list"
  bench \_ -> unsafePerformEff $ traverse_ noop longList
