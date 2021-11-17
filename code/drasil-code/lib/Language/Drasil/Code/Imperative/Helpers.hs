module Language.Drasil.Code.Imperative.Helpers (
  liftS, lookupC
) where

import Language.Drasil (QuantityDict)
import Database.Drasil (UID, findOrErr)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..))

import Control.Monad.State (State)

-- | Puts a state-dependent value into a singleton list.
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- | Gets the 'QuantityDict' corresponding to a 'UID'.
lookupC :: DrasilState -> UID -> QuantityDict -- TODO: This is a duplicate of a similarly named function from another package. We should create "chunk util functions" for each chunk type, in the same file the chunk is defined.
lookupC g u = findOrErr u (sysinfodb $ codeSpec g)
