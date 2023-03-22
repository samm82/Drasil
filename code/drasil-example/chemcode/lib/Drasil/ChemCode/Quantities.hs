module Drasil.ChemCode.Quantities where

import Language.Drasil hiding (matrix)
import Language.Drasil.ShortHands

inputs :: [QuantityDict]
inputs = [r]

r :: QuantityDict
r = vcSt "r" (nounPhraseSP "representation of a chemical equation")
  (autoStage lR) String -- FIXME: should this be a string?