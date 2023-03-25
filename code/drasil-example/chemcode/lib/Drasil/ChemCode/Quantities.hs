module Drasil.ChemCode.Quantities where

import Language.Drasil hiding (matrix)
import Language.Drasil.ShortHands

inputs :: [QuantityDict]
inputs = [r]

quants :: [QuantityDict]
quants = inputs ++ [cVec, xVec]

cVec, r, xVec :: QuantityDict

cVec = vc "cVec" (nounPhraseSP "generic vector") (vec lC) (Vect Real)

r = vcSt "r" (nounPhraseSP "representation of a chemical equation")
  (autoStage lR) String -- FIXME: should this be a string?

xVec = vc "xVec" (nounPhraseSP "generic vector") (vec lX) (Vect Real)
