module Drasil.ChemCode.Changes where

import Language.Drasil

import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (unlikeChgDom)

import Drasil.ChemCode.Assumptions

uChanges :: [ConceptInstance]
uChanges = [allEqsPermitted, checkValidForms, checkValidEqns]

allEqsPermitted, checkValidForms, checkValidEqns :: ConceptInstance
allEqsPermitted = cic "allEqsPermitted"      allEqsPermittedDesc      "allEqsPermitted"      unlikeChgDom
checkValidForms = cic "checkValidForms"      checkValidFormsDesc      "checkValidForms"      unlikeChgDom
checkValidEqns = cic "checkValidEqns"      checkValidEqnsDesc      "checkValidEqns"      unlikeChgDom

allEqsPermittedDesc, checkValidFormsDesc, checkValidEqnsDesc :: Sentence

allEqsPermittedDesc = foldlSent [refS elemCompDiff, S "assumes.."]

checkValidFormsDesc = foldlSent [refS validForms, S "assumes.."]

checkValidEqnsDesc  = foldlSent [refS validEqns, S "assumes.."]