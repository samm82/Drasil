module Drasil.ChemCode.Assumptions where

import Language.Drasil

import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (assumpDom)

assumps :: [ConceptInstance]
assumps = [elemCompDiff, validForms, validEqns]

elemCompDiff, validForms, validEqns :: ConceptInstance
elemCompDiff = cic "elemCompDiff"      elemCompDiffDesc      "elemCompDiff"      assumpDom
validForms = cic "validForms"      validFormsDesc      "validForms"      assumpDom
validEqns = cic "validEqns"      validEqnsDesc      "validEqns"      assumpDom

elemCompDiffDesc, validFormsDesc, validEqnsDesc :: Sentence

elemCompDiffDesc = foldlSent [S "For all", phrase chemical, plural equation `sC`
    S "there is at most one more", phrase compound, S "than", phrase element]

validFormsDesc = foldlSent [S "All inputted", phrase chemical,
    S "formulas describe real", phrase chemical, plural compound]  

validEqnsDesc  = foldlSent [S "All inputted", phrase chemical, plural equation,
    S "describe real", phrase chemical, plural reaction]  
