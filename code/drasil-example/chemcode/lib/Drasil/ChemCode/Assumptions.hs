module Drasil.ChemCode.Assumptions where

import Language.Drasil

import Data.Drasil.Citations (inorganicIUPAC, organicIUPAC)
import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (assumpDom)

assumps :: [ConceptInstance]
assumps = [elemCompDiff, validForms, validEqns, correctInputFormat, simpleForms]

elemCompDiff, validForms, validEqns, correctInputFormat, simpleForms :: ConceptInstance
elemCompDiff       = cic "elemCompDiff"       elemCompDiffDesc       "elemCompDiff"       assumpDom
validForms         = cic "validForms"         validFormsDesc         "validForms"         assumpDom
validEqns          = cic "validEqns"          validEqnsDesc          "validEqns"          assumpDom
correctInputFormat = cic "correctInputFormat" correctInputFormatDesc "correctInputFormat" assumpDom
simpleForms        = cic "simpleForms"        simpleFormsDesc        "simpleForms"        assumpDom

elemCompDiffDesc, validFormsDesc, validEqnsDesc, correctInputFormatDesc,
  simpleFormsDesc :: Sentence

elemCompDiffDesc = foldlSent [S "For all", phrase chemical, plural equation `sC`
  S "there is at most one more", phrase compound, S "than", phrase element]

validFormsDesc = foldlSent [S "All inputted", phrase chemical,
  S "formulas describe real", phrase chemical, plural compound]

validEqnsDesc = foldlSent [S "All inputted", phrase chemical, plural equation,
  S "describe real", phrase chemical, plural reaction]

correctInputFormatDesc = foldlSent [S "All inputted", phrase chemical,
  S "formulas follow the conventions laid out in", foldlList Comma List
    (map refS [inorganicIUPAC, organicIUPAC]), S "by the", introduceAbb iupac]

simpleFormsDesc = foldlSent [S "All inputted", phrase chemical +:+.
  S "formulas only consist of atomic symbols and subscripts",
  S "This means that they cannot contain", foldlList Comma Options
    (map S ["dots", "parentheses", "hyphens", "superscripts"]) `sC` S "so",
    foldlList Comma List [plural hydrate, plural polymer,
      S "formulas with" +:+ plural isotope +:+ S "and some with" +:+ plural polyIon],
  S "will not be present in inputted", phrase chemical, plural equation]