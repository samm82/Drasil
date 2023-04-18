module Drasil.ChemCode.Changes where

import Prelude hiding (product)

import Language.Drasil

import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (likeChgDom, unlikeChgDom)

import Drasil.ChemCode.Assumptions
import Drasil.ChemCode.Concepts (progName)

lChanges :: [ConceptInstance]
lChanges = [complexForms, identifyPhaseLabels]

complexForms, identifyPhaseLabels :: ConceptInstance
complexForms = cic "complexForms"      complexFormsDesc      "complexForms"      likeChgDom
identifyPhaseLabels = cic "identifyPhaseLabels" identifyPhaseLabelsDesc "identifyPhaseLabels" likeChgDom

complexFormsDesc, identifyPhaseLabelsDesc :: Sentence
complexFormsDesc = foldlSent [refS simpleForms, S "assumes that inputted",
  phrase chemical +:+. S "formulas only consist of atomic symbols and subscripts",
  S "In the future" `sC` S "the user might be able to input more complex",
  phrase chemical, S "formulas" `sC` S "such as those containing", 
  foldlList Comma Options (map S ["dots", "parentheses", "hyphens",
  "superscripts"]) -- FIXME: and/or support?
  ]

identifyPhaseLabelsDesc = likeChgGivenHelper (foldlSent_ [S "the phase labels for the",
  plural reactant, S "of a", phrase chemical, phrase reaction]) (foldlSent_ [
    S "identify the phase labels for the", plural product `sC` S "which would",
    S "involve determining solubility"])

likeChgGivenHelper :: Sentence -> Sentence -> Sentence
likeChgGivenHelper given action = foldlSent [S "In the future" `sC` short progName,
  S "might be able to" `sC` S "given", given `sC` action]

-- LC2: In the future, ChemCode might be able to, given the amount of one substance (in
-- moles) in a reaction, calculate the amount required/produced of every other substance
-- (also in moles) in the reaction.1
-- LC3: In the future, ChemCode might be able to, given the amount of each reactant (in
-- moles) in a reaction, determine the limiting reactant(s).1 This is dependent on LC2.
-- LC4: In the future, ChemCode might be able to, given the amount of more than one reactant
-- (in moles) in a reaction, calculate the theoretical yield of each product (also in moles).1
-- This is dependent on LC3.
-- LC5: In the future, ChemCode might be able to, given the amount of more than one reactant
-- (in moles) in a reaction, calculate the amount of excess reactant(s) (also in moles).1
-- This is dependent on LC3.
-- LC6: The system currently assumes that inputted chemical formulas follow the conventions
-- laid out in [23] and [24]. In the future, ChemCode might be able to parse valid inputted
-- chemical formulas that do not follow these conventions and format them correctly when
-- outputting them.
-- LC7: In the future, the user might be able to enter the amounts required by LC2, LC3, LC4,
-- and LC5 in terms of mass (e.g., in grams).1
-- LC8: In the future, ChemCode might be able to classify a chemical reaction as “combination
-- (or synthesis), decomposition, combustion, single replacement, [or] double replacement” [9, p. 301].1
-- LC9: In the future, ChemCode might also be able to classify “oxidation-reduction reactions,
-- . . . acid-base reactions, and condensation reactions” [9, p. 301].1 This should be done
-- after LC8.
-- LC10: In the future, ChemCode might allow the user to input phase labels.1
-- LC11: In the future, ChemCode might be able to, given the phase labels for the reactants of
-- a reaction, identify the phase labels for the products, which would involve determining
-- solubility.1 This is dependent on LC10.
-- LC12: In the future, ChemCode might be able to identify when a reaction will not take place.1
-- This is dependent on LC11.

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