module Drasil.ChemCode.Changes where

import Prelude hiding (product)

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (likeChgDom, unlikeChgDom)
import Data.Drasil.SI_Units (mole)

import Drasil.ChemCode.Assumptions
import Drasil.ChemCode.Concepts (progName)
import Data.Drasil.Citations (inorganicIUPAC, lund2023, organicIUPAC)

lChanges :: [ConceptInstance]
lChanges = [complexForms, calcMoles, detLimReag, calcYield, calcExcess,
  incorrectInputFormat, termsOfMass, classRxns, classMoreRxns, identifyPhaseLabels]

complexForms, calcMoles, detLimReag, calcYield, calcExcess, incorrectInputFormat,
  termsOfMass, classRxns, classMoreRxns, identifyPhaseLabels :: ConceptInstance
complexForms = cic "complexForms"      complexFormsDesc      "complexForms"      likeChgDom
calcMoles = cic "calcMoles"      calcMolesDesc      "calcMoles"      likeChgDom
detLimReag = cic "detLimReag"      detLimReagDesc      "detLimReag"      likeChgDom
calcYield = cic "calcYield"      calcYieldDesc      "calcYield"      likeChgDom
calcExcess = cic "calcExcess"      calcExcessDesc      "calcExcess"      likeChgDom
incorrectInputFormat = cic "incorrectInputFormat"      incorrectInputFormatDesc      "incorrectInputFormat"      likeChgDom
termsOfMass = cic "termsOfMass"      termsOfMassDesc      "termsOfMass"      likeChgDom
classRxns = cic "classRxns"      classRxnsDesc      "classRxns"      likeChgDom
classMoreRxns = cic "classMoreRxns"      classMoreRxnsDesc      "classMoreRxns"      likeChgDom
identifyPhaseLabels = cic "identifyPhaseLabels" identifyPhaseLabelsDesc "identifyPhaseLabels" likeChgDom

complexFormsDesc, calcMolesDesc, detLimReagDesc, calcYieldDesc, calcExcessDesc,
  incorrectInputFormatDesc, termsOfMassDesc, classRxnsDesc, classMoreRxnsDesc,
  identifyPhaseLabelsDesc :: Sentence
complexFormsDesc = foldlSent [refS simpleForms, S "assumes that inputted",
  phrase chemical +:+. S "formulas only consist of atomic symbols and subscripts",
  S "In the future" `sC` S "the user might be able to input more complex",
  phrase chemical, S "formulas" `sC` S "such as those containing",
  foldlList Comma Options (map S ["dots", "parentheses", "hyphens",
    "superscripts"]) -- FIXME: and/or support?
  ]

calcMolesDesc = likeChgGivenHelper (foldlSent_ [S "the amount of one substance",
  inMolesInReac]) (foldlSent_ [S "calculate the amount of every other substance",
    alsoInMoles, S "required/produced by the", phrase reaction
  ])
  -- TODO: from lund2023

detLimReagDesc = likeChgGivenHelper amountMultReac
  (foldlSent_ [
    S "determine the limiting" +:+. (phrase reactant :+: sParen (S "s")),
    S "This is dependent on", refS calcMoles
  ])
  -- TODO: from lund2023

calcYieldDesc = likeChgGivenHelper amountMultReac (foldlSent_ [
    S "calculate the theoretical yield of each", phrase product +:+. alsoInMoles,
    S "This is dependent on", refS detLimReag
  ])
  -- TODO: from lund2023

calcExcessDesc = likeChgGivenHelper amountMultReac (foldlSent_ [
    S "calculate the amount of excess", phrase reactant :+: sParen (S "s") +:+.
    alsoInMoles, S "This is dependent on", refS detLimReag
  ])
  -- TODO: from lund2023

incorrectInputFormatDesc = foldlSent [refS correctInputFormat, S "assumes that inputted",
  phrase chemical, S "formulas follow the conventions laid out in" +:+.
  (refS inorganicIUPAC `S.and_` refS organicIUPAC), S "In the future" `sC` short progName,
  S "might be able to parse valid inputted", phrase chemical,
  S "formulas that do not follow these conventions and format them correctly when outputting them"
  ]

termsOfMassDesc = foldlSent [actorMightBeAbleTo (S "the user"),
  S "enter the amounts required by", foldlList Comma List $ map refS [
    calcMoles, detLimReag, calcYield, calcExcess], S "in terms of mass",
    sParen (S "e.g., in grams") -- FIXME: define 'gram'
  ]
  -- TODO: from lund2023

classRxnsDesc = foldlSent [actorMightBeAbleTo (short progName),
  S "classify a", phrase chemical, phrase reaction, S "as",
  Quote (S "combination (or synthesis), decomposition, combustion, single" +:+
    S "replacement, [or] double replacement"), complexRef lund2023 $ Page [301]
    -- FIXME: "or" in square brackets with foldlList?
  ]
  -- TODO: from lund2023

classMoreRxnsDesc = foldlSent [actorMightBeAbleTo (short progName),
  S "classify", foldlList Comma List (map (\x -> S x +:+ plural reaction) [
    "oxidation-reduction", "... acid-base", "condensation"]) +:+.
    complexRef lund2023 (Page [301]), S "This should be done after",
    refS classRxns
  ]
  -- TODO: from lund2023

identifyPhaseLabelsDesc = likeChgGivenHelper (foldlSent_ [S "the phase labels for the",
  plural reactant, S "of a", phrase chemical, phrase reaction]) (foldlSent_ [
    S "identify the phase labels for the", plural product `sC` S "which would",
    S "involve determining solubility"
  ])
    -- TODO: from lund2023
    -- TODO: dependent on LC10

inMolesInReac, alsoInMoles, amountMultReac :: Sentence
inMolesInReac = sParen (S "in" +:+ plural mole) +:+ S "in a" +:+ phrase reaction
alsoInMoles  = sParen (S "also in" +:+ plural mole)
amountMultReac = S "the amount of more than one" +:+ phrase reactant +:+ inMolesInReac

likeChgGivenHelper :: Sentence -> Sentence -> Sentence
likeChgGivenHelper given action = actorMightBeAbleTo (short progName) `sC`
  S "given" +:+. (given `sC` action)

actorMightBeAbleTo :: Sentence -> Sentence
actorMightBeAbleTo actor = S "In the future" `sC` actor +:+ S "might be able to"

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