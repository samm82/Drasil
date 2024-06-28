module Drasil.ChemCode.Changes where

import Prelude hiding (product)

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations (inorganicIUPAC, lund2023, nonIntCoeffSource,
  organicIUPAC)
import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (assumption, likeChgDom, unlikeChgDom)
import Data.Drasil.SI_Units (mole)

import Drasil.ChemCode.Assumptions
import Drasil.ChemCode.Concepts (progName)

-- LIKELY CHANGES

lChanges :: [ConceptInstance]
lChanges = [complexForms, calcMoles, detLimReag, calcYield, calcExcess,
  incInputFormat, termsOfMass, classReacs, classMoreReacs, inputPhaseLabels,
  findPhaseLabels, reacTakePlace]

complexForms, calcMoles, detLimReag, calcYield, calcExcess, incInputFormat,
  termsOfMass, classReacs, classMoreReacs, inputPhaseLabels, findPhaseLabels,
  reacTakePlace :: ConceptInstance
complexForms     = cic "complexForms"     complexFormsDesc     "complexForms"     likeChgDom
calcMoles        = cic "calcMoles"        calcMolesDesc        "calcMoles"        likeChgDom
detLimReag       = cic "detLimReag"       detLimReagDesc       "detLimReag"       likeChgDom
calcYield        = cic "calcYield"        calcYieldDesc        "calcYield"        likeChgDom
calcExcess       = cic "calcExcess"       calcExcessDesc       "calcExcess"       likeChgDom
incInputFormat   = cic "incInputFormat"   incInputFormatDesc   "incInputFormat"   likeChgDom
termsOfMass      = cic "termsOfMass"      termsOfMassDesc      "termsOfMass"      likeChgDom
classReacs       = cic "classReacs"       classReacsDesc       "classReacs"       likeChgDom
classMoreReacs   = cic "classMoreReacs"   classMoreReacsDesc   "classMoreReacs"   likeChgDom
inputPhaseLabels = cic "inputPhaseLabels" inputPhaseLabelsDesc "inputPhaseLabels" likeChgDom
findPhaseLabels  = cic "findPhaseLabels"  findPhaseLabelsDesc  "findPhaseLabels"  likeChgDom
reacTakePlace    = cic "reacTakePlace"    reacTakePlaceDesc    "reacTakePlace"    likeChgDom

complexFormsDesc, calcMolesDesc, detLimReagDesc, calcYieldDesc, calcExcessDesc,
  incInputFormatDesc, termsOfMassDesc, classReacsDesc, classMoreReacsDesc,
  inputPhaseLabelsDesc, findPhaseLabelsDesc, reacTakePlaceDesc :: Sentence
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
    dependentOn calcMoles
  ])
  -- TODO: from lund2023

calcYieldDesc = likeChgGivenHelper amountMultReac (foldlSent_ [
    S "calculate the theoretical yield of each", phrase product +:+. alsoInMoles,
    dependentOn detLimReag
  ])
  -- TODO: from lund2023

calcExcessDesc = likeChgGivenHelper amountMultReac (foldlSent_ [
    S "calculate the amount of excess", phrase reactant :+: sParen (S "s") +:+.
    alsoInMoles, dependentOn detLimReag
  ])
  -- TODO: from lund2023

incInputFormatDesc = foldlSent [refS correctInputFormat, S "assumes that inputted",
  phrase chemical, S "formulas follow the conventions laid out in" +:+.
  (refS inorganicIUPAC `S.and_` refS organicIUPAC), S "In the future" `sC`
    short progName, S "might be able to parse valid inputted", phrase chemical,
  S "formulas that do not follow these conventions and format them correctly",
  S "when outputting them"
  ]

termsOfMassDesc = foldlSent [actorMightBeAbleTo (S "the user"),
  S "enter the amounts required by", foldlList Comma List $ map refS [
    calcMoles, detLimReag, calcYield, calcExcess], S "in terms of mass",
    sParen (S "e.g., in grams") -- FIXME: define 'gram'
  ]
  -- TODO: from lund2023

classReacsDesc = foldlSent [actorMightBeAbleTo (short progName),
  S "classify a", phrase chemical, phrase reaction, S "as",
  Quote (S "combination (or synthesis), decomposition, combustion, single" +:+
    S "replacement, [or] double replacement"), complexRef lund2023 $ Page [301]
    -- FIXME: "or" in square brackets with foldlList?
  ]
  -- TODO: from lund2023

classMoreReacsDesc = foldlSent [actorMightBeAbleTo (short progName),
  S "classify", foldlList Comma List (map (\x -> S x +:+ plural reaction) [
    "oxidation-reduction", "... acid-base", "condensation"]) +:+.
    complexRef lund2023 (Page [301]), S "This should be done after",
    refS classReacs
  ]
  -- TODO: from lund2023

inputPhaseLabelsDesc = actorMightBeAbleTo (S "the user") +:+. S "input phase labels"
  -- TODO: from lund2023


findPhaseLabelsDesc = likeChgGivenHelper (foldlSent_ [S "the phase labels for the",
  plural reactant, S "of a", phrase chemical, phrase reaction]) (foldlSent_ [
    S "identify the phase labels for the", plural product `sC` S "which would" +:+.
    S "involve determining solubility", dependentOn inputPhaseLabels
  ])
    -- TODO: from lund2023

reacTakePlaceDesc = foldlSent [actorMightBeAbleTo (short progName),
  S "identify when a", phrase chemical, phrase reaction +:+. S "will not take place",
  dependentOn findPhaseLabels
  ]
  -- TODO: from lund2023

-- HELPERS FOR LIKELY CHANGES

inMolesInReac, alsoInMoles, amountMultReac :: Sentence
inMolesInReac = sParen (S "in" +:+ plural mole) +:+ S "in a" +:+ phrase reaction
alsoInMoles  = sParen (S "also in" +:+ plural mole)
amountMultReac = S "the amount of more than one" +:+ phrase reactant +:+ inMolesInReac

likeChgGivenHelper :: Sentence -> Sentence -> Sentence
likeChgGivenHelper given action = actorMightBeAbleTo (short progName) `sC`
  S "given" +:+. (given `sC` action)

actorMightBeAbleTo :: Sentence -> Sentence
actorMightBeAbleTo actor = S "In the future" `sC` actor +:+ S "might be able to"

dependentOn :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence
dependentOn r = S "This is dependent on" +:+ refS r

-- UNLIKELY CHANGES

uChanges :: [ConceptInstance]
uChanges = [allEqsPermitted, checkValidForms, checkValidEqns, nonintCoeffs]

allEqsPermitted, checkValidForms, checkValidEqns, nonintCoeffs :: ConceptInstance
allEqsPermitted = cic "allEqsPermitted" allEqsPermittedDesc "allEqsPermitted" unlikeChgDom
checkValidForms = cic "checkValidForms" checkValidFormsDesc "checkValidForms" unlikeChgDom
checkValidEqns  = cic "checkValidEqns"  checkValidEqnsDesc  "checkValidEqns"  unlikeChgDom
nonintCoeffs    = cic "nonintCoeffs"    nonintCoeffsDesc    "nonintCoeffs"    unlikeChgDom

allEqsPermittedDesc, checkValidFormsDesc, checkValidEqnsDesc, nonintCoeffsDesc :: Sentence

allEqsPermittedDesc = foldlSent [refS elemCompDiff, S "assumes that for all",
  S "inputted", phrase chemical, plural equation `sC` S "there is at most one more",
  phrase compound, S "than" +:+. phrase element, S " Manual empirical analysis of",
  phrase chemical, plural equation, S "without this constraint led to unexpected",
  S "results" `sC` S "and preliminary investigation did not find any proof that these",
  S "types of", plural reaction +:+. S "never take place", S "Therefore" `sC`
    S "this constraint will remain an", phrase assumption,
  S "and is unlikely to be relaxed"]

checkValidFormsDesc = databaseHelper validForms (S "formulas")    (plural compound)
checkValidEqnsDesc  = databaseHelper validEqns  (plural equation) (plural reaction)

nonintCoeffsDesc = foldlSent [refS intCoeffs, S "assumes that all inputted",
  phrase chemical, plural equation +:+. S "will be balanced using integer coefficients",
  S "While there are some cases where these coefficients are not integers",
  sParen (S "see" +:+ refS nonIntCoeffSource) `sC` S "the overwhelming majority of",
  phrase chemical, plural equation, S "are balanced using integer coefficients" `sC`
    S "since this can represent the numbers of molecules present in a" +:+.
    phrase reaction, S "Therefore" `sC` S "only integer coefficients will be used",
  S "for balancing", phrase chemical, plural equation]

-- HELPERS FOR UNLIKELY CHANGES

databaseHelper :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence
  -> Sentence -> Sentence
databaseHelper source a b = foldlSent [refS source, S "assumes that all",
  S "inputted", phrase chemical, a, S "describe real", phrase chemical +:+. b,
  S "While there are extensive libraries of", phrase chemical, b `sC`
    S "it is impossible to verify that any database of", phrase chemical,
  b +:+. S "is complete for the sake of verifying user input",
  S "Therefore" `sC` S "it is unlikely that there will be a reliable way of",
  S "ensuring the user only inputs real", phrase chemical, a]