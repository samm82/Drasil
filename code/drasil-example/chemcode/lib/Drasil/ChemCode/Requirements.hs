module Drasil.ChemCode.Requirements (funcReqs, nonfuncReqs) where

import Language.Drasil hiding (matrix)
import Drasil.DocLang.SRS (propCorSol)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (assumption, code, environment,
  funcReqDom, likelyChg, mg, mis, module_, nonFuncReqDom, output_, property,
  requirement, srs, traceyMatrix, unlikelyChg, vavPlan, propOfCorSol)
import Data.Drasil.Concepts.Math (matrix)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)


{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [inputFormula, convertMatrix]

inputFormula, convertMatrix :: ConceptInstance

inputFormula  = cic "inputFormula"  inputFormulaDesc  "Input-Formula"     funcReqDom
convertMatrix = cic "convertMatrix" convertMatrixDesc "Convert-to-Matrix" funcReqDom

inputFormulaDesc, convertMatrixDesc :: Sentence

inputFormulaDesc = foldlSent [S "Input a representation of a", phrase chemical,
  phrase equation]
convertMatrixDesc = foldlSent [S "Convert the inputted", phrase chemical,
  phrase equation, S "to", phrase matrix, S "form"]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [accurate, verifiable, reusable, portable]
-- nonfuncReqs = [accurate, verifiable, understandable, reusable, maintainable, portable]

-- correct :: ConceptInstance
-- correct = cic "correct" (foldlSent [
--   atStartNP' (output_ `the_ofThePS` code), S "have the",
--   plural property, S "described in", namedRef (propCorSol [] []) (titleize' propOfCorSol)
--   ]) "Correct" nonFuncReqDom

accurate :: ConceptInstance
accurate = cic "accurate" (foldlSent [atStart chemical, plural equation,
  S "are only useful if they are balanced" `sC`
    S "so computed coefficients should be exact"]) "Accurate" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  atStartNP (the code), S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

-- understandable :: ConceptInstance
-- understandable = cic "understandable" (foldlSent [
--   atStartNP (the code), S "is modularized with complete",
--   phraseNP (mg `and_` mis)]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [atStartNP (the code), S "is modularized"])
  "Reusable" nonFuncReqDom

-- maintainable :: ConceptInstance
-- maintainable = cic "maintainable" (foldlSent [
--   S "The traceability between", foldlList Comma List [plural requirement,
--   plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
--   plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
--   plural traceyMatrix, S "in the", getAcc srs `S.and_` phrase mg]) "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  atStartNP (the code), S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom