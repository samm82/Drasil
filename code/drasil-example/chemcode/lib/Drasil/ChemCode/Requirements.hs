module Drasil.ChemCode.Requirements (funcReqs, nonfuncReqs) where

import Language.Drasil hiding (matrix)
import Drasil.DocLang.SRS (propCorSol, userChar)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (assumption, code, environment,
  funcReqDom, likelyChg, mg, mis, module_, nonFuncReqDom, output_, property,
  requirement, srs, traceyMatrix, unlikelyChg, vav, vavPlan, propOfCorSol)
import Data.Drasil.Concepts.Math (matrix)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.ChemCode.Concepts (progName)


{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [convertMatrix, feasOut, infeasOut]

convertMatrix, feasOut, infeasOut :: ConceptInstance

convertMatrix = cic "convertMatrix" convertMatrixDesc "Convert-to-Matrix" funcReqDom
feasOut       = cic "feasOut"       feasOutDesc       "Feasible-Output"   funcReqDom
infeasOut     = cic "infeasOut"     infeasOutDesc     "Infeasible-Output" funcReqDom

convertMatrixDesc, feasOutDesc, infeasOutDesc :: Sentence

convertMatrixDesc = foldlSent [S "Convert the inputted", phrase chemical,
  phrase equation, S "to", phrase matrix, S "form"]

feasOutDesc = foldlSent [S "If the inputted", phrase chemical, phrase equation,
  S "is feasible" `sC` S "output its balanced form with the smallest positive",
  S "integer coefficients possible"]

infeasOutDesc = foldlSent [S "If the inputted", phrase chemical, phrase equation,
  S "is infeasible" `sC` S "output a descriptive message"] 

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [accurate, verifiable, understandable, reusable, portable]
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
  atStartNP (the code), S "is tested following the",
  titleize vav, sParen (S "VnV"), S "Plan"]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "A new intended user", sParen (S "as described by" +:+
    refS (userChar ([]::[Contents]) ([]::[Section]))),
  S "should be able to learn how to use", short progName, S "in an acceptable",
  S "amount of time" `sC` S "as measured by the procedure in Section 10 of",
  S "the VnV Plan"]) "Understandable" nonFuncReqDom

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