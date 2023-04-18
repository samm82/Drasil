module Drasil.ChemCode.Requirements (funcReqs, nonfuncReqs) where

import Language.Drasil hiding (matrix)
import Drasil.DocLang.SRS (propCorSol, userChar)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations (lund2023)
import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Documentation (assumption, code, environment,
  funcReqDom, likelyChg, mg, mis, module_, nonFuncReqDom, output_, property,
  requirement, srs, traceyMatrix, unlikelyChg, vav, vavPlan, propOfCorSol)
import Data.Drasil.Concepts.Math (matrix)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.ChemCode.Concepts (progName)
import Drasil.ChemCode.Quantities (maintainFrac)
import Drasil.ChemCode.IMods (chemEqIntLinProg)


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
nonfuncReqs = [accurate, verifiable, understandable, usable, reusable,
  maintainable, portable]

-- correct :: ConceptInstance
-- correct = cic "correct" (foldlSent [
--   atStartNP' (output_ `the_ofThePS` code), S "have the",
--   plural property, S "described in", namedRef (propCorSol [] []) (titleize' propOfCorSol)
--   ]) "Correct" nonFuncReqDom

accurate :: ConceptInstance
accurate = cic "accurate" (foldlSent [atStart chemical, plural equation,
  S "are only useful if they are balanced", refS lund2023 `sC`
    S "so computed coefficients from", refS chemEqIntLinProg,
    S "should be exact"]) "Accurate" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  atStartNP (the code), S "is tested following the",
  titleize vav, sParen (S "VnV"), S "Plan"]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "A new intended user", sParen (S "as described by" +:+
    refS (userChar ([]::[Contents]) ([]::[Section]))),
  S "is able to learn how to use", short progName, S "in an acceptable",
  S "amount of time" `sC` S "as measured by the procedure in Section 10 of",
  S "the VnV Plan"]) "Understandable" nonFuncReqDom

usable :: ConceptInstance
usable = cic "usable" (foldlSent [
  S "An intended user", sParen (S "as described by" +:+
    refS (userChar ([]::[Contents]) ([]::[Section]))), S "finds",
  short progName, S "easy to use" `sC`
    S "as measured by the procedure in Section 11 of the VnV Plan"])
  "Usable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [atStartNP (the code), S "is modularized"])
  "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The development time for any of the", plural likelyChg,
  S "will not exceed", ch maintainFrac, S "of the original development time"])
  "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  atStartNP (the code), S "is able to be run on systems with the",
  S "corresponding programming language installed" `sC` S "including" +:+.
  S "systems running on Windows or macOS", S"The tests from the VnV Plan",
  S "should pass in these environments"])
  "Portable" nonFuncReqDom
