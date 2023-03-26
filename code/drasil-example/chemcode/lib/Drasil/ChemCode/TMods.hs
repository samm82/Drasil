module Drasil.ChemCode.TMods where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil

import Data.Drasil.Citations (ilpWiki)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Software (program)

import Drasil.ChemCode.Quantities (aMat, bVec, cVec, xVec, zeroVec)

tms :: [TheoryModel]
tms = [intLinProg]

intLinProg :: TheoryModel
intLinProg = tm
  (equationalConstraints' ilpCS)
  ([] :: [QuantityDict]) -- FIXME: I should not need to manually define the type signature for this to type-check.
  [ilpChunk] -- FIXME: Why do I need this?
  []
  [ilpRel] -- FIXME: I should not need to manually reference ilpRel twice.
  []
  [dRef ilpWiki]
  "intLinProg"
  [foldlSent_ [S "The above", phrase equation, -- FIXME: can this be called an "equation"?
    S "gives the canonical form of an integer linear",
    phrase program `sC` S "which is", Quote (foldlSent_ [
      S "a mathematical optimization or feasibility", phrase program,
      S "in which some or all of the variables are restricted to be integers",
      S "[and] the objective function and the constraints", 
      sParen (S "other than the integer constraints"), S "are linear"]),
    refS ilpWiki]]
  where
    ilpChunk =
      dccWDS "ilpChunk" (nounPhraseSP "integer linear program") (S "") -- FIXME: ?

    ilpRel :: ModelExpr
    ilpRel = completeCase [
      (sy cVec `mulRe` sy xVec, lit $ int 1),
      (sy aMat `mulRe` sy xVec $<= sy bVec, lit $ int 2),
      (sy xVec $>= sy zeroVec, lit $ int 3)
      ]

    ilpCS = mkConstraintSet ilpChunk $ NE.fromList [ilpRel]