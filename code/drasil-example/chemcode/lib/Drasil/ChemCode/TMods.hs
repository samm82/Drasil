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
  (ilpModel "canonIntLinProg" (cn' "canonical integer linear program")
    $ maxILP xVec cVec
      $ sy aMat `mulRe` sy xVec $<= sy bVec NE.:| [sy xVec $>= sy zeroVec, isIn (sy xVec) (Vect Integer)])
  ([] :: [QuantityDict]) -- FIXME: I should not need to manually define the type signature for this to type check
  [ilpChunk] -- FIXME: why do I need this?
  []
  [sy cVec `mulRe` sy xVec,
   sy aMat `mulRe` sy xVec $<= sy bVec,
   sy xVec $>= sy zeroVec,
   isIn (sy xVec) (Vect Integer)] -- FIXME: apparently this is needed since generation doesn't happen from ModelKinds
  []
  [dRef ilpWiki]
  "canonIntLinProg" -- FIXME: this is likely needed for the same ModelKinds reason
  [foldlSent [S "The above", phrase equation, -- FIXME: can this be called an "equation"?
    S "gives the canonical form of an integer linear",
    phrase program `sC` S "which is", Quote (foldlSent_ [
      S "a mathematical optimization or feasibility", phrase program,
      S "in which some or all of the variables are restricted to be integers",
      S "[and] the objective function and the constraints", 
      sParen (S "other than the integer constraints"), S "are linear"]),
    refS ilpWiki],
  S "The values of" +:+ ch xVec +:+. S "are unknown and will be solved for"
  ]
  where
    ilpChunk =
      dccWDS "ilpChunk" (nounPhraseSP "integer linear program") (S "") -- FIXME: ?
