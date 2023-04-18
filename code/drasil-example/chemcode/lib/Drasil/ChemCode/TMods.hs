module Drasil.ChemCode.TMods where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil

import Data.Drasil.Citations (ilpWiki, lund2023)
import Data.Drasil.Concepts.Chemistry (chemical, reaction)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Software (program)

import Drasil.ChemCode.Quantities (aMat, bVec, cVec, xVec, zeroVec, genE, genI,
  genR, count)

balanced :: ConceptChunk
balanced = dccWDS "balanced" (cn' "balanced")
  (sParen (S "Referring to a" +:+ phrase chemical +:+ phrase equation) +:+
    S "following the Law of Conservation of Matter" +:+ sParen (refS lawConsMass))
-- This should really be in Concept.hs, but this causes an import cycle

tms :: [TheoryModel]
tms = [intLinProg, lawConsMass]

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

lawConsMass :: TheoryModel
lawConsMass = tm
  (equationalModel "lawConsMass" (cn' "law of conservation of mass")
    $ fromEqn' "" (cn "") EmptyS (label "Law of Conservation of Mass") Real
    consMassExpr)
  ([] :: [QuantityDict]) -- FIXME: I should not need to manually define the type signature for this to type check
  [consMassChunk] -- FIXME: why do I need this?
  []
  [consMassExpr] -- FIXME: apparently this is needed since generation doesn't happen from ModelKinds
  []
  [dRef lund2023]
  "lawConsMass" -- FIXME: this is likely needed for the same ModelKinds reason
  [foldlSent [S "This law states that", Quote (foldlSent_ [
      S "matter can neither be created nor destroyed in a", phrase chemical,
      phrase reaction, S "... but it may change forms to other substances"]),
    complexRef lund2023 (Page [112])],
   foldlSent [S "The above", phrase equation, S "assumes that", ch genR,
      S "is", phrase balanced]
  ]
  where
    consMassChunk =
      dccWDS "consMassChunk" (nounPhraseSP "law of conservation of mass") (S "") -- FIXME: ?
    consMassExpr = forall [genE, genR]
      (sideOfConsMassExpr "reac" $= sideOfConsMassExpr "prod")
    sideOfConsMassExpr s = sumAll (eqSymb genI) (
      apply count [sy genE, access (idx (sy genR) (sy genI)) "comp"] `mulRe`
      access (idx (access (sy genR) s) (sy genI)) "coeff"
      )
