module Drasil.ChemCode.TMods where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil

import Drasil.ChemCode.Quantities (cVec, xVec)

tms :: [TheoryModel]
tms = [intLinProg]

-- intLinProg :: TheoryModel
-- intLinProg = tm intLinProgMK
--   [] ([] :: [ConceptChunk])
--   [] [express intLinProgFD] [] [] "intLinProg" intLinProgNotes

-- intLinProgMK :: ModelKind Expr
-- intLinProgMK = equationalModel "intLinProgTM"
--   (nounPhraseSP "Integer linear program") intLinProgFD

-- intLinProgFD :: SimpleQDef
-- intLinProgFD = mkFuncDefByQ lO [lR] intLinProgExpr

-- intLinProgExpr :: Expr
-- intLinProgExpr = sy cVec `mulRe` sy xVec

intLinProg :: TheoryModel
intLinProg = tmNoRefs
  (equationalConstraints' ilpCS)
  ([] :: [QuantityDict]) -- FIXME: I should not need to manually define the type signature for this to type-check.
  [ilpChunk] -- FIXME: Why do I need this?
  []
  [ilpRel] -- FIXME: I should not need to manually reference ilpRel twice.
  []
  "intLinProg"
  [] -- FIXME: NOTES!
  where
    ilpChunk =
      dccWDS "ilpChunk" (nounPhraseSP "integer linear program") (S "") -- FIXME: ?

    ilpRel :: ModelExpr
    ilpRel = sy cVec `mulRe` sy xVec

    ilpCS = mkConstraintSet ilpChunk $ NE.fromList [ilpRel]