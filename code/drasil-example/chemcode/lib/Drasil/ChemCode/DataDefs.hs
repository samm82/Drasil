module Drasil.ChemCode.DataDefs where

import Control.Lens ((^.))

import Language.Drasil
import Theory.Drasil

import Data.Drasil.Citations (elemListWiki, smithChemSpec)
import Data.Drasil.Concepts.Chemistry (compound, element, reaction)
import Data.Drasil.Concepts.Documentation (output_)

import Drasil.ChemCode.Quantities (compT, count, elemT, genC, genE, reacT, tupC)

dds :: [DataDefinition]
dds = [countDD, elementDD, compoundDD, reactionDD]

countDD :: DataDefinition
countDD = ddMENoRefs countExpr Nothing "countFunc"
  [foldlSent_ [S "The", phrase output_, S "represents the number of atoms of a given",
    phrase element, ch genE, sParen (S "of type" +:+ eS (space $ genE ^. typ)), S "in a given",
    phrase compound, ch genC, sParen (S "of type" +:+ eS (space $ genC ^. typ))]]

countExpr :: ModelQDef
countExpr = mkFuncDefByQ count [genE, genC]
  $ completeCase [(access tupC 2, isMember (sy tupC) (sy genC) $&& (access tupC 1 $= sy genE)),
                  (int 0, not_ $ isMember (sy tupC) (sy genC) $&& (access tupC 1 $= sy genE))]

elementDD :: DataDefinition
elementDD = ddME elementExpr [dRef smithChemSpec] Nothing "elementType"
  [S "A type representing each" +:+ phrase element +:+ S "from" +:+
    refS elemListWiki]
-- FIXME: what's a good UID for this? Is "elementType" different enough from "elemT"?

elementExpr :: ModelQDef
elementExpr = mkQuantDef elemT $ space (elemT ^. typ) -- FIXME: This is probably a hack

compoundDD :: DataDefinition
compoundDD = ddMENoRefs compoundExpr Nothing "compoundType"
  [S "A type representing a" +:+ phrase compound]
-- FIXME: what's a good UID for this?

compoundExpr :: ModelQDef
compoundExpr = mkQuantDef compT $ space (compT ^. typ) -- FIXME: This is probably a hack

reactionDD :: DataDefinition
reactionDD = ddMENoRefs reactionExpr Nothing "reactionType"
  [S "A type representing a" +:+ phrase reaction]
-- FIXME: what's a good UID for this?

reactionExpr :: ModelQDef
reactionExpr = mkQuantDef reacT $ space (reacT ^. typ) -- FIXME: This is probably a hack