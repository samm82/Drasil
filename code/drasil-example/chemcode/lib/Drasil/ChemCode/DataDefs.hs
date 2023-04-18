module Drasil.ChemCode.DataDefs where

import Control.Lens ((^.))

import Language.Drasil
import Theory.Drasil

import Data.Drasil.Citations (elemListWiki, smithChemSpec)
import Data.Drasil.Concepts.Chemistry (chemical, compound, element, equation,
  reaction)
import Data.Drasil.Concepts.Documentation (output_)

import Drasil.ChemCode.Quantities (compT, count, elemT, genC, genE, genR, genX,
  genY, reacT, tupC, elems)

dds :: [DataDefinition]
dds = [elementDD, compoundDD, reactionDD, countDD, elemsDD]

elementDD :: DataDefinition
elementDD = ddME elementExpr [dRef smithChemSpec] Nothing "elementType"
  [foldlSent [S "A type representing each", phrase element, S "from",
    refS elemListWiki]]
-- FIXME: what's a good UID for this? Is "elementType" different enough from "elemT"?

elementExpr :: ModelQDef
elementExpr = mkQuantDef elemT $ space (elemT ^. typ) -- FIXME: This is probably a hack

compoundDD :: DataDefinition
compoundDD = ddMENoRefs compoundExpr Nothing "compoundType"
  [S "A type representing a" +:+. phrase compound]
-- FIXME: what's a good UID for this?

compoundExpr :: ModelQDef
compoundExpr = mkQuantDef compT $ space (compT ^. typ) -- FIXME: This is probably a hack

reactionDD :: DataDefinition
reactionDD = ddMENoRefs reactionExpr Nothing "reactionType"
  [S "A type representing a" +:+. phrase reaction]
-- FIXME: what's a good UID for this?

reactionExpr :: ModelQDef
reactionExpr = mkQuantDef reacT $ space (reacT ^. typ) -- FIXME: This is probably a hack

countDD :: DataDefinition
countDD = ddMENoRefs countExpr Nothing "countFunc"
  [foldlSent [S "The", phrase output_, S "represents the number of atoms of a given",
    phrase element, ch genE, sParen (S "of type" +:+ eS (space $ genE ^. typ)), S "in a given",
    phrase compound, ch genC, sParen (S "of type" +:+ eS (space $ genC ^. typ))]]

countExpr :: ModelQDef
countExpr = mkFuncDefByQ count [genE, genC]
  $ completeCase [(access (sy tupC) "count" `suchThat` (access (sy tupC) "elem" $= sy genE),
                    exists [tupC] $ isMember (sy tupC) (sy genC) $&&
                     (access (sy tupC) "elem" $= sy genE)),
                  (int 0, not_ $ exists [tupC] $  isMember (sy tupC) (sy genC) $&&
                    (access (sy tupC) "elem" $= sy genE))]

elemsDD :: DataDefinition
elemsDD = ddMENoRefs elemsExpr Nothing "elemsFunc"
  [foldlSent [S "The", phrase output_, S "represents the set of", plural element,
    S "that occur in a given", phrase chemical, phrase equation, ch genR,
    sParen (S "of type" +:+ eS (space $ genR ^. typ))]]

elemsExpr :: ModelQDef
elemsExpr = mkFuncDefByQ elems [genR]
  $ setComp genE $ exists [genC, genY, genX] $
    (isMember (tCons (map sy [genC, genY])) (access (sy genR) "prod") $|| --FIXME: this should use union, not or
    isMember (tCons (map sy [genC, genY])) (access (sy genR) "reac")) $&&
    isMember (tCons (map sy [genE, genX])) (sy genC)
