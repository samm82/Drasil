module Drasil.ChemCode.DataDefs where

import Control.Lens ((^.))

import Language.Drasil
import Theory.Drasil

import Data.Drasil.Citations (elemListWiki, smithChemSpec)
import Data.Drasil.Concepts.Chemistry (compound, element)

import Drasil.ChemCode.Quantities (compT, elemT)

dds :: [DataDefinition]
dds = [elementDD, compoundDD]

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