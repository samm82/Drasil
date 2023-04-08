module Drasil.ChemCode.DataDefs where

import Language.Drasil
import Theory.Drasil

import Data.Drasil.Citations (elemListWiki, smithChemSpec)
import Data.Drasil.Concepts.Chemistry (element)

import Drasil.ChemCode.Quantities (elemT)

dds :: [DataDefinition]
dds = [elementDD]

elementDD :: DataDefinition
elementDD = ddE elementExpr [dRef smithChemSpec] Nothing "elementType"
  [S "A type representing each" +:+ phrase element +:+ S "from" +:+.
    refS elemListWiki]
-- FIXME: what's a good UID for this? Is "elementType" different enough from "elemT"?

elementExpr :: QDefinition Expr
elementExpr = mkQuantDef elemT $ sy elemT