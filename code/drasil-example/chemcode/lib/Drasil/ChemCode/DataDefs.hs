module Drasil.ChemCode.DataDefs where

import Language.Drasil
import Theory.Drasil

import Drasil.ChemCode.Quantities (elemT)

dds :: [DataDefinition]
dds = [elementDD]

elementDD :: DataDefinition
elementDD = ddENoRefs elementExpr Nothing "elementType" []
-- FIXME: what's a good UID for this? Is this different enough from "elemT"?

elementExpr :: QDefinition Expr
elementExpr = mkQuantDef elemT $ sy elemT