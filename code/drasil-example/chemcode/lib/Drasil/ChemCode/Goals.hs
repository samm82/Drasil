module Drasil.ChemCode.Goals (goals) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

goals :: [ConceptInstance]
goals = [findBalancedForm] --, outputInfeasMsg]

findBalancedForm :: ConceptInstance
findBalancedForm = cic "findBalancedForm" (S "Find its balanced" +:+.
    S "form with the smallest positive integer coefficients if it is feasible")
  "findBalancedForm" goalStmtDom

-- outputInfeasMsg :: ConceptInstance
-- outputInfeasMsg = cic "outputInfeasMsg" (atStart output_ +:+.
--     S "a descriptive message if it is infeasible")
--   "outputInfeasMsg" goalStmtDom