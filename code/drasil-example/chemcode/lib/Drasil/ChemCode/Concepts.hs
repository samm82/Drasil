module Drasil.ChemCode.Concepts where

import Language.Drasil

import Data.Drasil.Concepts.Chemistry

progName :: CI -- name of example
progName = commonIdeaWithDict "progName" (pn "Chemistry Code") "ChemCode" []

balanced :: ConceptChunk
balanced = dccWDS "balanced" (cn' "balanced")
  (sParen (S "Referring to a" +:+ phrase chemical +:+ phrase equation) +:+
    S "following the Law of Conservation of Matter")
  -- FIXME: add reference to TM2
