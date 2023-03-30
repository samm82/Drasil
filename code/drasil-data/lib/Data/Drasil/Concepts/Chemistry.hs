-- | Defines concepts used in the field of chemistry.
module Data.Drasil.Concepts.Chemistry where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.

import Prelude hiding (product)
import Language.Drasil

import Data.Drasil.Citations (lund2023)
-- import Data.Drasil.Concepts.Documentation (property, value)
-- import Data.Drasil.Domains (chemistry)

-- | Collects all chemistry-related concepts.
chemCon :: [ConceptChunk]
chemCon = [chemical, chemistry, compound, element, equation, product,
  reactant, reaction]

-- * Chemistry Concepts
--
-- $ChemCon
--
-- In alphabetical order.

chemical, chemistry, compound, element, equation, product, reactant,
  reaction :: ConceptChunk

chemical = dccWDS "chemical" (cn' "chemical")
  (S "relating to" +:+ phrase chemistry)
chemistry = dcc "chemistry" (cn' "chemistry")
  "the study of the properties and behavior of matter" -- source: Wikipedia
compound = dccWDS "compound" (cn' "compound")
  (S "a molecule made up of more than one atom" `sC`
    S "which may or may not be of different" +:+ plural element)
element = dcc "element" (cn' "element")
  ("the group of all atoms with the same number of protons in the atomic nucleus. " ++
    "For example, all atoms with one proton are hydrogen atoms") -- FIXME: add source
equation = dccWDS "equation" (cn' "equation")
  (S "a textual representation of a" +:+ phrase chemical +:+ phrase reaction)
product = dccWDS "product" (cn' "product")
  (S "a substance formed by a" +:+ phrase chemical +:+ phrase reaction)
reactant = dccWDS "reactant" (cn' "reactant")
  (S "a substance involved in and changed by a" +:+ phrase chemical +:+
    phrase reaction)
reaction = dccWDS "reaction" (cn' "reaction")
  (S "an interaction between different types of matter that results in at" +:+
    S "least one new substance being formed" +:+ refS lund2023)
