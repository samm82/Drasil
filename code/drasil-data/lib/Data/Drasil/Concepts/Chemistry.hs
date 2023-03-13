-- | Defines concepts used in the field of chemistry.
module Data.Drasil.Concepts.Chemistry where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.
import Language.Drasil

import Data.Drasil.Domains (chemistry)
-- import Data.Drasil.Concepts.Documentation (property, value)

-- | Collects all chemistry-related concepts.
chemCon :: [ConceptChunk]
chemCon = [chemical,-- chemistry,
  compound, element, equation, reaction]

-- * Chemistry Concepts
--
-- $ChemCon
--
-- In alphabetical order.

chemical, compound, element, equation, reaction :: ConceptChunk
-- chemistry

chemical = dccWDS "chemical" (cn' "chemical")
  (S "relating to" +:+ phrase chemistry)
-- chemistry = dcc "chemistry" (cn' "chemistry")
--   "the study of the properties and behavior of matter" -- source: Wikipedia
compound = dccWDS "compound" (cn' "compound")
  (S "a molecule made up of more than one atom, which may or may not be of different" +:+
    plural element)
element = dcc "element" (cn' "element")
  ("the group of all atoms with the same number of protons in the atomic nucleus." ++
    "For example, all atoms with one proton are hydrogen atoms")
equation = dccWDS "equation" (cn' "equation")
  (S "a textual representation of a" +:+ phrase chemical +:+ S "reaction")
reaction = dcc "reaction" (cn' "reaction")
  "an interaction between different types of matter that forms one or more new substances"
