-- | Defines concepts used in the field of chemistry.
module Data.Drasil.Concepts.Chemistry where
--This is obviously a bad name, but for now it will do until we come
--  up with a better one.

import Prelude hiding (product)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations (hydrateSource, lund2023, polymerSource)
-- import Data.Drasil.Concepts.Documentation (property, value)
import Data.Drasil.Domains (chemistry)

-- | Collects all chemistry-related concepts.
chemCon :: [ConceptChunk]
chemCon = [chemical, compound, element, equation, feasible, formula, hydrate, -- chemistry
  isotope, nonStoicComp, polyIon, polymer, product, reactant, reaction, stoichiometry]

-- * Chemistry Concepts
--
-- $ChemCon
--
-- In alphabetical order.

chemical, compound, element, equation, feasible, formula, hydrate, isotope, -- chemistry
  nonStoicComp, polyIon, polymer, product, reactant, reaction, stoichiometry :: ConceptChunk

chemical = dccWDS "chemical" (cn' "chemical")
  (S "relating to" +:+ phrase chemistry)
-- chemistry = dcc "chemistry" (cn' "chemistry")
--   "the study of the properties and behavior of matter" -- source: Wikipedia
compound = dccWDS "compound" (cn' "compound")
  (S "a molecule made up of more than one atom" `sC`
    S "which may or may not be of different" +:+ plural element)
element = dcc "element" (cn' "element")
  ("the group of all atoms with the same number of protons in the atomic nucleus. " ++
   "For example, all atoms with one proton are hydrogen atoms") -- FIXME: add source
equation = dccWDS "equation" (cn' "equation")
  (S "a textual representation of a" +:+ phrase chemical +:+ phrase reaction)
feasible = dccWDS "feasible" (cn' "feasible")
  (sParen (S "Referring to a" +:+ phrase chemical +:+ phrase equation) +:+
    S "able to be balanced") -- FIXME: balanced defined in ChemCode?
  -- FIXME: add source
formula = dccWDS "formula" (cn' "formula")
  (S "a textual representation of a" +:+ phrase chemical +:+ phrase compound)
hydrate = dccWDS "hydrate" (cn' "hydrate")
  (Quote (S "a" +:+ phrase compound +:+ S "formed by the chemical combination" +:+
   S "of water and some other substance in a definite molecular ratio") +:+
   refS hydrateSource)
isotope = dccWDS "isotope" (cn' "isotope")
  (S "an atom that is the same" +:+ phrase element +:+ S "as another but" +:+
   S "has a different number of neutrons" +:+ refS lund2023)
nonStoicComp = dccWDS "nonStoicComp" (cn' "nonstoichiometric compound")
  $ Quote (S "Any solid" +:+ phrase chemical +:+ phrase compound +:+ S "in which the" +:+
   S "numbers of atoms of the" +:+ plural element +:+ S "present cannot be" +:+
   S "expressed as a ratio of small positive integers") -- FIXME: add source
polyIon = dccWDS "polyIon" (cn' "polyatomic ion")
  (S "a group of atoms" +:+ Quote (S "bonded together that carr[ies] an" +:+
    S "overall electric charge") +:+ refS lund2023)
polymer = dccWDS "polymer" (cn' "polymer")
  (S "a macromolecule" +:+ Quote (S "formed by the chemical bonding of" +:+
    S "large numbers of smaller molecules") +:+ refS polymerSource)
product = dccWDS "product" (cn' "product")
  (S "a substance formed by a" +:+ phrase chemical +:+ phrase reaction)
reactant = dccWDS "reactant" (cn' "reactant")
  (S "a substance involved in and changed by a" +:+ phrase chemical +:+
    phrase reaction)
reaction = dccWDS "reaction" (cn' "reaction")
  (S "an interaction between different types of matter that results in at" +:+
    S "least one new substance being formed" +:+ refS lund2023)
stoichiometry = dccWDS "stoichiometry" (cn' "stoichiometry")
  $ Quote (foldlSent_ [S "The calculation of the quantities of", plural reactant `S.or_`
      plural product, S "in a", phrase chemical, phrase reaction, S "using the",
    S "relationships found in a balanced", phrase chemical, phrase equation]) +:+
    -- FIXME: balanced defined in ChemCode?
    complexRef lund2023 (Page [337])

-- FIXME: getAbbrStr could likely be used to reuse this in People, but this
-- would cause an import cycle
iupac :: CI
iupac = commonIdeaWithDict "iupac"
  (pn "International Union of Pure and Applied Chemistry")
  "IUPAC" [chemistry]
