-- | Defines helper functions for making the Table of Auxiliary Constants section.
module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF, tableOfConstants, tableOfConstantsRef) where

import Language.Drasil
import Utils.Drasil
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (value, description, symbol_, tAuxConsts)
import qualified Data.Drasil.Concepts.Math as CM (unit_)
import Control.Lens ((^.))

-- | Gets the auxiliary constant values given an introductory 'Idea' and a 'QDefinition'.
valsOfAuxConstantsF :: Idea a => a ->[SimpleQDef] -> Section
valsOfAuxConstantsF kWord listOfConstants = SRS.valsOfAuxCons 0 (contentGenerator kWord listOfConstants)  []

-- | Gets a table of constants from a 'QDefinition'. Also uses an 'Idea' as the introduction.
contentGenerator :: Idea a => a -> [SimpleQDef] -> [Contents]
contentGenerator _ [] = [foldlSP [S "There are no auxiliary constants"]]
contentGenerator a b  = [intro a, LlC $ tableOfConstants b]

--FIXME: general introduction?
-- | Helper that creates a general introduction using an 'Idea'.
intro :: (Idea a) => a -> Contents
intro kWord =  foldlSP [S "This section contains the standard values that are used for calculations in" +:+ short kWord]

-- | Helper that gets a table of constants from a 'QDefinition'.
tableOfConstants :: [SimpleQDef] -> LabelledContent
tableOfConstants f = llcc tableOfConstantsRef $ Table
  [titleize symbol_, titleize description, titleize value, titleize CM.unit_]
  (mkTable [ch, phrase, \c -> eS $ express $ c ^. defnExpr, toSentence] f)
  (titleize' tAuxConsts)
  True

-- | Table of constants reference label.
tableOfConstantsRef :: Reference
tableOfConstantsRef = makeTabRef' (tAuxConsts ^. uid)
