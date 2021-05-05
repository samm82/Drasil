module Drasil.DblPendulum.Goals (goals, goalsInputs) where

import Language.Drasil
import Utils.Drasil
import Data.Drasil.Concepts.Documentation (goalStmtDom)
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (mass, len)
import Data.Drasil.Concepts.Physics (gravitationalConst)
import Data.Drasil.Concepts.Math (iAngle)


goals :: [ConceptInstance]
goals = [motionMass]


goalsInputs :: [Sentence]
goalsInputs = [phrase CPP.mass `sAnd` phrase CPP.len  `ofThe` (S "rod" `sC` phrase iAngle ) +:+ S "of the" +:+ phrase CPP.mass `andThe` phrase gravitationalConst ]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate" +:+ S "motion" `ofThe` phrase CPP.mass)
  "Motion-of-the-mass" goalStmtDom


