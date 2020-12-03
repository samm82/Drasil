module Drasil.Pendulum.DataDefs (dataDefs, angFrequencyDD,
         frequencyDD, periodSHMDD, angAccelerationDD, hPositionDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import Utils.Drasil
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)
import Drasil.Pendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (
      frequency, period, angularFrequency, angularAccel, time, position)
import Drasil.Pendulum.Unitals (lenRod, pendDisplacementAngle)
import Data.Drasil.Concepts.Physics (pendulum)
import qualified Data.Drasil.Quantities.Math as QM (pi_)


dataDefs :: [DataDefinition]
dataDefs = [frequencyDD, angFrequencyDD, periodSHMDD, angAccelerationDD, hPositionDD]

hPositionDD :: DataDefinition
hPositionDD = ddNoRefs hPositionDDQD Nothing "hPositionDD" [hPositionRef]

hPositionDDQD :: QDefinition
hPositionDDQD = mkQuantDef QP.position hPositionDDEqn

hPositionDDEqn :: Expr
hPositionDDEqn = sy lenRod * sin(sy pendDisplacementAngle)


hPositionRef :: Sentence
hPositionRef = S "This is the horizontal" +:+ phrase QP.position `ofThe` phrase pendulum

-------------------------------FrequencyDD
frequencyDD :: DataDefinition
frequencyDD = ddNoRefs frequencyDDQD Nothing "frequencyDD" [frequencyRef]

frequencyDDQD :: QDefinition
frequencyDDQD = mkQuantDef QP.frequency frequencyDDEqn

frequencyDDEqn :: Expr
frequencyDDEqn = 1 / sy QP.period


frequencyRef :: Sentence
frequencyRef = ch QP.frequency `isThe` S "number of back and forth swings in one" +:+ phrase second


------------------------------------------------------

angFrequencyDD :: DataDefinition
angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [angFrequencyRef]

angFrequencyDDQD :: QDefinition
angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

angFrequencyDDEqn :: Expr
angFrequencyDDEqn = cross (2 * sy QM.pi_)  (1/sy QP.period) $= 2 * sy QM.pi_ /sy QP.period


angFrequencyRef :: Sentence
angFrequencyRef = ch QP.period `sIs` S "from" +:+ makeRef2S periodSHMDD
----------------------------------------------------------

periodSHMDD :: DataDefinition
periodSHMDD = ddNoRefs periodSHMDDQD Nothing "periodSHMDD" [periodSHMRef]

periodSHMDDQD :: QDefinition
periodSHMDDQD = mkQuantDef QP.period periodSHMDDEqn

periodSHMDDEqn :: Expr
periodSHMDDEqn = 2 * sy QM.pi_ /sy QP.angularFrequency

periodSHMRef :: Sentence
periodSHMRef = ch QP.period `sIs` S "from" +:+ makeRef2S angFrequencyDD

-------------------Angular Acceleration---------------------

angAccelerationDD :: DataDefinition
angAccelerationDD = ddNoRefs angAccelerationDDQD Nothing "angAccelerationDD" [angAccelerationRef]

angAccelerationDDQD :: QDefinition
angAccelerationDDQD = mkQuantDef QP.angularAccel angAccelerationDDEqn

angAccelerationDDEqn :: Expr
angAccelerationDDEqn =  deriv (sy QP.angularFrequency) QP.time


angAccelerationRef :: Sentence
angAccelerationRef = ch QP.angularFrequency `sIs` S "from" +:+ makeRef2S angFrequencyDD


