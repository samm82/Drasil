module Drasil.Pendulum.IMods (iMods, calAngularAccelerationIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC) 
import Utils.Drasil
import Data.Drasil.Quantities.Physics (gravitationalAccel,
         angularAccel, momentOfInertia, position, acceleration,
         time, angularFrequency, torque, time, angularDisplacement)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.Pendulum.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)
import Data.Drasil.Concepts.Math (constraint, equation)
import Data.Drasil.Concepts.Physics (pendulum)
import Drasil.Pendulum.TMods (newtonSLR)
import Drasil.Pendulum.GenDefs (angFrequencyGD)
import Drasil.Pendulum.DataDefs (angAccelerationDD, hPositionDD)


iMods :: [InstanceModel]
iMods = [calAngularAccelerationIM]

---
calAngularAccelerationIM :: InstanceModel
calAngularAccelerationIM = imNoRefs calAngularAccelerationRC 
  [qwC lenRod $ UpFrom (Exc, 0)
  ,qwC initialPendAngle $ UpFrom (Exc, 0)
  , qwC gravitationalAccel $ UpFrom (Exc, 0)
  , qwC angularAccel $ UpFrom (Exc, 0)
  , qwC time $ UpFrom (Exc, 0)]
  (qw pendDisplacementAngle) [UpFrom (Exc, 0)]
  (Just calAngularAccelerationDeriv) "calAngularAcceleration" [calAngularAccelConstraintNote]
  

calAngularAccelerationRC :: RelationConcept
calAngularAccelerationRC = makeRC "calAngularAccelerationRC" (nounPhraseSP "calculation of angular displacement")
  EmptyS $ apply1 pendDisplacementAngle time $= sy initialPendAngle * cos ( sy angularFrequency * sy time)
  

calAngularAccelerationDeriv :: Derivation 
calAngularAccelerationDeriv = mkDerivName (phrase angularAccel) (weave [calAngularAccelerationDerivSents, map E calAngularAccelerationDerivEqns])

calAngularAccelerationDerivSents :: [Sentence]
calAngularAccelerationDerivSents = [calAngularAccelerationDerivSent1, calAngularAccelerationDerivSent2, calAngularAccelerationDerivSent3,
                             calAngularAccelerationDerivSent4, calAngularAccelerationDerivSent5]

calAngularAccelerationDerivSent1, calAngularAccelerationDerivSent2, calAngularAccelerationDerivSent3,
  calAngularAccelerationDerivSent4, calAngularAccelerationDerivSent5 :: Sentence

calAngularAccelerationDerivSent1 = foldlSentCol [S "When the", phrase pendulum `sIs` S "displaced to an initial angle and released" `sC`
                                       S "the", phrase pendulum, S "swings back and forth with periodic motion" +:+
                                       S "By applying Newton's Second Law for Rotation" `sIn` makeRef2S newtonSLR `sC`
                                       S "the equation of motion for the", phrase pendulum, S "may be obtained"]
       
 
calAngularAccelerationDerivSent2 = foldlSentCol [S "Where", ch torque +:+ S "denotes the", phrase torque `sC`
                                    ch momentOfInertia +:+ S "denotes the", phrase momentOfInertia `sAnd` ch angularAccel +:+ 
                                    S "denotes the", phrase angularAccel +:+ definedIn'' angAccelerationDD +:+
                                    S "This implies"]
                 

calAngularAccelerationDerivSent3 = foldlSentCol [S "And rearranged as" ] 

calAngularAccelerationDerivSent4 = foldlSentCol [S "If the amplitude of", phrase angularDisplacement, S "is small enough" `sC`
  S "we can approximate", E (sin (sy pendDisplacementAngle) $= sy pendDisplacementAngle), S "for the purpose of a simple", phrase pendulum,
  S "at very small angles." :+:
  S " Then the", phrase equation, S "of motion reduces to the", phrase equation, S "of simple harmonic motion"]                                       

calAngularAccelerationDerivSent5 = foldlSentCol [S "Thus the simple harmonic motion is" ] 

calAngularAccelerationDerivEqns :: [Expr]
calAngularAccelerationDerivEqns = [calAngularAccelerationDerivEqn1, calAngularAccelerationDerivEqn2, calAngularAccelerationDerivEqn3,
                                 calAngularAccelerationDerivEqn4, calAngularAccelerationDerivEqn5]

calAngularAccelerationDerivEqn1, calAngularAccelerationDerivEqn2, calAngularAccelerationDerivEqn3,
 calAngularAccelerationDerivEqn4, calAngularAccelerationDerivEqn5 :: Expr

calAngularAccelerationDerivEqn1 = sy torque $= sy momentOfInertia * sy angularAccel

calAngularAccelerationDerivEqn2 = negate (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle) * sy lenRod) $= sy mass * sy lenRod $^ 2 
                                * deriv (deriv (sy pendDisplacementAngle) time) time 
                                                   
calAngularAccelerationDerivEqn3 = deriv (deriv (sy pendDisplacementAngle) time) time + sy gravitationalAccel/ sy lenRod * sin (sy pendDisplacementAngle) $= 0

calAngularAccelerationDerivEqn4 = deriv (deriv (sy pendDisplacementAngle) time) time + sy gravitationalAccel/ sy lenRod * sy pendDisplacementAngle $= 0

calAngularAccelerationDerivEqn5 = apply1 pendDisplacementAngle time $= sy initialPendAngle * cos ( sy angularFrequency * sy time)

----------------------
    
calAngularAccelConstraintNote :: Sentence


calAngularAccelConstraintNote = S "The" +:+ phrase constraint +:+
     E ( sy initialPendAngle $> 0) `sIs` S "required" +:+.
     S "The" +:+ phrase angularFrequency `sIs` definedIn'' angFrequencyGD +:+ 
     S "To calculate the" +:+ phrase acceleration `sC` S "we need to know the" +:+ phrase position +:+ makeRef2S hPositionDD
