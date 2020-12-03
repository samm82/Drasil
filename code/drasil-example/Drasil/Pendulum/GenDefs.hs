module Drasil.Pendulum.GenDefs (genDefns, forceOnPendulumGD,
         angFrequencyGD, periodPend) where

import Prelude hiding (cos, sin, sqrt)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Quantities.Physics(angularAccel, acceleration, force, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, angularDisplacement, time,
    momentOfInertia, period, xForce, angularAccel, displacement)
import Data.Drasil.Concepts.Physics (pendulum, weight)
import Data.Drasil.Quantities.PhysicalProperties(mass)
import Drasil.Pendulum.TMods(newtonSLR)
import Drasil.Pendulum.DataDefs(periodSHMDD, angFrequencyDD, angAccelerationDD)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.Pendulum.TMods(newtonSL)
import Drasil.Pendulum.Figures (figMotion)

-- import Drasil.Pendulum.Assumptions (pend2DMotion, polarCoord,
      -- startOrigin, startTimeDesc, airDragNegDesc, motionSmallDesc)

import Drasil.Pendulum.Unitals (lenRod, pendDisplacementAngle)

genDefns :: [GenDefn]
genDefns = [forceOnPendulumGD, angFrequencyGD, periodPend] 


-------------------------------------Force acting on the Pendulum--------------------------------- 
forceOnPendulumGD :: GenDefn
forceOnPendulumGD = gdNoRefs forceOnPendulumRC (getUnit force)
           (Just forceOnPendulumDeriv) "forceOnPendulum" [{-Notes-}]

forceOnPendulumRC :: RelationConcept
forceOnPendulumRC = makeRC "forceOnPendulumRC" (nounPhraseSent $ foldlSent_ 
            [S "The", phrase force, S "acting on the", phrase pendulum]) EmptyS forceOnPendulumRel
 
forceOnPendulumRel :: Relation             
forceOnPendulumRel = sy force $= sy mass * sy acceleration $= negate (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle))
                        

forceOnPendulumDeriv :: Derivation
forceOnPendulumDeriv = mkDerivName (phrase xForce +:+ phrase pendulum) (weave [forceOnPendulumDerivSents, map E forceOnPendulumDerivEqns])

forceOnPendulumDerivSents :: [Sentence]
forceOnPendulumDerivSents = [forceOnPendulumDerivSent1, forceOnPendulumDerivSent2]

forceOnPendulumDerivSent1, forceOnPendulumDerivSent2 :: Sentence

forceOnPendulumDerivEqns :: [Expr]
forceOnPendulumDerivEqns = [forceOnPendulumDerivEqn1, forceOnPendulumDerivEqn2]

forceOnPendulumDerivEqn1, forceOnPendulumDerivEqn2 :: Expr 

forceOnPendulumDerivSent1 = S "According to" +:+ makeRef2S newtonSL

forceOnPendulumDerivEqn1 = sy force $= sy mass * sy acceleration $= negate (sy mass * sy gravitationalAccel)

forceOnPendulumDerivSent2 =  S "The net" +:+ phrase force +:+ S "of the" +:+ phrase pendulum +:+ S "along the tangential direction" +:+
              S "is" +:+ E ( sy mass * sy gravitationalAccel) +:+

              S "The negative sign is from the direction of" `ofThe` phrase force +:+ S "which is against the direction" `ofThe` phrase mass +:+
                 makeRef2S figMotion +:+
              S "So from the above" +:+ phrase equation

forceOnPendulumDerivEqn2 = sy force $= negate (sy mass * sy gravitationalAccel) * sin (sy pendDisplacementAngle)



--------------------------------------Angular Frequency of Pendulum-------------------------------------------

angFrequencyGD :: GenDefn
angFrequencyGD = gdNoRefs angFrequencyRC (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyRC :: RelationConcept
angFrequencyRC = makeRC "angFrequencyRC" (nounPhraseSent $ foldlSent_ 
            [ S "The" +:+ phrase angularFrequency `ofThe` phrase pendulum]) EmptyS angFrequencyRel
 
angFrequencyRel :: Relation             
angFrequencyRel = sy angularFrequency $= sqrt (sy gravitationalAccel / sy lenRod )

angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phrase angularFrequency +:+ phrase pendulum) (weave [angFrequencyDerivSents, map E angFrequencyDerivEqns])


angFrequencyDerivSents :: [Sentence]
angFrequencyDerivSents = [angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
                      angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7]

angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
     angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7 :: Sentence

angFrequencyDerivEqns :: [Expr]
angFrequencyDerivEqns = [angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3,
                     angFrequencyDerivEqn4, angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7]

angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3, angFrequencyDerivEqn4,
                   angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7 :: Expr

                                 

angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque `sC` S "on a", phrase pendulum, definedIn'' newtonSLR `sC`
                  S "the", phrase force, S "providing the restoring" +:+ phrase torque `sIs` S "the component of the",
                  phrase weight `ofThe` phrase pendulum, S "bob that acts along the arc length" +:+
                  S "The", phrase torque `isThe` S "length" `ofThe` S "string", ch lenRod +:+ S "multiplied by the component"
                  `ofThe` S "net", phrase force +:+ S "that is perpendicular to the radius" `ofThe` S "arc." +:+
                  S "The minus sign indicates the" +:+ phrase torque +:+ S "acts in the opposite direction of the", phrase angularDisplacement]


angFrequencyDerivEqn1 = sy torque $= negate (sy lenRod) * (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle))
angFrequencyDerivSent2 = S "So then"
angFrequencyDerivEqn2 = sy momentOfInertia * sy angularAccel $= negate (sy lenRod) * (sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle))
angFrequencyDerivSent3 = S "Therefore,"
angFrequencyDerivEqn3 = sy momentOfInertia * deriv (deriv (sy pendDisplacementAngle) time) time $= negate (sy lenRod)
             * sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle)
angFrequencyDerivSent4 = S "Substituting for" +:+ ch momentOfInertia
angFrequencyDerivEqn4 = sy mass * sy lenRod $^ 2 * deriv (deriv (sy pendDisplacementAngle) time) time $= negate (sy lenRod)
             * sy mass * sy gravitationalAccel * sin (sy pendDisplacementAngle)
angFrequencyDerivSent5 = S "Crossing out" +:+ ch mass `sAnd` ch lenRod +:+ S "we have"
angFrequencyDerivEqn5 = deriv (deriv (sy pendDisplacementAngle) time) time $= negate(sy gravitationalAccel/ sy lenRod) * sin (sy pendDisplacementAngle)
angFrequencyDerivSent6 = S "For small angles, we approximate" +:+ S "sin" +:+ ch pendDisplacementAngle +:+ S "to" +:+ ch pendDisplacementAngle
angFrequencyDerivEqn6 = deriv (deriv (sy pendDisplacementAngle) time) time $= negate(sy gravitationalAccel/ sy lenRod) * sy pendDisplacementAngle
angFrequencyDerivSent7 = S "Because this" +:+ phrase equation `sC` S "has the same form as the" +:+ phrase equation +:+
                  S "for simple harmonic motion the solution is easy to find." +:+ S " The" +:+ phrase angularFrequency
angFrequencyDerivEqn7 = sy angularFrequency $= sqrt (sy gravitationalAccel / sy lenRod)
angFrequencyGDNotes :: Sentence
angFrequencyGDNotes = S "The" +:+ makeRef2S angAccelerationDD +:+ S "The" +:+ phrase pendulum +:+ phrase period +:+ definedIn'' periodPend +:+
    S "can be derived from here"
 

                                       
 --------------------------------Period of Pendulum Motion---------------------------------------- 

periodPend :: GenDefn
periodPend = gdNoRefs periodPendRC (getUnit period)
           (Just periodPendDeriv) "periodPend" [periodPendNotes]

periodPendRC :: RelationConcept
periodPendRC = makeRC "periodPendRC" (nounPhraseSent $ foldlSent_ 
            [ S "The", phrase period, S "on the", phrase pendulum]) EmptyS periodPendRel
 
periodPendRel :: Relation             
periodPendRel = sy period $= 2 * sy QM.pi_ * sqrt (sy lenRod/ sy gravitationalAccel)

periodPendDeriv :: Derivation
periodPendDeriv = mkDerivName (phrase period +:+ phrase pendulum) (weave [periodPendDerivSents, map E periodPendDerivEqns])    

periodPendDerivSents :: [Sentence]
periodPendDerivSents = [periodPendDerivSent1, periodPendDerivSent2]

periodPendDerivSent1, periodPendDerivSent2 :: Sentence    

periodPendDerivEqns :: [Expr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: Expr 

periodPendDerivSent1 = S "The" +:+ phrase period `ofThe` phrase pendulum +:+ S "can be defined from" +:+
                makeRef2S angFrequencyGD +:+ phrase equation
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel / sy lenRod)
periodPendDerivSent2 =  S "Therefore from the" +:+ phrase equation +:+ makeRef2S angFrequencyDD `sC` S "we have"

periodPendDerivEqn2 = sy period $= 2 * sy QM.pi_ * sqrt (sy lenRod/ sy gravitationalAccel)

periodPendNotes :: Sentence
periodPendNotes = S "The" +:+ getTandS period `sIs` definedIn'' periodSHMDD 

-----------------------Angular Acceleration of Pendulum----------------------------------------------------

{-angAccelerationGD :: GenDefn
angAccelerationGD = gdNoRefs angAccelerationRC (getUnit angularAccel)
           (Just angAccelerationDeriv) "angAccelerationGD" [angAccelerationNotes]

angAccelerationRC :: RelationConcept
angAccelerationRC = makeRC "angAccelerationRC" (nounPhraseSent $ foldlSent_ 
            [ S "The" +:+ phrase angularAccel `ofThe` phrase pendulum]) EmptyS angAccelerationRel
 
angAccelerationRel :: Relation             
angAccelerationRel = sy angularAccel $= negate (sy gravitationalAccel / sy lenRod ) * sin (sy pendDisplacementAngle)

angAccelerationDeriv :: Derivation
angAccelerationDeriv = mkDerivName (phrase angularAccel +:+ phrase pendulum) (weave [angAccelerationDerivSents, map E angAccelerationDerivEqns])


angAccelerationDerivSents :: [Sentence]
angAccelerationDerivSents = [angAccelerationDerivSent1, angAccelerationDerivSent2, angAccelerationDerivSent3, angAccelerationDerivSent4, angAccelerationDerivSent5]

angAccelerationDerivSent1, angAccelerationDerivSent2, angAccelerationDerivSent3, angAccelerationDerivSent4, angAccelerationDerivSent5 :: Sentence

angAccelerationDerivEqns :: [Expr]
angAccelerationDerivEqns = [angAccelerationDerivEqn1, angAccelerationDerivEqn2, angAccelerationDerivEqn3, angAccelerationDerivEqn4, angAccelerationDerivEqn5]

angAccelerationDerivEqn1, angAccelerationDerivEqn2, angAccelerationDerivEqn3, angAccelerationDerivEqn4, angAccelerationDerivEqn5 :: Expr
                                 

angAccelerationDerivSent1 = foldlSentCol [S "From Newton's Law of Rotation" +:+ definedIn'' newtonSLR]           

angAccelerationDerivEqn1 = sy torque $= sy momentOfInertia * sy angularAccel

angAccelerationDerivSent2 = S "Summing" +:+ plural torque +:+ S "the restoring" +:+ phrase torque +:+ 
                            S "being the only one gives"

angAccelerationDerivEqn2 = sumAll (Variable "i")(sy torque) $= negate ( sy mass * sy gravitationalAccel * sy lenRod) * sin (sy pendDisplacementAngle)
               $= sy momentOfInertia * sy angularAccel


angAccelerationDerivSent3 = S "For small angular" +:+ phrase displacement `sC` E (sin (sy pendDisplacementAngle $= sy pendDisplacementAngle)) +:+
        S "so the" +:+ phrase torque +:+ phrase equation +:+ S "becomes: " 
angAccelerationDerivEqn3 = negate (sy mass * sy gravitationalAccel) * sy lenRod * sin (sy pendDisplacementAngle) $=  sy momentOfInertia * sy angularAccel

angAccelerationDerivSent4 = S "Whenever the" +:+ phrase acceleration `sIs` S "proportional to" `sAnd` S "in the opposite direction as the" +:+
       phrase displacement `sC` S "the motion is simple harmonic" +:+
       S "For a simple" +:+ phrase pendulum `sC` S "with all the" +:+ phrase mass +:+ S "the same distance from the suspension point" `sC`
       S "the" +:+ phrase momentOfInertia `sIs` S "this:"

angAccelerationDerivEqn4 = sy momentOfInertia $= sy mass * sy lenRod $^ 2 

angAccelerationDerivSent5 = S "The" +:+ phrase equation +:+ S "relating the" +:+ phrase angularAccel `toThe`
      phrase angularDisplacement +:+ S "for a simple" +:+ phrase pendulum +:+ S "thus becomes:"
angAccelerationDerivEqn5 = sy angularAccel $= negate(sy gravitationalAccel/ sy lenRod) * (sy pendDisplacementAngle)

angAccelerationNotes :: Sentence
angAccelerationNotes = S "The" +:+ phrase acceleration +:+ S "changes throughout an oscillation of a" +:+ phrase pendulum `sC`
 S "and the integral of" +:+ phrase angularAccel +:+ S "versus" +:+ phrase time +:+ S "graph will be change in" +:+
 phrase angularVelocity +:+ makeRef2S angVelocityTM-}