module Drasil.DblPendulum.Unitals where

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.Constraints (gtZeroConstr)

import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst, requirement, srs, typUnc)
import Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.SI_Units (metre, degree, radian, kilogram)
import qualified Data.Drasil.Quantities.Physics as QP (position, ixPos, xPos, force, velocity,
  angularVelocity, angularAccel, gravitationalAccel, tension, acceleration, yAccel,
  xAccel, yVel, xVel, iyPos, yPos, time, torque, momentOfInertia, angularDisplacement,
  angularFrequency, frequency, period)
import Data.Drasil.Concepts.Physics (pendulum, twoD)
import Data.Drasil.Concepts.Math as CM (angle, iAngle, xDir, yDir)
import Data.Drasil.Quantities.Math as QM (unitVect, unitVectj, pi_)
import Drasil.DblPendulum.Concepts (rod, firstRod, secondRod, firstObject, secondObject)
import Data.Drasil.Units.Physics (angVelU, velU)
import Data.Drasil.Quantities.Physics (angularVelocity)


symbols:: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless

acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [lenRod, QPP.mass, QP.angularAccel, pendDisplacementAngle, initialPendAngle] 

outputs :: [QuantityDict]
outputs = map qw [pendDisplacementAngle]

units :: [UnitaryConceptDict]
units = map ucw unitalChunks

unitalChunks :: [UnitalChunk]
unitalChunks = [lenRod, 
  lenRod_1, lenRod_2, massObj_1, massObj_2, angularVel_1, angularVel_2,
  pendDisAngle_1, pendDisAngle_2, xVel_1, xVel_2, yVel_1, yVel_2,
  QPP.mass, QP.force, QP.ixPos, QP.xPos, QP.yPos,
  QP.angularVelocity, QP.angularAccel, QP.gravitationalAccel, QP.tension, QP.acceleration,
  QP.yAccel, QP.xAccel, QP.yVel, QP.xVel, QP.iyPos, QP.time, QP.velocity, QP.position, QP.torque,
  QP.momentOfInertia, QP.angularDisplacement, QP.angularVelocity, initialPendAngle,
  QP.angularFrequency, QP.frequency, QP.period, pendDisplacementAngle]

lenRod, lenRod_1, lenRod_2, massObj_1, massObj_2, angularVel_1, angularVel_2, 
  pendDisAngle_1, pendDisAngle_2, pendDisplacementAngle, initialPendAngle,
  xVel_1, yVel_1, xVel_2, yVel_2,
  initPendAngle_1, initPendAngle_2 :: UnitalChunk

-- Fix me, replace lenRod with lenOne
lenRod = makeUCWDS "l_rod" (nounPhraseSent $ phraseNP(len `the_ofThe` rod))
        (phraseNP (len `the_ofThe` rod))
        (sub cL lRod) metre

lenRod_1 = makeUCWDS "l_1" (nounPhraseSent $ phraseNP(len `the_ofThe` firstRod))
        (S "The" +:+ phraseNP (len `the_ofThe` firstRod)) -- Fix me, can have more information 
        (sub cL label1) metre

lenRod_2 = makeUCWDS "l_2" (nounPhraseSent $ phraseNP(len `the_ofThe` secondRod))
        (S "The" +:+ phraseNP (len `the_ofThe` secondRod))
        (sub cL label2) metre

massObj_1 = makeUCWDS "m_1" (nounPhraseSent $ phraseNP (mass `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (mass `the_ofThe` firstObject))
        (sub lM label1) kilogram

massObj_2 = makeUCWDS "m_2" (nounPhraseSent $ phraseNP (mass `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (mass `the_ofThe` secondObject))
        (sub lM label2) kilogram

xVel_1 = makeUCWDS "v_x1" (nounPhraseSent $ phraseNP (angularVelocity `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (angularVelocity `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label1])) velU

xVel_2 = makeUCWDS "v_x2" (nounPhraseSent $ phraseNP (angularVelocity `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (angularVelocity `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label2])) velU

yVel_1 = makeUCWDS "v_y1" (nounPhraseSent $ phraseNP (angularVelocity `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (angularVelocity `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label1])) velU

yVel_2 = makeUCWDS "v_y2" (nounPhraseSent $ phraseNP (angularVelocity `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (angularVelocity `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label2])) velU

-- xVel_1 = uc QP.xVel (sub lV  label1) velU

angularVel_1 = makeUCWDS "w_1" (nounPhraseSent $ phraseNP (angularVelocity `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (angularVelocity `the_ofThe` firstObject))
        (sub lW label1) angVelU

angularVel_2 = makeUCWDS "w_2" (nounPhraseSent $ phraseNP (angularVelocity `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (angularVelocity `the_ofThe` secondObject))
        (sub lW label2) angVelU

pendDisAngle_1 = makeUCWDS "theta_p1" (nounPhraseSent $ phraseNP (angle `the_ofThe` firstRod))
        (S "The" +:+ phraseNP (angle `the_ofThe` firstRod))
        (sub lTheta (Concat [lP, label1])) degree

pendDisAngle_2 = makeUCWDS "theta_p2" (nounPhraseSent $ phraseNP (angle `the_ofThe` secondRod))
        (S "The" +:+ phraseNP (angle `the_ofThe` secondRod))
        (sub lTheta (Concat [lP, label2])) degree

pendDisplacementAngle = makeUCWDS "pendDisplacementAngle" (nounPhraseSent $ phraseNP (angle `the_ofThe` pendulum))
        (phraseNP (angle `the_ofThe` pendulum))
        (sub lTheta lP) degree

initPendAngle_1 = makeUCWDS "theta_i1" (cn "initial pendulum angle")
        (S "The" +:+ phraseNP (NP.the (CM.iAngle `of_` pendulum)))
        (sub lTheta (Concat [lI, label1])) radian

initPendAngle_2 = makeUCWDS "theta_i2" (cn "initial pendulum angle")
        (S "The" +:+ phraseNP (NP.the (CM.iAngle `of_` pendulum)))
        (sub lTheta (Concat [lI, label2])) radian

initialPendAngle = makeUCWDS "initialPendAngle" (cn "initial pendulum angle")
        (phraseNP (NP.the (CM.iAngle `of_` pendulum)))
        (sub lTheta lI) radian


unitless :: [DefinedQuantityDict]
unitless = [QM.unitVect, QM.unitVectj, QM.pi_]
-----------------------

lRod, label1, label2, labelx, labely:: Symbol
lRod = label "rod"
labelx = label "x"
labely = label "y"
label1  = Integ 1
label2  = Integ 2

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lenRodCons, pendDisplacementAngleOutCons, angAccelOutCons, initialPendAngleCons :: ConstrConcept

inConstraints :: [UncertQ]
inConstraints = map (`uq` defaultUncrt)
  [lenRodCons, initialPendAngleCons]

outConstraints :: [UncertQ]
outConstraints = map (`uq` defaultUncrt) 
  [angAccelOutCons, pendDisplacementAngleOutCons]


lenRodCons     = constrained' lenRod        [gtZeroConstr] (dbl 44.2)
initialPendAngleCons  = constrained' initialPendAngle    [gtZeroConstr] (dbl 2.1)
--gravAccelCons  = constrained' QP.gravitationalAccel    [gtZeroConstr] (dbl 9.8)
pendDisplacementAngleOutCons  = constrained' pendDisplacementAngle    [gtZeroConstr] (dbl 2.1)
angAccelOutCons    = constrained' QP.angularAccel    [gtZeroConstr] (exactDbl 0)


