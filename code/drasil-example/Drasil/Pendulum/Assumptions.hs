module Drasil.Pendulum.Assumptions (pend2DMotion, polarCoord, startOrigin, startTimeDesc,
            airDragNegDesc, motionSmallDesc, assumptions) where
    
import Language.Drasil
import Utils.Drasil
import Drasil.Pendulum.Concepts (pendulumTitle)
import Data.Drasil.Concepts.Documentation (assumpDom)
import Data.Drasil.Concepts.Physics (twoD, time)

assumptions :: [ConceptInstance]
assumptions = [pend2DMotion, polarCoord, startOrigin, startTime, airDragNeg, motionSmall]

pend2DMotion, polarCoord, startOrigin, startTime, airDragNeg, motionSmall :: ConceptInstance 

pend2DMotion    = cic "pend2DMotion"      pend2DMotionDesc    "pend2DMotion"    assumpDom
polarCoord      = cic "polarCoord"        polarCoordDesc      "polarCoord"      assumpDom
startOrigin     = cic "startOrigin"       startOriginDesc     "startOrigin"     assumpDom
motionSmall     = cic "motionSmall"       motionSmallDesc     "motionSmall"     assumpDom
startTime       = cic "startTime"         startTimeDesc       "startTime"       assumpDom
airDragNeg      = cic "airDragNeg"        airDragNegDesc      "airDragNeg"      assumpDom

pend2DMotionDesc :: Sentence
pend2DMotionDesc = S "The" +:+ phrase pendulumTitle +:+ S "motion" `sIs` phrase twoD +:+. sParen (getAcc twoD)

polarCoordDesc :: Sentence
polarCoordDesc = S "Polar coordinate" `sIs` S "used" 

startOriginDesc :: Sentence
startOriginDesc = S "The" +:+. (phrase pendulumTitle `sIs` S "attached" `toThe` S "origin")

motionSmallDesc :: Sentence
motionSmallDesc = S "The motion" `sIs` S "small enough that the curvature of the earth can be neglected"

startTimeDesc :: Sentence
startTimeDesc = S "Start" +:+ phrase time `sIs` S "zero"

airDragNegDesc :: Sentence
airDragNegDesc = S "Air drag" `sIs` S "neglected"





