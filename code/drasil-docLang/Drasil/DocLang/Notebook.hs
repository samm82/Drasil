module Drasil.DocLang.Notebook where

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, prpsOfDoc, 
  review, body, mainIdea, procForAnls, summary, methAndAnls, coordinateSystem, 
  example, appendix, reference)
import qualified Data.Drasil.Concepts.Physics as P (motion, horizontalMotion, verticalMotion, kinematics)

-- | Notebook constructor. 
-- Create the notebook from given system name, authors, and sections
--doc :: NamedIdea c => c -> Sentence -> [Section] -> Document
--doc  sys = Document (Doc.notebook `S.forTPS` sys)

intro, prpsOfDoc, body, review, mainIdea, motion, hormotion, vermotion, methAndAnls,
  coorSyst, kinematic, procForAnls, summary, appendix, reference, example :: Int -> [Contents] -> Section
  
intro       d cs = section "intro"       d (titleize Doc.introduction)       cs introLabel
prpsOfDoc   d cs = section "prpsOfDoc"   d (titleize Doc.prpsOfDoc)          cs docPurposeLabel

body        d cs = section "body"        d (titleize Doc.body)             cs bodyLabel
review      d cs = section "review"      d (titleize Doc.review)           cs reviewLabel

mainIdea    d cs = section "mainIdea"    d (titleize Doc.mainIdea)         cs mainIdeaLabel
motion      d cs = section "motion"      d (titleize P.motion)             cs motionLabel
hormotion   d cs = section "hormotion"   d (titleize P.horizontalMotion)   cs hormotionLabel
vermotion   d cs = section "vermotion"   d (titleize P.verticalMotion)     cs vermotionLabel

methAndAnls d cs = section "methAndAnls" d (titleize' Doc.methAndAnls)     cs methsAndanlsLabel

summary     d cs = section "summary"     d (titleize Doc.summary)          cs summaryLabel

procForAnls d cs = section "procForAnls" d (titleize Doc.procForAnls)      cs anlsProcLabel
coorSyst    d cs = section "coorSyst"    d (titleize Doc.coordinateSystem) cs coorSystLabel
kinematic   d cs = section "kinematic"   d (titleize P.kinematics)         cs kinematicLabel

appendix    d cs = section "appendix"    d (titleize Doc.appendix)         cs appendixLabel

reference   d cs = section "reference"   d (titleize' Doc.reference)       cs referenceLabel
example     d cs = section "example"     d (titleize Doc.example)          cs exampleLabel

--Labels--
sectionReferences :: [Reference]
sectionReferences = [introLabel, docPurposeLabel, methsAndanlsLabel, referenceLabel,
  bodyLabel, reviewLabel, mainIdeaLabel, motionLabel, hormotionLabel, vermotionLabel, 
  appendixLabel, coorSystLabel, kinematicLabel, summaryLabel, anlsProcLabel, exampleLabel]

introLabel, docPurposeLabel, methsAndanlsLabel, referenceLabel, bodyLabel,
  reviewLabel, mainIdeaLabel, motionLabel, hormotionLabel, vermotionLabel, 
  appendixLabel, coorSystLabel, kinematicLabel, summaryLabel, anlsProcLabel, exampleLabel :: Reference
introLabel          = makeSecRef "Intro"            $ titleize Doc.introduction
docPurposeLabel     = makeSecRef "DocPurpose"       $ titleize Doc.prpsOfDoc
methsAndanlsLabel   = makeSecRef "MethsAndAnls"     $ titleize' Doc.methAndAnls
referenceLabel      = makeSecRef "References"       $ titleize' Doc.reference
bodyLabel           = makeSecRef "Body"             $ titleize Doc.body
reviewLabel         = makeSecRef "Review"           $ titleize Doc.review
mainIdeaLabel       = makeSecRef "MainIdea"         $titleize Doc.mainIdea        
motionLabel         = makeSecRef "Motion"           $ titleize P.motion              
hormotionLabel      = makeSecRef "HorizontalMotion" $ titleize P.horizontalMotion
vermotionLabel      = makeSecRef "VerticalMotion"   $ titleize P.verticalMotion
appendixLabel       = makeSecRef "Appendix"         $ titleize Doc.appendix
coorSystLabel       = makeSecRef "CoordinateSystem" $ titleize Doc.coordinateSystem
kinematicLabel      = makeSecRef "Kinematic"        $ titleize P.kinematics
summaryLabel        = makeSecRef "Summary"          $ titleize Doc.summary
anlsProcLabel       = makeSecRef "AnlsProc"         $ titleize Doc.procForAnls
exampleLabel        = makeSecRef "Example"          $ titleize Doc.example