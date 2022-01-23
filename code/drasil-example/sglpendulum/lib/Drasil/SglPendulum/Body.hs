{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPendulum.Body where

import Language.Drasil hiding (organization, section)
import Theory.Drasil (TheoryModel)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.People (olu)
import Data.Drasil.SI_Units (metre, second, newton, kilogram, degree, radian, hertz)
import Data.Drasil.Concepts.Computation (compcon, inValue, algorithm)
import Data.Drasil.Concepts.Documentation (srsDomains, doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.PhysicalProperties (mass, len, physicalcon)
import Data.Drasil.Concepts.Software (program, errMsg)
import Data.Drasil.Domains (physics)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Theories.Physics (newtonSLR)
import Data.Drasil.TheoryConcepts (inModel)
import Data.Drasil.Quantities.Math (unitVect, unitVectj)
import Data.Drasil.Quantities.Physics (physicscon)

import Drasil.DblPendulum.Assumptions (assumpSingle)
import Drasil.DblPendulum.Body (justification, charsOfReader, prob, organizationOfDocumentsIntro,
  sysCtxIntro, sysCtxDesc, sysCtxList, stdFields, scope, terms, userCharacteristicsIntro)
import qualified Drasil.DblPendulum.Body as DPD (tMods)
import Drasil.DblPendulum.Concepts (concepts, rod)
import Drasil.DblPendulum.Requirements (nonFuncReqs)
import Drasil.DblPendulum.Unitals (acronyms)
import Drasil.DblPendulum.References (citations)

import Drasil.SglPendulum.Figures (figMotion, sysCtxFig1)
import Drasil.SglPendulum.Goals (goals, goalsInputs)
import Drasil.SglPendulum.DataDefs (dataDefs)
import Drasil.SglPendulum.IMods (iMods)
import Drasil.SglPendulum.GenDefs (genDefns)
import Drasil.SglPendulum.Unitals (inputs, outputs, inConstraints, outConstraints, symbols)
import Drasil.SglPendulum.Requirements (funcReqs)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

{-
tocTree :: Tree D.SRSSection
tocTree = 
  Node D.TableOfContents [
    Node D.RefSec [
      Node D.TUnits [],
      Node D.TSymb [], 
      Node D.TAandA []],
    Node D.IntroSec [
      Node D.IPurposeSub [],
      Node D.IScopeSub [], 
      Node D.ICharSub [],
      Node D.IOrgSub []],
    Node D.GSDSec [
      Node D.SysCntxt [],
      Node D.UsrChars [], 
      Node D.SystCons []],
    Node D.SSDSec [
      Node D.ProblemDescription [
        Node D.TermsAndDefs [],
        Node D.PhySysDesc [],
        Node D.Goals []],
      Node D.SolChSpec [
        Node D.Assumptions [],
        Node D.TMs [],
        Node D.GDs [],
        Node D.DDs [],
        Node D.IMs [],
        Node D.Constraints [],
        Node D.CorrSolnPpties []]],
    Node D.ReqrmntSec [
      Node D.FReqsSub [],
      Node D.NonFReqsSub []],
    Node D.TraceabilitySec [],
    Node D.AuxConstntSec [],
    Node D.Bibliography []
  ]
-}

toC :: [SRSSection]
toC = [TableContents,
    Ref' [
      TU, 
      TS, 
      TAA],
    Intro [
      IPurpose, 
      IScope, 
      IChar, 
      IOrg],
    GSD [
      SysCntxt', 
      UsrChars', 
      SystCons'],
    SSD [
      ProblemDescription' [
        TermsAndDefs',
        PhySysDesc',
        Goals'], 
      SolChSpec' [
        Assumpt,
        TM,
        GD,
        DD,
        IM,
        Consts,
        CorSolPpt]],
    Reqrmnt [
      FReqs,
      NonFReqs], 
    Traceability,
    AuxConstnt,
    Bibliography'
  ]

mkSRS :: SRSDecl
mkSRS = [TableOfContents $ ToCProg toC,        -- This creates the Table of Contents
  RefSec $ RefProg intro,        -- This creates the Reference section of the SRS
  -- This add the introduction blob to the reference section  
    TUnits TUProg,                 -- Adds table of unit section with a table frame
    TSymb $ tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits], -- Adds table of symbol section with a table frame
      --introductory blob (TSPurpose), TypogConvention, bolds vector parameters (Vector Bold), orders the symbol, and adds units to symbols 
    TAandA TAAProg,                -- Add table of abbreviation and acronym section
  IntroSec $ IntroProg justification (phrase progName),
    IPurposeSub $ IPurposeProg $ purpDoc progName Verbose,
    IScopeSub $ IScopeProg scope,
    ICharSub $ ICharProg [] charsOfReader [],
    IOrgSub $ IOrgProg organizationOfDocumentsIntro inModel (SRS.inModel 0 []) EmptyS,
  GSDSec $ GSDProg EmptyS,
    SysCntxt $ SysCntxtProg [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
    UsrChars $ UsrCharsProg [userCharacteristicsIntro], 
    SystCons $ SystConsProg [],                            
  SSDSec $ SSDProg EmptyS,
    ProblemDescription $ PDProg prob,                      --  This adds a is used to define the problem your system will solve
      TermsAndDefs $ TDProg Nothing terms,                      -- This is used to define the terms to be defined in terminology sub section
      PhySysDesc $ PSDProg progName physSystParts figMotion [], -- This defines the Physicalsystem sub-section, define the parts
                                                            -- of the system using physSysParts, figMotion is a function in figures for the image
      Goals $ GProg goalsInputs, -- This adds a goals section and goals input is defined for the preample of the goal.
    SolChSpec $ SCSProg EmptyS, -- This creates the solution characteristics section with a preamble
      Assumptions $ AssumpProg EmptyS,
      TMs $ TMProg [] (Label : stdFields),
      GDs $ GDProg [] ([Label, Units] ++ stdFields) ShowDerivation,
      DDs $ DDProg [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation,
      IMs $ IMProg [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation,
      Constraints $ ConstProg EmptyS inConstraints,
      CorrSolnPpties $ CorrSolProg outConstraints [],
  ReqrmntSec $ ReqsProg EmptyS,
    FReqsSub $ FReqsProg EmptyS [],
    NonFReqsSub NonFReqsProg,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg progName [],  -- Adds Auxilliary constraint section
  Bibliography                              -- Adds reference section
  ]

progName :: CI
progName = commonIdeaWithDict "pendulumTitle" (pn "Pendulum") "SglPendulum" [physics]

si :: SystemInformation
si = SI {
  _sys         = progName, 
  _kind        = Doc.srs,
  _authors     = [olu],
  _purpose     = purpDoc progName Verbose,
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = inConstraints,
  _constants   = [] :: [ConstQDef],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb (map qw iMods ++ map qw symbols)
  (nw newtonSLR : nw progName : nw mass : nw len : nw kilogram : nw inValue : nw newton : nw degree : nw radian
    : nw unitVect : nw unitVectj : [nw errMsg, nw program] ++ map nw symbols ++
   map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw physicCon' ++
   map nw physicscon ++ concepts ++ map nw physicalcon ++ map nw acronyms ++ map nw symbols ++ map nw [metre, hertz] ++
   [nw algorithm] ++ map nw compcon ++ map nw educon ++ map nw prodtcon)
  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz]) dataDefs
  iMods genDefns tMods concIns [] [] ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbols) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumpSingle ++ goals ++ funcReqs ++ nonFuncReqs

------------------------------
-- Section : INTRODUCTION --
------------------------------

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.4 : Organization of Documents --
-------------------------------------

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
-- Description of Genreal System automatically generated in GSDProg

--------------------------
-- 3.1 : System Context --
--------------------------

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

-------------------------------
-- 3.3 : System Constraints  --
-------------------------------
-- System Constraints automatically generated in SystCons


--------------------------------------------
-- Section 4: Specific System Description --
--------------------------------------------
-- Description of Specific System automatically generated in SSDProg

-------------------------------
-- 4.1 : System Constraints  --
-------------------------------

---------------------------------
-- 4.1.1 Terminology and Definitions --
---------------------------------

-----------------------------------
-- 4.1.2 Physical System Description --
-----------------------------------
physSystParts :: [Sentence]
physSystParts = map ((!.) . atStartNP) [the rod, the mass]

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------
-- Assumptions defined in Assumptions

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------
-- Theoretical Models defined in TMs
tMods :: [TheoryModel]
tMods = DPD.tMods ++ [newtonSLR]

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------
-- General Definitions defined in GDs

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-- Data Definitions defined in DDs

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------
-- Instance Models defined in IMs

-----------------------------
-- 4.2.6 : Data Constraints --
-----------------------------
-- Data Constraints defined in Constraints

-----------------------------
-- 4.2.7 : Properties of a Correct Solution --
-----------------------------
-- Properties of a Correct Solution defined in CorrSolnPpties

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------
-- in Requirements.hs

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

--------------------------------
-- SECTION 6b : UNLIKELY CHANGES --
--------------------------------

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------

----------------------------
-- Section 9 : References --
----------------------------
