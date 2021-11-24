-- | Holds all section constructors and labels for creating SRS documents.
module Drasil.DocLang.SRS (
  -- * Section Constructors
  -- | For use in an SRS document. Ordered by appearance in a SRS.
  tOfCont, refMat, tOfUnit, tOfSymb, tOfAbbAcc, intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, genSysDes, sysCont,
  userChar, sysCon, specSysDes, probDesc, termAndDefn, physSyst, goalStmt,
  solCharSpec, assumpt, thModel, genDefn, dataDefn, inModel, datCon, propCorSol,
  require, nonfuncReq, funcReq, likeChg, unlikeChg, traceyMandG, valsOfAuxCons,
  reference, appendix, offShelfSol, scpOfTheProj, prodUCTable, indPRCase,
  termogy,
  -- * Section Labels
  -- | Labels linked to the associated section constructor. Ordered by appearance in a SRS.
  tOfContLabel, refMatLabel, tOfUnitLabel, tOfSymbLabel, tOfAbbAccLabel,
  introLabel, docPurposeLabel, reqsScopeLabel, readerCharsLabel, docOrgLabel,
  stakeholderLabel, clientLabel, customerLabel, genSysDescLabel, sysContextLabel,
  userCharsLabel, sysConstraintsLabel, specSystDescLabel, physSystLabel, probDescLabel,
  termDefsLabel, goalStmtLabel, solCharSpecLabel, assumptLabel, thModelLabel,
  genDefnLabel, dataDefnLabel, inModelLabel, datConLabel, corSolPropsLabel, requirementsLabel,
  funcReqLabel, nonfuncReqLabel, likeChgLabel, unlikeChgLabel, traceMatricesLabel,
  valsOfAuxConsLabel, referenceLabel, appendixLabel, offShelfSolnsLabel, indPRCaseLabel,
  projScopeLabel, useCaseTableLabel, terminologyLabel,
  -- * All Section References
  sectionReferences) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil
import Utils.Drasil.Concepts

import qualified Data.Drasil.Concepts.Documentation as Doc (appendix, assumption,
  charOfIR, client, customer, consVals, datumConstraint, functionalRequirement,
  generalSystemDescription, goalStmt, indPRCase, introduction, likelyChg,
  unlikelyChg, nonfunctionalRequirement, offShelfSolution, orgOfDoc, physSyst,
  prodUCTable, problemDescription, propOfCorSol, prpsOfDoc, reference, requirement,
  scpOfReq, scpOfTheProj, solutionCharSpec, specificsystemdescription,
  stakeholder, sysCont, systemConstraint, termAndDef, terminology, traceyMandG,
  tOfCont, tOfSymb, tOfUnit, userCharacteristic, refMat, abbAcc)
import qualified Data.Drasil.TheoryConcepts as Doc (dataDefn, genDefn, inModel, thModel)


-- Ordered by appearance in SRS.
-- | Standard SRS section builders.
tOfCont, refMat, tOfUnit, tOfSymb, tOfAbbAcc, intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, genSysDes, sysCont,
  userChar, sysCon, specSysDes, probDesc, termAndDefn, physSyst, goalStmt,
  solCharSpec, assumpt, thModel, genDefn, dataDefn, inModel, datCon, propCorSol,
  require, nonfuncReq, funcReq, likeChg, unlikeChg, traceyMandG, valsOfAuxCons,
  reference, appendix, offShelfSol, scpOfTheProj, prodUCTable, indPRCase,
  termogy :: Int -> [Contents] -> Section

-- | Table of Contents section.
tOfCont       d cs = section' d (titleize' Doc.tOfCont)                  cs tOfContLabel

-- | Reference Material section.
refMat        d cs = section' d (titleize Doc.refMat)                    cs refMatLabel
-- | Table of Units section.
tOfUnit       d cs = section' d (titleize' Doc.tOfUnit)                  cs tOfUnitLabel
-- | Table of Symbols section.
tOfSymb       d cs = section' d (titleize' Doc.tOfSymb)                  cs tOfSymbLabel
-- | Table of Abbreviations and Acronyms section.
tOfAbbAcc     d cs = section' d (titleize' Doc.abbAcc)                   cs tOfAbbAccLabel

-- | Introduction section.
intro         d cs = section' d (titleize Doc.introduction)              cs introLabel
-- | Purpose of Document section.
prpsOfDoc     d cs = section' d (titleize Doc.prpsOfDoc)                 cs docPurposeLabel
-- | Scope of Requirements section.
scpOfReq      d cs = section' d (titleize' Doc.scpOfReq)                 cs reqsScopeLabel
-- | Characteristics of Intended Reader section.
charOfIR      d cs = section' d (titleize' Doc.charOfIR)                 cs readerCharsLabel
-- | Organization of Document section.
orgOfDoc      d cs = section' d (titleize Doc.orgOfDoc)                  cs docOrgLabel

-- | Stakeholders section.
stakeholder   d cs = section' d (titleize' Doc.stakeholder)              cs stakeholderLabel
-- | The Customer section.
theCustomer   d cs = section' d (titleizeNP $ the Doc.customer)          cs customerLabel
-- | The Client section.
theClient     d cs = section' d (titleizeNP $ the Doc.client)            cs clientLabel

-- | General System Description section.
genSysDes     d cs = section' d (titleize Doc.generalSystemDescription)  cs genSysDescLabel
-- | System Context section.
sysCont       d cs = section' d (titleize Doc.sysCont)                   cs sysContextLabel
-- | User Characteristics section.
userChar      d cs = section' d (titleize' Doc.userCharacteristic)       cs userCharsLabel
-- | System Constraints section.
sysCon        d cs = section' d (titleize' Doc.systemConstraint)         cs sysConstraintsLabel

-- | Specific System Description section.
specSysDes    d cs = section' d (titleize Doc.specificsystemdescription) cs specSystDescLabel

-- | Problem Description section.
probDesc      d cs = section' d (titleize Doc.problemDescription)        cs probDescLabel
-- | Terminology and Definitions section.
termAndDefn   d cs = section' d (titleize' Doc.termAndDef)               cs termDefsLabel
-- | Physical System Description section.
physSyst      d cs = section' d (titleize Doc.physSyst)                  cs physSystLabel
-- | Goal Statement section.
goalStmt      d cs = section' d (titleize' Doc.goalStmt)                 cs goalStmtLabel

-- | Solution Characteristics Specification section.
solCharSpec   d cs = section' d (titleize Doc.solutionCharSpec)          cs solCharSpecLabel
-- | Assumptions section.
assumpt       d cs = section' d (titleize' Doc.assumption)               cs assumptLabel
-- | Theoretical Models section.
thModel       d cs = section' d (titleize' Doc.thModel)                  cs thModelLabel
-- | General Definitions section.
genDefn       d cs = section' d (titleize' Doc.genDefn)                  cs genDefnLabel
-- | Data Definitions section.
dataDefn      d cs = section' d (titleize' Doc.dataDefn)                 cs dataDefnLabel
-- | Instance Models section.
inModel       d cs = section' d (titleize' Doc.inModel)                  cs inModelLabel
-- | Data Constraints section.
datCon        d cs = section' d (titleize' Doc.datumConstraint)          cs datConLabel
-- | Properties of a Correct Solution section.
propCorSol    d cs = section' d (titleize' Doc.propOfCorSol)             cs corSolPropsLabel

-- | Requirements section.
require       d cs = section' d (titleize' Doc.requirement)              cs requirementsLabel
-- | Non-Functional Requirements section.
nonfuncReq    d cs = section' d (titleize' Doc.nonfunctionalRequirement) cs nonfuncReqLabel
-- | Functional Requirements section.
funcReq       d cs = section' d (titleize' Doc.functionalRequirement)    cs funcReqLabel

-- | Likely Changes section.
likeChg       d cs = section' d (titleize' Doc.likelyChg)                cs likeChgLabel
-- | Unlikely Changes section.
unlikeChg     d cs = section' d (titleize' Doc.unlikelyChg)              cs unlikeChgLabel

-- | Traceablilty Matrices and Graphs section.
traceyMandG   d cs = section' d (titleize' Doc.traceyMandG)              cs traceMatricesLabel
-- | Values of Auxiliary Constants section.
valsOfAuxCons d cs = section' d (titleize Doc.consVals)                  cs valsOfAuxConsLabel
-- | References section.
reference     d cs = section' d (titleize' Doc.reference)                cs referenceLabel
-- | Appendix section.
appendix      d cs = section' d (titleize Doc.appendix)                  cs appendixLabel
-- | Off-the-Shelf Solutions section.
offShelfSol   d cs = section' d (titleize' Doc.offShelfSolution)         cs offShelfSolnsLabel

-- Unused
-- | Scope of the Project section.
scpOfTheProj  d cs = section' d (atStart (Doc.scpOfTheProj titleize))    cs projScopeLabel
-- | Product Use Case Table section.
prodUCTable   d cs = section' d (titleize Doc.prodUCTable)               cs useCaseTableLabel
-- | Individual Product Use Case section.
indPRCase     d cs = section' d (titleize' Doc.indPRCase)                cs indPRCaseLabel
-- | Terminology section.
termogy       d cs = section' d (titleize Doc.terminology)               cs terminologyLabel

--Labels--
-- | Collections all 'Section' 'Reference's.
sectionReferences :: [Reference]
sectionReferences = [tOfContLabel, refMatLabel, tOfUnitLabel, tOfSymbLabel, tOfAbbAccLabel,
  introLabel, docPurposeLabel, reqsScopeLabel, readerCharsLabel, docOrgLabel,
  stakeholderLabel, clientLabel, customerLabel, genSysDescLabel, sysContextLabel,
  userCharsLabel, sysConstraintsLabel, specSystDescLabel, physSystLabel, probDescLabel,
  termDefsLabel, goalStmtLabel, solCharSpecLabel, assumptLabel, thModelLabel,
  genDefnLabel, dataDefnLabel, inModelLabel, datConLabel, corSolPropsLabel, requirementsLabel,
  funcReqLabel, nonfuncReqLabel, likeChgLabel, unlikeChgLabel, traceMatricesLabel,
  valsOfAuxConsLabel, referenceLabel, appendixLabel, offShelfSolnsLabel, indPRCaseLabel,
  projScopeLabel, useCaseTableLabel, terminologyLabel]

--FIXME: create using section information somehow?
-- | Makes a 'Reference' to a 'Section'.
tOfContLabel, refMatLabel, tOfUnitLabel, tOfSymbLabel, tOfAbbAccLabel,
  introLabel, docPurposeLabel, reqsScopeLabel, readerCharsLabel, docOrgLabel,
  stakeholderLabel, clientLabel, customerLabel, genSysDescLabel, sysContextLabel,
  userCharsLabel, sysConstraintsLabel, specSystDescLabel, physSystLabel, probDescLabel,
  termDefsLabel, goalStmtLabel, solCharSpecLabel, assumptLabel, thModelLabel,
  genDefnLabel, dataDefnLabel, inModelLabel, datConLabel, corSolPropsLabel, requirementsLabel,
  funcReqLabel, nonfuncReqLabel, likeChgLabel, unlikeChgLabel, traceMatricesLabel,
  valsOfAuxConsLabel, referenceLabel, appendixLabel, offShelfSolnsLabel, indPRCaseLabel,
  projScopeLabel, useCaseTableLabel, terminologyLabel :: Reference

tOfContLabel        = makeSecRef "ToC"              $ titleize' Doc.tOfCont

refMatLabel         = makeSecRef "RefMat"           $ titleize  Doc.refMat
tOfUnitLabel        = makeSecRef "ToU"              $ titleize' Doc.tOfUnit
tOfSymbLabel        = makeSecRef "ToS"              $ titleize' Doc.tOfSymb
tOfAbbAccLabel      = makeSecRef "TAbbAcc"          $ titleize' Doc.abbAcc

introLabel          = makeSecRef "Intro"            $ titleize  Doc.introduction
docPurposeLabel     = makeSecRef "DocPurpose"       $ titleize  Doc.prpsOfDoc
reqsScopeLabel      = makeSecRef "ReqsScope"        $ titleize' Doc.scpOfReq
readerCharsLabel    = makeSecRef "ReaderChars"      $ titleize' Doc.charOfIR
docOrgLabel         = makeSecRef "DocOrg"           $ titleize  Doc.orgOfDoc

stakeholderLabel    = makeSecRef "Stakeholder"      $ titleize' Doc.stakeholder
clientLabel         = makeSecRef "Client"           $ titleizeNP $ the Doc.client
customerLabel       = makeSecRef "Customer"         $ titleizeNP $ the Doc.customer

genSysDescLabel     = makeSecRef "GenSysDesc"       $ titleize  Doc.generalSystemDescription
sysContextLabel     = makeSecRef "SysContext"       $ titleize  Doc.sysCont
userCharsLabel      = makeSecRef "UserChars"        $ titleize' Doc.userCharacteristic
sysConstraintsLabel = makeSecRef "SysConstraints"   $ titleize' Doc.systemConstraint

specSystDescLabel   = makeSecRef "SpecSystDesc"     $ titleize  Doc.specificsystemdescription
physSystLabel       = makeSecRef "PhysSyst"         $ titleize  Doc.physSyst
probDescLabel       = makeSecRef "ProbDesc"         $ titleize  Doc.problemDescription
termDefsLabel       = makeSecRef "TermDefs"         $ titleize' Doc.termAndDef
goalStmtLabel       = makeSecRef "GoalStmt"         $ titleize' Doc.goalStmt

solCharSpecLabel    = makeSecRef "SolCharSpec"      $ titleize  Doc.solutionCharSpec
assumptLabel        = makeSecRef "Assumps"          $ titleize' Doc.assumption
thModelLabel        = makeSecRef "TMs"              $ titleize' Doc.thModel
genDefnLabel        = makeSecRef "GDs"              $ titleize' Doc.genDefn
dataDefnLabel       = makeSecRef "DDs"              $ titleize' Doc.dataDefn
inModelLabel        = makeSecRef "IMs"              $ titleize' Doc.inModel
datConLabel         = makeSecRef "DataConstraints"  $ titleize' Doc.datumConstraint
corSolPropsLabel    = makeSecRef "CorSolProps"      $ titleize' Doc.propOfCorSol

requirementsLabel   = makeSecRef "Requirements"     $ titleize' Doc.requirement
funcReqLabel        = makeSecRef "FRs"              $ titleize' Doc.functionalRequirement
nonfuncReqLabel     = makeSecRef "NFRs"             $ titleize' Doc.nonfunctionalRequirement

likeChgLabel        = makeSecRef "LCs"              $ titleize' Doc.likelyChg
unlikeChgLabel      = makeSecRef "UCs"              $ titleize' Doc.unlikelyChg

traceMatricesLabel  = makeSecRef "TraceMatrices"    $ titleize' Doc.traceyMandG
valsOfAuxConsLabel  = makeSecRef "AuxConstants"     $ titleize  Doc.consVals
referenceLabel      = makeSecRef "References"       $ titleize' Doc.reference 
appendixLabel       = makeSecRef "Appendix"         $ titleize  Doc.appendix
offShelfSolnsLabel  = makeSecRef "offShelfSolns"    $ titleize' Doc.offShelfSolution

-- Used only under People/Dan/Presentations/CommitteeMeeting4/BodyNew.hs
indPRCaseLabel      = makeSecRef "IndividualProdUC" $ titleize' Doc.indPRCase
-- Seem to be unused. Should they be deleted?
projScopeLabel      = makeSecRef "ProjScope"        $ atStart $ Doc.scpOfTheProj titleize
useCaseTableLabel   = makeSecRef "UseCaseTable"     $ titleize  Doc.prodUCTable
terminologyLabel    = makeSecRef "Terminology"      $ titleize  Doc.terminology