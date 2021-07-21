{-# Language TupleSections #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Drasil.DocDecl (SRSDecl, mkDocDesc)
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), DocDesc, DocSection(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(..), IntroSec(..), IntroSub(..), LCsSec(..), LFunc(..),
  PDSub(..), ProblemDescription(..), RefSec(..), RefTab(..), ReqrmntSec(..),
  ReqsSub(..), SCSSub(..), StkhldrSec(..), StkhldrSub(..), SolChSpec(..),
  SSDSec(..), SSDSub(..), TraceabilitySec(..), TraceConfig(..),
  TSIntro(..), UCsSec(..))
import Drasil.DocumentLanguage.Definitions (ddefn, derivation, instanceModel,
  gdefn, tmodel)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)

import Language.Drasil
import Language.Drasil.Display (compsy)
import Utils.Drasil

import Database.Drasil(ChunkDB, SystemInformation(SI), _authors, _kind,
  _quants, _sys, _folderPath, _sysinfodb, _usedinfodb, ccss, ccss', citeDB, collectUnits,
  termTable)

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccGen)
import Drasil.Sections.TableOfContents (toToC, findToC)
import Drasil.Sections.TableOfSymbols (table, tsIntro)
import Drasil.Sections.TableOfUnits (tOfUnitSIName, tuIntro, defaultTUI)
import qualified Drasil.DocLang.SRS as SRS (appendix, genDefn,
  genSysDes, likeChg, unlikeChg, reference, solCharSpec,
  stakeholder, tOfCont, tOfSymb, tOfUnit, userChar, offShelfSol, refMat,
  tOfAbbAcc)
import Drasil.DocLang.References (fillcdbSRS)
import qualified Drasil.Sections.AuxiliaryConstants as AC (valsOfAuxConstantsF)
import qualified Drasil.Sections.GeneralSystDesc as GSD (genSysIntro,
  systCon, usrCharsF, sysContxt)
import qualified Drasil.Sections.Introduction as Intro (charIntRdrF,
  introductionSection, orgSec, purposeOfDoc, scopeOfRequirements)
import qualified Drasil.Sections.Requirements as R (reqF, fReqF, nfReqF)
import qualified Drasil.Sections.SpecificSystemDescription as SSD (assumpF,
  datConF, dataDefnF, genDefnF, goalStmtF, inModelF, physSystDesc, probDescF,
  propCorSolF, solutionCharSpecIntro, specSysDescr, termDefnF, thModF, helperCI,
  tmStub, ddStub, imStub, pdStub)
import qualified Drasil.Sections.Stakeholders as Stk (stakeholderIntro,
  tClientF, tCustomerF)
import qualified Drasil.DocumentLanguage.TraceabilityMatrix as TM (
  generateTraceTableView)
import qualified Drasil.DocumentLanguage.TraceabilityGraph as TG (traceMGF)

import qualified Data.Drasil.Concepts.Documentation as Doc (likelyChg, section_,
  software, unlikelyChg)


import Control.Lens ((^.))
import Data.Function (on)
import Data.List (nub, sortBy)
import qualified Data.Map as Map (elems)

----- Gather all information necessary to create a document -----
-- | Creates a document from a document description, a title combinator function, and system information.
mkDoc :: SRSDecl -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc dd comb si@SI {_sys = sys, _kind = kind, _authors = authors} =
  Document (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) (findToC l) $
  mkSections fullSI l where
    fullSI = fillcdbSRS dd si
    l = mkDocDesc fullSI dd

-- | Constructs the unit definitions ('UnitDefn's) found in the document description ('DocDesc') from a database ('ChunkDB').
extractUnits :: DocDesc -> ChunkDB -> [UnitDefn]
extractUnits dd cdb = collectUnits cdb $ ccss' (getDocDesc dd) (egetDocDesc dd) cdb

----- Section creators -----

-- | Helper for creating the different document sections.
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: DocSection -> Section
    doit TableOfContents      = mkToC dd
    doit (RefSec rs)          = mkRefSec si dd rs
    doit (IntroSec is)        = mkIntroSec si is
    doit (StkhldrSec sts)     = mkStkhldrSec sts
    doit (SSDSec ss)          = mkSSDSec si ss
    doit (AuxConstntSec acs)  = mkAuxConsSec acs 
    doit Bibliography         = mkBib (citeDB si)
    doit (GSDSec gs')         = mkGSDSec gs'
    doit (ReqrmntSec r)       = mkReqrmntSec r
    doit (LCsSec lc)          = mkLCsSec lc
    doit (UCsSec ulcs)        = mkUCsSec ulcs
    doit (TraceabilitySec t)  = mkTraceabilitySec t si
    doit (AppndxSec a)        = mkAppndxSec a
    doit (OffShelfSolnsSec o) = mkOffShelfSolnSec o

{--}

-- | Helper for making the Table of Contents section.
mkToC :: DocDesc -> Section
mkToC dd = SRS.tOfCont [intro, UlC $ ulcc $ Enumeration $ Bullet $ map ((, Nothing) . toToC) dd] []
  where
    intro = mkParagraph $ S "An outline of all sections included in this SRS is recorded here for easy reference."

{--}

-- | Helper for creating the reference section and subsections.
-- Includes Table of Symbols, Units and Abbreviations and Acronyms.
mkRefSec :: SystemInformation -> DocDesc -> RefSec -> Section
mkRefSec si dd (RefProg c l) = SRS.refMat [c] (map (mkSubRef si) l)
  where
    mkSubRef :: SystemInformation -> RefTab -> Section
    mkSubRef si' TUnits = mkSubRef si' $ TUnits' defaultTUI tOfUnitSIName
    mkSubRef SI {_sysinfodb = db} (TUnits' con f) =
        SRS.tOfUnit [tuIntro con, LlC $ f (nub $ sortBy compUnitDefn $ extractUnits dd db)] []
    -- FIXME: _quants = v should be removed from this binding and symbols should
    -- be acquired solely through document traversal, however #1658. If we do
    -- just the doc traversal here, then we lose some symbols which only appear
    -- in a table in GlassBR. If we grab symbols from tables (by removing the `isVar`)
    -- in ExtractDocDesc, then the passes which extract `DefinedQuantityDict`s will
    -- error out because some of the symbols in tables are only `QuantityDict`s, and thus
    -- missing a `Concept`.
    mkSubRef SI {_quants = v, _sysinfodb = cdb} (TSymb con) =
      SRS.tOfSymb 
      [tsIntro con,
                LlC $ table Equational (sortBySymbol
                $ filter (`hasStageSymbol` Equational) 
                (nub $ map qw v ++ ccss' (getDocDesc dd) (egetDocDesc dd) cdb))
                atStart] []
    mkSubRef SI {_sysinfodb = cdb} (TSymb' f con) =
      mkTSymb (ccss (getDocDesc dd) (egetDocDesc dd) cdb) f con
    mkSubRef SI {_usedinfodb = db} TAandA =
      SRS.tOfAbbAcc [LlC $ tableAbbAccGen $ nub $ map fst $ Map.elems $ termTable db] []

-- | Helper for creating the table of symbols.
mkTSymb :: (Quantity e, Concept e, Eq e, MayHaveUnit e) =>
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = SRS.tOfSymb [tsIntro c,
  LlC $ table Equational
    (sortBy (compsy `on` eqSymb) $ filter (`hasStageSymbol` Equational) (nub v))
    (lf f)] 
    []
  where lf Term = atStart
        lf Defn = capSent . (^. defn)
        lf (TermExcept cs) = \x -> if (x ^. uid) `elem` map (^. uid) cs then
          capSent (x ^. defn) else atStart x --Compare chunk uids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = \x -> if (x ^. uid) `elem` map (^.uid) cs then
          atStart x else capSent (x ^. defn)
        lf TAD = \tDef -> titleize tDef +: EmptyS +:+. capSent (tDef ^. defn)

{--}

-- | Makes the Introduction section into a 'Section'.
mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec si (IntroProg probIntro progDefn l) =
  Intro.introductionSection probIntro progDefn $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> Section
    mkSubIntro _ (IPurpose intro) = Intro.purposeOfDoc intro
    mkSubIntro _ (IScope main) = Intro.scopeOfRequirements main
    mkSubIntro SI {_sys = sys} (IChar assumed topic asset) =
      Intro.charIntRdrF sys assumed topic asset (SRS.userChar [] [])
    mkSubIntro _ (IOrgSec i b s t) = Intro.orgSec i b s t
    -- FIXME: s should be "looked up" using "b" once we have all sections being generated

{--}

-- | Helper for making the Stakeholders section.
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrProg l) = SRS.stakeholder [Stk.stakeholderIntro] $ map mkSubs l
  where
    mkSubs :: StkhldrSub -> Section
    mkSubs (Client kWrd details) = Stk.tClientF kWrd details
    mkSubs (Cstmr kWrd)          = Stk.tCustomerF kWrd

{--}

-- | Helper for making the General System Description section.
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDProg l) = SRS.genSysDes [GSD.genSysIntro] $ map mkSubs l
   where
     mkSubs :: GSDSub -> Section
     mkSubs (SysCntxt cs)            = GSD.sysContxt cs
     mkSubs (UsrChars intro)         = GSD.usrCharsF intro
     mkSubs (SystCons cntnts subsec) = GSD.systCon cntnts subsec

{--}

-- | Helper for making the Specific System Description section.
mkSSDSec :: SystemInformation -> SSDSec -> Section
mkSSDSec si (SSDProg l) =
  SSD.specSysDescr $ map (mkSubSSD si) l
  where
    mkSubSSD :: SystemInformation -> SSDSub -> Section
    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

-- | Helper for making the Specific System Description Problem section.
mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDProg prob subSec subPD) = SSD.probDescF prob (subSec ++ map mkSubPD subPD)
  where mkSubPD (TermsAndDefs sen concepts) = SSD.termDefnF sen concepts
        mkSubPD (PhySysDesc prog parts dif extra) = SSD.physSystDesc prog parts dif extra
        mkSubPD (Goals ins g) = SSD.goalStmtF ins (mkEnumSimpleD g)

-- | Helper for making the Solution Characteristics Specification section.
mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec si (SCSProg l) =
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) SSD.imStub] $
    map (mkSubSCS si) l
  where
    mkSubSCS :: SystemInformation -> SCSSub -> Section
    mkSubSCS _ (TMs _ _ [])   = error "There are no Theoretical Models"
    mkSubSCS _ (GDs _ _ [] _) = SSD.genDefnF []
    mkSubSCS _ (DDs _ _ [] _) = error "There are no Data Definitions"
    mkSubSCS _ (IMs _ _ [] _)  = error "There are no Instance Models"
    mkSubSCS si' (TMs intro fields ts) =
      SSD.thModF (siSys si') $ map mkParagraph intro ++ map (LlC . tmodel fields si') ts
    mkSubSCS si' (DDs intro fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ concatMap (\x -> [LlC $ ddefn fields si' x, derivation x]) dds
    mkSubSCS si' (DDs intro fields dds _) =
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ map (LlC . ddefn fields si') dds
    mkSubSCS si' (GDs intro fields gs' ShowDerivation) =
      SSD.genDefnF $ map mkParagraph intro ++ concatMap (\x -> [LlC $ gdefn fields si' x, derivation x]) gs'
    mkSubSCS si' (GDs intro fields gs' _) =
      SSD.genDefnF $ map mkParagraph intro ++ map (LlC . gdefn fields si') gs'
    mkSubSCS si' (IMs intro fields ims ShowDerivation) =
      SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      concatMap (\x -> [LlC $ instanceModel fields si' x, derivation x]) ims
    mkSubSCS si' (IMs intro fields ims _) =
      SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      map (LlC . instanceModel fields si') ims
    mkSubSCS si' (Assumptions ci) =
      SSD.assumpF $ mkEnumSimpleD $ map (`SSD.helperCI` si') ci
    mkSubSCS _ (Constraints end cs)  = SSD.datConF end cs
    mkSubSCS _ (CorrSolnPpties c cs) = SSD.propCorSolF c cs
    siSys :: SystemInformation -> IdeaDict
    siSys SI {_sys = sys} = nw sys

{--}

-- | Helper for making the Requirements section.
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsProg l) = R.reqF $ map mkSubs l
  where
    mkSubs :: ReqsSub -> Section
    mkSubs (FReqsSub  frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (FReqsSub' frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (NonFReqsSub nfrs) = R.nfReqF (mkEnumSimpleD nfrs)

{--}

-- | Helper for making the Likely Changes section.
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsProg c) = SRS.likeChg (intro : mkEnumSimpleD c) []
  where intro = foldlSP [S "This", phrase Doc.section_, S "lists the",
                plural Doc.likelyChg, S "to be made to the", phrase Doc.software]

{--}

-- | Helper for making the Unikely Changes section.
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsProg c) = SRS.unlikeChg (intro : mkEnumSimpleD c) []
  where intro = foldlSP [S "This", phrase Doc.section_, S "lists the",
                plural Doc.unlikelyChg, S "to be made to the", phrase Doc.software]

{--}

-- | Helper for making the Traceability Matrices and Graphs section.
mkTraceabilitySec :: TraceabilitySec -> SystemInformation -> Section
mkTraceabilitySec (TraceabilityProg progs) si = TG.traceMGF trace
  (map (\(TraceConfig _ pre _ _ _) -> foldlList Comma List pre) progs)
  (map LlC trace) (_folderPath si) []
  where
  trace = map (\(TraceConfig u _ desc rows cols) -> TM.generateTraceTableView
    u desc rows cols si) progs

{--}

-- | Helper for making the Off-the-Shelf Solutions section.
mkOffShelfSolnSec :: OffShelfSolnsSec -> Section
mkOffShelfSolnSec (OffShelfSolnsProg cs) = SRS.offShelfSol cs [] 

{--}

-- | Helper for making the Values of Auxiliary Constants section.
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsProg key listOfCons) = AC.valsOfAuxConstantsF key $ sortBySymbol listOfCons

{--}

-- | Helper for making the References section.
mkBib :: BibRef -> Section
mkBib bib = SRS.reference [UlC $ ulcc (Bib bib)] []

{--}

-- | Helper for making the Appendix section.
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxProg cs) = SRS.appendix cs []
