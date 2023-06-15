module Drasil.ChemCode.Body where

import Prelude hiding (product)
import Language.Drasil hiding (number, matrix, organization)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Drasil.DocLang.SRS as SRS
import Drasil.SRSDocument
import Theory.Drasil (GenDefn, InstanceModel)

import Data.Drasil.Citations (chen2022, drasilSource, elemListWiki, hydrateSource,
  ilpWiki, inorganicIUPAC, koothoor2013, lund2023, maclachlan2021,
  nonIntCoeffSource, organicIUPAC, parnasClements1986, polymerSource,
  smithChemSpec, smithEtAl2007, smithKoothoor2016, smithLai2005)
import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Computation (algorithm)
import qualified Data.Drasil.Concepts.Documentation as Doc (scope, srs)
import Data.Drasil.Concepts.Documentation hiding (element, scope, srs)
import Data.Drasil.Concepts.Education (educon, highSchoolChemistry, thirdYear)
import Data.Drasil.Concepts.Math (mathcon, matrix, number, ode)
import Data.Drasil.Concepts.Software (program, reusability)
import Data.Drasil.People (samCrawford)
import Data.Drasil.SI_Units (mole)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.ChemCode.Assumptions (assumps)
import Drasil.ChemCode.Changes (lChanges, uChanges)
import Drasil.ChemCode.Concepts
import Drasil.ChemCode.DataDefs (dds)
import Drasil.ChemCode.Figures (physSysFig, sysCtxFig)
import Drasil.ChemCode.Goals (goals)
import Drasil.ChemCode.Quantities (constants, inputs, quants)
import Drasil.ChemCode.Requirements (funcReqs, nonfuncReqs)
import Drasil.ChemCode.TMods (tms)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $ RefProg intro [
    -- TUnits,
    tsymb tSymbIntro,
    TAandA
    ],
  IntroSec $
    IntroProg justification (short progName)
      [ IPurpose $ purpDoc progName Verbose,
        IScope scope,
        IChar [] readerChars [],
        IOrgSec inModel (SRS.inModel [] []) EmptyS
      ],
  -- StkhldrSec $
  --   StkhldrProg
  --     [Client glassBR $ phraseNP (a_ company)
  --       +:+. S "named Entuitive" +:+ S "It is developed by Dr." +:+ S (name mCampidelli),
  --     Cstmr glassBR],
  GSDSec $
    GSDProg
      [
        SysCntxt [sysCtxIntro, LlC sysCtxFig, sysCtxDesc, sysCtxList]
      , UsrChars [userChars]
      , SystCons [sysConstraints] []
      ],
  SSDSec $
    SSDProg
      [
        SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc progName physSysParts physSysFig [mkParagraph physSysFigNote]
        , Goals goalInputs
        ],
       SSDSolChSpec $ SCSProg
        [
        Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] [] HideDerivation
        , DDs [] ([Label, Symbol] ++ stdFields) HideDerivation -- FIXME: may want to change later
                                -- FIXME: may want to add Units later
        -- , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) HideDerivation
        -- , Constraints auxSpecSent inputDataConstraints
        -- , CorrSolnPpties [probBr, stressDistFac] []
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub EmptyS [],
        NonFReqsSub
      ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg progName constants,
  Bibliography
  ]

tSymbIntro :: [TSIntro]
tSymbIntro = [TSPurpose, -- SymbConvention [Lit (nw chemistry)],
    SymbOrder -- VectorUnits
  ]

justification, scope, orgOfDocIntro, prob :: Sentence
justification = foldlSent [atStart chemical, plural equation,
  S "are common ways of representing", phrase chemical, plural reaction +:+.
  sParen (refS physSysFig +:+ S "shows an example of a" +:+ phrase chemical +:+
    phrase equation), S "Subscripts indicate the number of atoms of each",
  phrase element, S "present in the given", phrase chemical +:+. phrase compound,
  S "A", phrase chemical, phrase equation, S "is", Quote (S "balanced"),
  S "if there are the same number of atoms of each", phrase element,
  S "before and after the", phrase reaction +:+. (S "takes place" `sC`
    S "which satisfies the Law of Conservation of Mass"), -- (\tmref{TM_ConsMass})
  atStart chemical, plural equation, S "are balanced by introducing",
  S "coefficients before each", phrase chemical, S "formula" +:+.
  sParen (S "this coefficient may be" +:+ Quote (S "1") `sC` S "in which" +:+
    S "case is implicit and not added to the" +:+ phrase equation),
  S "We want a tool to balance", phrase chemical, plural equation +:+.
    (S "automatically to improve the productivity of scientists and engineers" `S.and_`
      S "reduce the potential for human error"), S "This", phrase program,
  S "should balance a given", phrase chemical, phrase equation,
  S "if it is feasible" -- (see \nameref{sec_termsDefs})
    `S.and_` S "if it is not" `sC` S "it should provide a",
  phrase message, S "communicating this to the" +:+. phrase user,
  atStartNP (the program), S "that performs these tasks as documented here",
  S "will be called", introduceAbb progName
  ]

scope = foldlSent_ [
  S "all inputted", phrase chemical, plural equation, S "where the total",
  S "number of", phrase chemical, plural compound, S "is at most",
  S "one more than the total number of" +:+. plural element, S "These",
  phrase chemical, plural equation, S "will be balanced with integer" +:+.
  S "coefficients", S "The", phrase Doc.scope, S "also includes all inputted",
  phrase chemical, S "formulas that", foldlList Comma List [
    foldlSent_ [S "describe real", phrase chemical, plural compound],
    S "are formatted following a set of conventions",
    S "only consist of atomic symbols and subscripts" +:+ sParen (
      S "including the fractional subscripts in" +:+ plural nonStoicComp
    )]
  ]

orgOfDocIntro = foldlSent [atStartNP (the organization), S "of this",
  phrase document, S "follows", phraseNP (the template), S "for an",
  getAcc Doc.srs, S "for", phrase sciCompS, S "proposed by",
  refS koothoor2013 `S.and_` refS smithLai2005]

prob = foldlSent_ [S "balance", phrase chemical, plural equation,
  S "so they can be useful for other computations" +:+. refS lund2023,
  S "Additionally" `sC` S "since molecules only exist in positive integer",
  plural quantity, sParen (S "since dividing a molecule changes it into" +:+
    S "different molecules") `sC` S "the coefficients used to balance the",
  phrase chemical, phrase equation, S "should be whole numbers" `sC`
    S "and by convention should be as small as possible" +:+. refS lund2023,
  S "There are some cases where the coefficients are not integers",
  sParen (S "see" +:+ refS nonIntCoeffSource) `sC` S "but this is not in the",
  phrase Doc.scope, S "of this", phrase program]

readerChars :: [Sentence]
readerChars = [phrase highSchoolChemistry +:+ sParen (S "namely stochiometry"),
  phrase thirdYear +:+ S "linear optimization" +:+
    sParen (S "namely integer programming")]

userChars :: Contents
userChars = foldlSP [S "The end", phrase user `S.of_` short progName,
  S "should have an understanding of", phrase highSchoolChemistry,
  sParen (S "namely stochiometry")]

-- SYSTEM CONTEXT

sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig, S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside", phraseNP (the software) `sC`
    phraseNP (the user) +:+. S "in this case", S "A rectangle represents the",
   phrase softwareSys, S "itself" +:+. sParen (short progName),
   S "Arrows are used to show the data flow between the", phraseNP (system
   `andIts` environment)]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phraseNP (product_ `andThe` user),
   S "is through a user" +:+. phrase interface,
   S "The responsibilities of the", phraseNP (user `andThe` system),
   S "are as follows"]

sysCtxUsrResp :: [[Sentence]]
sysCtxUsrResp = [
    [S "Provide an unbalanced", phrase chemical, phrase equation `sC`
     S "ensuring conformation to", phrase input_, S "data format required by",
     short progName],
    -- [S "Ensure that consistent units are used for",
    --   pluralNP (combineNINI input_ variable)],
    [S "Ensure required" +:+ namedRef (SRS.assumpt [] []) (pluralNP
     (combineNINI software assumption)), S "are appropriate for the",
     phrase problem, S "to which the", phrase user, S "is applying the",
     phrase software]
  ]

sysCtxSysResp :: [[Sentence]]
sysCtxSysResp = [
    [S "Detect data type mismatch" `sC` S "such as a string of characters",
     S "instead of a floating point", phrase number],
    [S "Format the inputted", phrase chemical, phrase equation, S "as a",
     phrase matrix],
    [S "If the inputted", phrase chemical, phrase equation, S "is feasible" `sC`
      S "find its balanced form with the smallest possible whole number coefficients"]
  ]

sysCtxExtLibResp :: [[Sentence]]
sysCtxExtLibResp = [
    [S "Solve the integer linear programming problem for the inputted",
     phrase chemical, phrase equation]
  ]

sysCtxResp :: [Sentence]
sysCtxResp = map (\x -> x +:+ S "Responsibilities") [titleize user,
  short progName, S "External" +:+ titleize library]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map (bulletFlat . map foldlSent_) [sysCtxUsrResp, sysCtxSysResp, sysCtxExtLibResp]

-- SYSTEM CONSTRAINTS

sysConstraints :: Contents
sysConstraints = foldlSP [short progName, S "will be developed using Drasil",
  refS drasilSource `sC` Quote (foldlSent_ [S "a framework for generating high-quality",
    phrase documentation `S.and_` phrase code, S "for Scientific Computing Software"]),
  refS maclachlan2021 `sC` S "with the goal of extending it by adding concepts", --FIXME: ref from page "iii"
  S "relevant to the problem outlined in the" +:+.
  namedRef (SRS.probDesc ([]::[Contents]) ([]::[Section])) (phrase problemDescription),
  S "Since Drasil is built on the idea of", phrase reusability `sC` S "external" +:+.
  S "libraries will be used to solve the integer programming problems",
  S "This was previously done with", phrase ode, sParen (short ode), S "solvers" `sC`
  S "since", Quote (foldlSent_ [S "creating a complete", short ode, S "solver in Drasil",
    S "would take considerable time" `sC`S "and there are already many reliable external",
    S "libraries ... tested by long use"]) +:+. complexRef chen2022 (Page [24]),
  S "These rationales also apply to ILP solvers"]

physSysParts :: [Sentence]
physSysParts = map foldlSent[
    [S "The", plural reactant, S "of a given", phrase chemical, phrase reaction],
    [S "The", plural product, S "of the same", phrase chemical, phrase reaction]
  ]

physSysFigNote :: Sentence
physSysFigNote = foldlSent [refS physSysFig, S "was partially generated by ChatGPT"]

goalInputs :: [Sentence]
goalInputs = map (\x -> S "a" +:+ phrase x) inputs

symbolsAll :: [QuantityDict]
symbolsAll = quants ++ map qw constants

acronyms :: [CI]
acronyms = [assumption, progName, Doc.srs, thModel, dataDefn, requirement,
  unlikelyChg, ode, iupac] -- genDefn, inModel

terms :: [ConceptChunk]
terms = [balanced, compound, element, equation, feasible, formula, hydrate,
  isotope, nonStoicComp, polyIon, polymer, product, reactant, reaction,
  stoichiometry]

units :: [UnitDefn]
units = [mole]

si :: SystemInformation
si =
  SI
    { _sys         = progName,
      _kind        = Doc.srs,
      _authors     = [samCrawford],
      _background  = [],
      _purpose     = [],
      _quants      = symbolsAll,
      _concepts    = [] :: [DefinedQuantityDict],
      _instModels  = [] :: [InstanceModel],
      _datadefs    = dds,
      _configFiles = [],
      _inputs      = inputs,
      _outputs     = [] :: [QuantityDict],
      _defSequence = [] :: [Block SimpleQDef],
      _constraints = [] :: [ConstrainedChunk],
      _constants   = constants,
      _sysinfodb   = symbMap,
      _usedinfodb  = usedDB,
       refdb       = refDB
    }

symbMap :: ChunkDB
symbMap =
  cdb
    symbolsAll
    (nw progName : -- CI
      nw sciCompS : -- NamedChunk
      map nw [algorithm, balanced, program, reusability] ++ -- ConceptChunk
      map nw units ++ -- UnitDefn
      map nw doccon ++ map nw doccon' ++ map nw chemCon ++ map nw mathcon ++
      map nw educon ++ map nw acronyms ++ map nw symbolsAll)
    srsDomains
    units
    dds
    ([] :: [InstanceModel])
    ([] :: [GenDefn])
    tms
    concIns
    ([] :: [Section])
    ([] :: [LabelledContent])
    ([] :: [Reference])

usedDB :: ChunkDB
usedDB =
  cdb
    ([] :: [QuantityDict])
    (map nw acronyms ++ map nw symbolsAll)
    srsDomains
    ([] :: [UnitDefn])
    dds
    ([] :: [InstanceModel])
    ([] :: [GenDefn])
    tms
    ([] :: [ConceptInstance])
    ([] :: [Section])
    ([] :: [LabelledContent])
    ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations concIns

citations :: BibRef
citations = [chen2022, drasilSource, elemListWiki, hydrateSource, ilpWiki,
  inorganicIUPAC, koothoor2013, lund2023, maclachlan2021, nonIntCoeffSource,
  organicIUPAC, parnasClements1986, polymerSource, smithChemSpec,
  smithEtAl2007, smithKoothoor2016, smithLai2005]

concIns :: [ConceptInstance]
concIns = assumps ++ goals ++ funcReqs ++ nonfuncReqs ++ lChanges ++ uChanges

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]