module Drasil.ChemCode.Body where

import Prelude hiding (product)
import Language.Drasil hiding (organization)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Drasil.DocLang.SRS as SRS
import Drasil.SRSDocument
import Theory.Drasil (GenDefn, InstanceModel)

import Data.Drasil.Citations (elemListWiki, ilpWiki, koothoor2013, lund2023,
  parnasClements1986, smithChemSpec, smithLai2005)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Documentation hiding (element, scope, srs)
import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.People (samCrawford)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.ChemCode.Assumptions (assumps)
import Drasil.ChemCode.Changes (uChanges)
import Drasil.ChemCode.DataDefs (dds)
import Drasil.ChemCode.Quantities (inputs, quants)
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
        -- IChar [] (undIR ++ appStanddIR) [],
        IOrgSec orgOfDocIntro inModel (SRS.inModel [] []) EmptyS
      ],
  -- StkhldrSec $
  --   StkhldrProg
  --     [Client glassBR $ phraseNP (a_ company)
  --       +:+. S "named Entuitive" +:+ S "It is developed by Dr." +:+ S (name mCampidelli),
  --     Cstmr glassBR],
  -- GSDSec $ GSDProg [SysCntxt [sysCtxIntro, LlC sysCtxFig, sysCtxDesc, sysCtxList],
  --   UsrChars [userCharacteristicsIntro], SystCons [] [] ],
  SSDSec $
    SSDProg
      [
        SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        -- , PhySysDesc glassBR physSystParts physSystFig []
        -- , Goals goalInputs
        ],
       SSDSolChSpec $ SCSProg
        [
        Assumptions, 
        TMs [] (Label : stdFields)
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
  -- AuxConstntSec $ AuxConsProg glassBR auxiliaryConstants,  
  Bibliography
  ]

tSymbIntro :: [TSIntro]
tSymbIntro = [TSPurpose, -- SymbConvention [Lit (nw chemistry)],
    SymbOrder -- VectorUnits
  ]

justification, scope, orgOfDocIntro, prob :: Sentence
justification = foldlSent [atStart chemical, plural equation,
  S "are common ways of representing", phrase chemical, plural reaction,
  S "but they must be balanced" +:+. refS lund2023, -- to ensure the Law of
  -- Conservation of Mass (\tmref{TM_ConsMass}) is observed
  S "This process of balancing a", phrase chemical, phrase equation,
  S "involves introducing coefficients before each", phrase chemical,
  S "formula such that there are the same number of atoms of each",
  phrase element `S.onThe` phrase reactant `S.and_` phrase product,
  S "sides" `S.ofThe` phrase chemical +:+. phrase equation,
  S "Because balancing must be done before a given", phrase chemical,
  phrase reaction, S "can be used", refS lund2023 `sC` S "it is useful to" +:+.
    S "have a tool to automatically do this", S "This would improve the",
  S "productivity of scientists and engineers" `S.and_` S "reduce the" +:+.
    S "potential for human error", S "This", phrase program,
  S "should balance a given", phrase chemical, phrase equation,
  S "if it is feasible" -- (see \nameref{sec_termsDefs})
    `S.and_` S "if it is not" `sC` S "it should provide a descriptive",
  phrase message, S "communicating this to the" +:+. phrase user,
  atStartNP (the program), S "that performs these tasks as documented here",
  S "will be called", introduceAbb progName
  ]

scope = foldlSent_ [
  S "all", phrase chemical, plural equation, S "with at most one more",
  phrase compound, S "than" +:+. phrase element, S "Furthermore" `sC` 
    S "it also includes all inputted", phrase chemical, S "formulas that",
  foldlList Comma List [
    foldlSent_ [S "describe real", phrase chemical, plural compound],
    S "are formatted following a set of conventions",
    S "only consist of atomic symbols and subscripts"
    ]
  ]

orgOfDocIntro = foldlSent [atStartNP (the organization), S "of this",
  phrase document, S "follows", phraseNP (the template), S "for an",
  getAcc Doc.srs, S "for", phrase sciCompS, S "proposed by",
  refS koothoor2013 `S.and_` refS smithLai2005]

prob = foldlSent_ [S "balance", phrase chemical, plural equation,
  S "with the smallest possible whole number coefficients",
  S "so they can be useful for other computations"]

symbolsAll :: [QuantityDict]
symbolsAll = quants

acronyms :: [CI]
acronyms = [progName, Doc.srs, thModel, dataDefn, requirement, unlikelyChg] -- genDefn, inModel

terms :: [ConceptChunk]
terms = [compound, element, equation, product, reactant, reaction]

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
      _constants   = [] :: [ConstQDef],
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
      map nw [program, algorithm] ++ -- ConceptChunk
      map nw doccon ++ map nw doccon' ++ map nw chemCon ++ map nw mathcon ++
      map nw acronyms ++ map nw symbolsAll)
    srsDomains
    ([] :: [UnitDefn])
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
citations = [elemListWiki, ilpWiki, koothoor2013, lund2023, parnasClements1986,
  smithChemSpec, smithLai2005]

concIns :: [ConceptInstance]
concIns = assumps ++ funcReqs ++ nonfuncReqs ++ uChanges

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]


-- MOVE TO CONCEPTS
progName :: CI -- name of example
progName = commonIdeaWithDict "progName" (pn "Chemistry Code") "ChemCode" []