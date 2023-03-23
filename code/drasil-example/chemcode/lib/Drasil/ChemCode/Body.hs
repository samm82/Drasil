module Drasil.ChemCode.Body where

import Prelude hiding (product)
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.SRSDocument
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Data.Drasil.Citations (lund2023)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Documentation hiding (element, scope, srs)
import Data.Drasil.Concepts.Chemistry
import Data.Drasil.Concepts.Math (mathcon)
import Data.Drasil.Concepts.Software (program)

import Data.Drasil.People (samCrawford)

import Drasil.ChemCode.Requirements (funcReqs, nonfuncReqs)
import Drasil.ChemCode.Quantities (inputs)

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
      [ IScope scope ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub EmptyS [],
        NonFReqsSub
      ],
  Bibliography
  ]

tSymbIntro :: [TSIntro]
tSymbIntro = [TSPurpose, -- SymbConvention [Lit (nw chemistry)],
    SymbOrder -- VectorUnits
  ]

justification, scope :: Sentence
justification = foldlSent [atStart chemical, plural equation,
  S "are common ways of representing", phrase chemical, plural reaction,
  S "but they must be balanced" +:+. refS lund2023, -- to ensure the Law of
  -- Conservation of Mass (\tmref{TM_ConsMass}) is observed
  S "This process of balancing a", phrase chemical, phrase equation,
  S "involves introducing coefficients before each", phrase chemical,
  S "formula such that there are the same number of atoms of each",
  phrase element `S.onThe` phrase reactant `S.and_` phrase product,
  S "sides" `S.ofThe` phrase chemical +:+. phrase equation,
  
  atStartNP (the program), S "documented here is called", introduceAbb progName
  ]
-- Because balancing must be done before a given
-- chemical reaction can be used \cite{lund_introduction_2023}, it is useful to
-- have a tool to automatically do this. This would improve the
-- productivity of scientists and engineers and reduce the potential for human
-- error. This program should balance a given chemical reaction if it is
-- feasible (see \nameref{sec_termsDefs}), and if not, it should provide a
-- descriptive message communicating this to the user. The program that performs
-- these tasks as specified by this document will be called ``\progname{}''.

-- The following section provides an overview of the Software Requirements
-- Specification (SRS) for \progname{}. This section explains the purpose of this
-- document, the scope of the requirements, the characteristics of the intended
-- reader, and the organization of the document.
  
  -- foldlSent [atStart projectile, phrase motion, S "is a common" +:+.
  -- phraseNP (problem `in_` physics), S "Therefore, it is useful to have a",
  -- phrase program, S "to solve and model these types of" +:+. plural problem,
  -- atStartNP (the program), S "documented here is called", phrase projectileTitle]
scope = foldlSent_ [
  S "all", phrase chemical, plural equation, S "with at most one more", phrase compound,
  S "than", phrase element
  ]

symbolsAll :: [QuantityDict]
symbolsAll = inputs

acronyms :: [CI]
acronyms = [progName]

si :: SystemInformation
si =
  SI
    { _sys = progName,
      _kind = Doc.srs,
      _authors = [samCrawford],
      _background = [],
      _purpose = [],
      _quants = symbolsAll,
      _concepts = [] :: [DefinedQuantityDict],
      _instModels = [] :: [InstanceModel],
      _datadefs = [] :: [DataDefinition],
      _configFiles = [],
      _inputs = inputs,
      _outputs = [] :: [QuantityDict],
      _defSequence = [] :: [Block SimpleQDef],
      _constraints = [] :: [ConstrainedChunk],
      _constants = [] :: [ConstQDef],
      _sysinfodb = symbMap,
      _usedinfodb = usedDB,
      refdb = refDB
    }

symbMap :: ChunkDB
symbMap =
  cdb
    symbolsAll
    (nw progName : nw program : map nw doccon ++ map nw doccon' ++
      map nw chemCon ++ map nw mathcon ++ map nw acronyms ++ map nw symbolsAll)
    srsDomains
    ([] :: [UnitDefn])
    ([] :: [DataDefinition])
    ([] :: [InstanceModel])
    ([] :: [GenDefn])
    ([] :: [TheoryModel])
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
    ([] :: [DataDefinition])
    ([] :: [InstanceModel])
    ([] :: [GenDefn])
    ([] :: [TheoryModel])
    ([] :: [ConceptInstance])
    ([] :: [Section])
    ([] :: [LabelledContent])
    ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb [lund2023] concIns

concIns :: [ConceptInstance]
concIns = funcReqs ++ nonfuncReqs

-- MOVE TO CONCEPTS
progName :: CI -- name of example
progName = commonIdeaWithDict "progName" (pn "Chemistry Code") "ChemCode" []
