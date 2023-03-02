module Drasil.ChemCode.Body where

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.People (samCrawford)
import Drasil.SRSDocument
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = []

si :: SystemInformation
si =
  SI
    { _sys = chemcode,
      _kind = Doc.srs,
      _authors = [samCrawford],
      _background = [],
      _purpose = [],
      _quants = [] :: [QuantityDict],
      _concepts = [] :: [DefinedQuantityDict],
      _instModels = [] :: [InstanceModel],
      _datadefs = [] :: [DataDefinition],
      _configFiles = [],
      _inputs = [] :: [QuantityDict],
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
    ([] :: [QuantityDict])
    [nw chemcode]
    ([] :: [ConceptChunk])
    ([] :: [UnitDefn])
    ([] :: [DataDefinition])
    ([] :: [InstanceModel])
    ([] :: [GenDefn])
    ([] :: [TheoryModel])
    ([] :: [ConceptInstance])
    ([] :: [Section])
    ([] :: [LabelledContent])
    ([] :: [Reference])

usedDB :: ChunkDB
usedDB =
  cdb
    ([] :: [QuantityDict])
    ([] :: [IdeaDict])
    ([] :: [ConceptChunk])
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
refDB = rdb [] []

-- MOVE TO CONCEPTS
chemcode :: CI -- name of example
chemcode = commonIdeaWithDict "ChemCode" (pn "ChemCode") "ChemCode" []
