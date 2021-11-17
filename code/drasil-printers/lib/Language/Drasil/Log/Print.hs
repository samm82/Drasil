{-# LANGUAGE TypeApplications #-}

-- | Defines functions to help debug examples. Generated files appear in ./code/debug.
module Language.Drasil.Log.Print where

import Language.Drasil hiding (symbol)
import qualified Language.Drasil as L (symbol)
import Database.Drasil
import Utils.Drasil (stringList)

import qualified Data.Map as Map
import Control.Lens ((^.), view)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)
import Data.Function (on)

import Text.PrettyPrint.HughesPJ
import Language.Drasil.Plain.Print
import Language.Drasil.Printing.PrintingInformation
import Prelude hiding ((<>))
import Data.Typeable
import Theory.Drasil

-- * Main Function

-- | Gathers all printing functions and creates the debugging tables from them.
printAllDebugInfo :: PrintingInformation -> [Doc]
printAllDebugInfo pinfo = map (cdbSection . ($ pinfo)) [-- TODO: mkTableReferencedChunks, mkTableDepChunks, mkTableDepReffedChunks,
  mkTableSymb, mkTableOfTerms, mkTableConcepts, mkTableUnitDefn,
  mkTableDataDef, mkTableGenDef, mkTableTMod, mkTableIMod, mkTableCI,
  mkTableSec, mkTableLC, mkTableRef, renderUsedUIDs . mkListShowUsedUIDs]

-- * Helpers
-- ** Separators

-- | Debugging table separator.
cdbSection :: Doc -> Doc
cdbSection dd = text (replicate 100 '#' ++ "\n") $$ dd $$ text "\n"

-- | Header for debugging tables.
header :: Doc -> Doc
header d = text (replicate 100 '-') $$ d $$ text (replicate 100 '-')

-- TODO: These below should be simplified by factoring out code.

-- ** Table Generators
-- | General function to make the debugging tables. Takes in printing information, a function
-- that extracts a certain field from the printing information, a title, three column headers,
-- and three functions that sort the data from the printing information field into the 
-- required display formats (often 'UID's, terms, shortnames, definitions, etc.).
mkTableFromLenses :: Typeable a => PrintingInformation -> Proxy a
  -> String -> String -> String -> String -> (a -> Doc) -> (a -> Doc) -> (a -> Doc) -> Doc
mkTableFromLenses pm pa ttle h1 h2 h3 l1 l2 l3 =
  text ttle <> colon
  $$ header (text h1 $$ nest nestNum (text h2) $$ nest (nestNum*3) (text h3))
  $$ vcat (map chunkLayout chunks)
  where
    chunkLayout x = l1 x $$ nest nestNum (l2 x)
      $$ nest (nestNum*3) (l3 x)
    chunks = findAll (typeRep pa) $ pm ^. ckdb -- map (fst . snd) (Map.assocs $ tableLens db)
    nestNum = 30

-- | Makes a table with all symbolic quantities in the SRS.
mkTableSymb :: PrintingInformation -> Doc
mkTableSymb pinfo = mkTableFromLenses pinfo (Proxy @QuantityDict)
  "Symbol Chunks" "UID" "Term" "Symbol"
      (text . showUID)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
          (symbolDoc . flip L.symbol (pinfo ^. stg))

-- | Makes a table with terms in the SRS.
mkTableOfTerms :: PrintingInformation -> Doc
mkTableOfTerms pinfo = mkTableFromLenses pinfo (Proxy @IdeaDict)
  "Term Chunks" "UID" "Term" "Abbreviation"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (text . fromMaybe "" . getA)

-- | Makes a table with all concepts in the SRS.
mkTableConcepts :: PrintingInformation -> Doc
mkTableConcepts pinfo = mkTableFromLenses pinfo (Proxy @ConceptChunk)
  "Concepts" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all units used in the SRS.
mkTableUnitDefn :: PrintingInformation -> Doc
mkTableUnitDefn pinfo = mkTableFromLenses pinfo (Proxy @UnitDefn)
  "Unit Definitions" "UID" "Term" "Unit Symbol"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . Sy . usymb)

-- | Makes a table with all data definitions in the SRS.
mkTableDataDef :: PrintingInformation -> Doc
mkTableDataDef pinfo = mkTableFromLenses pinfo (Proxy @DataDefinition)
  "Data Definitions" "UID" "Term" "Symbol"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (symbolDoc . flip L.symbol (pinfo ^. stg))

-- | Makes a table with all general definitions in the SRS.
mkTableGenDef :: PrintingInformation -> Doc
mkTableGenDef pinfo = mkTableFromLenses pinfo (Proxy @GenDefn)
  "General Definitions" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all theoretical models in the SRS.
mkTableTMod :: PrintingInformation -> Doc
mkTableTMod pinfo = mkTableFromLenses pinfo (Proxy @TheoryModel)
  "Theory Models" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all instance models in the SRS.
mkTableIMod :: PrintingInformation -> Doc
mkTableIMod pinfo = mkTableFromLenses pinfo (Proxy @InstanceModel)
  "Instance Models" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all concept instances in the SRS.
mkTableCI :: PrintingInformation -> Doc
mkTableCI pinfo = mkTableFromLenses pinfo (Proxy @ConceptInstance)
  "ConceptInstance" "UID" "Term" "ShortName"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)

-- | Makes a table with all sections in the SRS.
mkTableSec :: PrintingInformation -> Doc
mkTableSec pinfo = mkTableFromLenses pinfo (Proxy @Section)
  "Sections" "UID" "Title" "ShortName"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . tle)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)

-- | Makes a table with all labelled content in the SRS.
mkTableLC :: PrintingInformation -> Doc
mkTableLC pinfo = mkTableFromLenses pinfo (Proxy @LabelledContent)
  "LabelledContent" "UID" "ShortName" "Type of Content"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)
        (text . getContConst . view accessContents)
  where
    getContConst :: RawContent -> String
    getContConst Table{} = "Table"
    getContConst Paragraph{} = "Paragraph"
    getContConst EqnBlock{} = "Equation"
    getContConst DerivBlock{} = "Derivation"
    getContConst Enumeration{} = "Enumeration"
    getContConst Defini{} = "Definition or Model"
    getContConst Figure{} = "Figure"
    getContConst Bib{} = "Bibliography"
    getContConst Graph{} = "Graph"

-- | Makes a table with all references in the SRS.
mkTableRef :: PrintingInformation -> Doc
mkTableRef pinfo = mkTableFromLenses pinfo (Proxy @Reference)
  "Reference" "UID" "Reference Address" "ShortName"
    (text . showUID)
      (text . getAdd . getRefAdd)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)

{- TODO: Fix later. I'll need to do a full analysis of this.

-- | Chunks that depend on other chunks. An empty list means the chunks do not depend on anything.
mkTableDepChunks :: PrintingInformation -> Doc
mkTableDepChunks PI{_ckdb = db} = text "Dependent Chunks (the chunks on the left use the chunks on the right in some capacity)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Dependent UIDs"))
  $$ vcat (map testIndepLayout traceMapUIDs)
  where
    testIndepLayout :: (UID, [UID]) -> Doc
    testIndepLayout (x, ys) = text (show x) $$ nest nestNum (text $ show ys)
    traceMapUIDs :: [(UID, [UID])]
    traceMapUIDs = Map.assocs $ db ^. traceTable
    nestNum = 30

-- | Chunks that are referenced and used by other chunks.
-- Those chunks build on top of the ones listed here.
mkTableReferencedChunks :: PrintingInformation -> Doc
mkTableReferencedChunks PI{_ckdb = db} = text "Referenced Chunks (other chunks build from these)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "UIDs that use the left UID"))
  $$ vcat (map testIsolateLayout refbyUIDs)
  where
    testIsolateLayout :: (UID, [UID]) -> Doc
    testIsolateLayout (x, ys) = text (show x) $$ nest nestNum (text $ show ys)
    refbyUIDs :: [(UID, [UID])]
    refbyUIDs = Map.assocs $ db ^. refbyTable
    nestNum = 30

-- | Chunks that use and are used by other chunks.
mkTableDepReffedChunks :: PrintingInformation -> Doc
mkTableDepReffedChunks PI{_ckdb = db} = text "Dependent and Referenced Chunks (chunks dependent on middle UIDs and used in the chunks on the right)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Dependent Chunk") $$ nest (nestNum*3) (text "Used-in Chunk"))
  $$ vcat (map traceRefLayout $ Map.assocs combinedMaps)
  where
    traceRefLayout :: (UID, ([UID], [UID])) -> Doc
    traceRefLayout x = text (show $ fst x) $$ nest nestNum (text $ show $ fst $ snd x)
      $$ nest (nestNum*3) (text $ show $ snd $ snd x)
    combinedMaps = Map.unionWith (\x y -> (fst x, snd y)) traceMapUIDs refByUIDs
    traceMapUIDs = Map.fromList $ map (\(x, y) -> (x, (y, []))) $ Map.assocs $ db ^. traceTable
    refByUIDs = Map.fromList $ map (\(x, y) -> (x, ([], y))) $ Map.assocs $ db ^. refbyTable
    nestNum = 30

-}

-- ** 'UID' Manipulation
-- | Creates a table of all UIDs and their "highest" recorded level of information. See 'mkListShowUsedUIDs'
-- for more details.
renderUsedUIDs :: [(UID, String)] -> Doc
renderUsedUIDs chs = header (text "UIDs" $$ nest 40 (text "Associated Chunks")) $$ vcat (map renderUsedUID chs)
  where
    renderUsedUID (u, chks) = text (show u) $$ nest 40 (text chks)


-- | For the last section of the log output. Shows which chunk UID is being used at which stage.
-- Note that chunks used at a "higher stage" (like 'Concept's and 'QuantityDict's) will still be built off of the
-- more basic types (like 'IdeaDict's), they are just not explicitly used in that manner.
-- Also, some chunks may have been "downgraded" when put into the database (for example, mapping a
-- 'QuantityDict' wrapper onto things like Constrained and Unital chunks happens often).
mkListShowUsedUIDs :: PrintingInformation -> [(UID, String)]
-- TODO: This function should be simplified.
mkListShowUsedUIDs PI{_ckdb = db} = sortBy (compare `on` fst) $
  map (second stringList) $ Map.toList $ Map.fromListWith (++) $
    map (\x -> (uid x, ["QuantityDict"]))      (findAll (typeRep (Proxy @QuantityDict))    db :: [QuantityDict]) ++
    map (\x -> (uid x, ["IdeaDict"]))          (findAll (typeRep (Proxy @IdeaDict))        db :: [IdeaDict]) ++
    map (\x -> (uid x, ["ConceptChunk"]))      (findAll (typeRep (Proxy @ConceptChunk))    db :: [ConceptChunk]) ++
    map (\x -> (uid x, ["UnitDefn"]))          (findAll (typeRep (Proxy @UnitDefn))        db :: [UnitDefn]) ++
    map (\x -> (uid x, ["DataDefinition"]))    (findAll (typeRep (Proxy @DataDefinition))  db :: [DataDefinition]) ++
    map (\x -> (uid x, ["InstanceModel"]))     (findAll (typeRep (Proxy @InstanceModel))   db :: [InstanceModel]) ++
    map (\x -> (uid x, ["GeneralDefinition"])) (findAll (typeRep (Proxy @GenDefn))         db :: [GenDefn]) ++
    map (\x -> (uid x, ["TheoryModel"]))       (findAll (typeRep (Proxy @TheoryModel))     db :: [TheoryModel]) ++
    map (\x -> (uid x, ["ConceptInstance"]))   (findAll (typeRep (Proxy @ConceptInstance)) db :: [ConceptInstance]) ++
    map (\x -> (uid x, ["Section"]))           (findAll (typeRep (Proxy @Section))         db :: [Section]) ++
    map (\x -> (uid x, ["LabelledContent"]))   (findAll (typeRep (Proxy @LabelledContent)) db :: [LabelledContent]) ++
    map (\x -> (uid x, ["Reference"]))         (findAll (typeRep Reference)                db :: [Reference])
