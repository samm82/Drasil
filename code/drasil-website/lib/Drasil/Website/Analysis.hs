-- | Contains all the information needed about the structure
-- of the Drasil framework to be displayed on the Drasil website.
module Drasil.Website.Analysis where

import Language.Drasil


-- * Analysis Section
--
-- $analysis
--
-- Holds functions that are made for the Analysis section of the Drasil Website,
-- including the section creator functions, title & introduction, and references.


-- | Creates the Analysis of Drasil Section. This section is split into the following sub-sections:
--
--    * Data Table (spreadsheet that contains all the information about classes, types, and instances)
--    * Type Graphs (graphs showing type dependencies)
--    * Class Instance Graphs (graphs showing the relationships between types and classes, taken from Data Table)
--    * Package Dependency Graphs (structure of modules within each @drasil-@ package)
analysisSec :: FilePath -> FilePath -> FilePath -> FilePath -> [String] -> Section
analysisSec analysisPath typePath clsIPath graphPath pkgs = 
    section 0 drasilAnalysisTitle -- Section title
    [mkParagraph analysisIntro] -- Section introduction
    [dataTableSec analysisPath, tableOfGraphsSec typePath clsIPath pkgs,
      graphSec graphPath $ map ("drasil-" ++) pkgs] -- Subsections
    $ makeSecRef "Analysis" $ S "Analysis" -- Section Reference

-- | Analysis section title.
drasilAnalysisTitle :: Sentence
drasilAnalysisTitle = S "Analysis of Drasil"

-- | Analysis section introduction.
analysisIntro :: Sentence
analysisIntro = S "This section contains an graphs and tables that may be used to analyze the \
  \structure of the Drasil framework. Here, we will explore the relationship between data types, \
  \classes, and instances of those classes within Drasil, as well as the structure of individual \
  \Drasil packages."

-- | Gathers all references used in this file.
analysisRefs :: FilePath -> FilePath -> FilePath -> FilePath -> [String] -> [Reference]
analysisRefs analysisPath typePath clsIPath graphPath pkgs = 
  [dataTableHTMLRef analysisPath, dataTableCSVRef analysisPath]
  ++ map (getGraphsInTableRef "datatype" "" typePath) pkgs
  ++ map (getGraphsInTableRef "classInst" "" clsIPath) pkgs
  ++ map (getGraphsInTableRef "datatype" "circo_" typePath) pkgs
  ++ map (getGraphsInTableRef "classInst" "circo_" clsIPath) pkgs
  ++ drasilDepGraphRefs graphPath (map ("drasil-" ++) pkgs)

-- * Data Table Subsection (Intersections of Types and Classes)
--
-- $dataTable
--
-- Contains a large spreadsheet of all the types, classes, and class instances in Drasil,
-- as well as the packages in which they are defined or used.

-- | Data Table subsection.
dataTableSec :: FilePath -> Section
dataTableSec path = 
  section 1 dataTableTitle -- Title
  [mkParagraph $ dataTableDesc path] -- Contents
  [] $ makeSecRef "DataTable" $ S "DataTable" -- Section reference

-- | Data Table subsection title.
dataTableTitle :: Sentence
dataTableTitle = S "Intersections of Types and Classes"

-- | Data table description. Explains the purpose, organization,
-- and links to a downloadable version.
dataTableDesc :: FilePath -> Sentence
dataTableDesc path = S "This" +:+ namedRef (dataTableHTMLRef path) (S "Data Table") +:+
  S "is generated by Drasil to keep track of all the different types, classes, and where they intersect through instances. \
  \The rows are organized in order of Drasil packages, modules, and data types. \
  \The data types are further separated by their composition; those labelled \
  \Data Type are completely new types created and used in Drasil, while Newtype Types are \
  \type synonyms. All of the classes in Drasil are defined as \
  \column headers, starting from Haskell-native classes like Eq and going through every \
  \unique Drasil-defined class. A box marked with \
  \'YYYY' symbolizes the file location of where that particular data type is an instance of a particular class. \
  \There is also a" +:+ namedRef (dataTableCSVRef path) (S "downloadable version") +:+ S "of the Data Table available as a .csv file."

-- | Data table references.
dataTableHTMLRef, dataTableCSVRef :: FilePath -> Reference
-- | HTML table.
dataTableHTMLRef path = makeURI "dataTableHTML" (path ++ "ClassInstDep/DataTable.html") (shortname' $ S "dataTableHTML")
-- | Downloadable .csv file.
dataTableCSVRef path = makeURI "dataTableCSV" (path ++ "ClassInstDep/DataTable.csv") (shortname' $ S "dataTableCSV")

-- * Table of Graphs Subsection
--
-- $tableOfGraphs
--
-- Generates two tables linked to all the generated dot graphs of Drasil.
-- The first table only shows the relationship between types, while
-- the second table shows the relationship between types and classes.

-- | Table of Graphs section. Contains a table for Type dependencies and Class-Instance relations.
tableOfGraphsSec :: FilePath -> FilePath -> [String] -> Section
tableOfGraphsSec typePath clsIPath pkgs = 
  section 1 tableOfGraphsTitle -- Title
  [mkParagraph tableOfGraphsDescType, mkParagraph tableOfGraphsDescClassInst, mkGraphsTable typePath clsIPath pkgs] -- Contents
  [] $ makeSecRef "TypeAndClassGraphs" $ S "TypeAndClassGraphs" -- Section reference

-- | Table of Graphs title.
tableOfGraphsTitle :: Sentence
tableOfGraphsTitle = S "Table of Graphs"

-- | Helper to create a graph table based on the kind
-- (either "datatype" or "classInst"), path, and packages.
graphTable :: String -> FilePath -> String -> FilePath -> [String] -> [[Sentence]]
graphTable knd1 path1 knd2 path2 = map (graphTableEntry knd1 path1 knd2 path2)

-- | Helper to create a row in a graph table. Based on the kind of table we want,
-- the file path to that graph, and the package name.
graphTableEntry :: String -> FilePath -> String -> FilePath -> String -> [Sentence]
graphTableEntry knd1 path1 knd2 path2 pkg = 
  [namedRef (getGraphsInTableRef knd1 "" path1 pkg) (S "drasil-" :+: S pkg +:+ S "Types"),
  namedRef (getGraphsInTableRef knd2 "" path2 pkg) (S "drasil-" :+: S pkg +:+ S "Class Instances")]

-- | Helper to create a reference that points to the graph specified by its kind
-- (either "datatype" or "classInst"), prefix (either an empty string or "circo_"), file path
-- to the graph folder, and package name.
getGraphsInTableRef :: String -> String -> FilePath -> String -> Reference
getGraphsInTableRef knd prfx path pkg = makeURI (knd ++ pkg ++ prfx ++ "graph") (path ++ prfx ++ pkg ++ ".svg") $ shortname' $ S $ pkg ++ prfx ++ "graph"

-- ** Table of Graphs

-- | Type dependency table description. Includes information about the colour scheme and what the graph actually means.
tableOfGraphsDescType :: Sentence
tableOfGraphsDescType = S "The following" +:+ namedRef tableGraphRef (S "Table of Type and Class Instance Graphs") +:+ S "is another artifact generated by Drasil. \
  \The type graphs explore the dependency of data types upon each other. These graphs include record-defined types, newtype wrappers, \
  \and data types built from other other types. For these graphs, a node with a black outline signifies that the type is not defined in that package, \
  \but still used in the creation of other types (this includes Haskell-native types since we do not redefine those). A red outline signifies \
  \that the type was created using Haskell's 'type' syntax, while dark green means the type was made using 'newtype' syntax. A purple border shows \
  \that the type uses constructor syntax and cyan is used for types written with record syntax. The arrow starts from the base types at the tip and \
  \follows through so that dependent types are placed at the tail. Usually, this means that those types at the tail may contain the type at the tip of the arrow."

-- | Class-Instance table description. Includes information about the colour scheme and what the graph actually means.
tableOfGraphsDescClassInst :: Sentence
tableOfGraphsDescClassInst = S "The class instance graphs aim to look at the structure of classes, data types, and the interactions \
  \between those two. Specifically, each arrow represents the given type as an instance of a given class. The tip of the arrow points to the class, and the \
  \tail specifies the type that is an instance of the tip's class. For clarity in analyzing the structure, classes defined in the graph's package are coloured magenta, \
  \classes that are used but not defined in the package are rendered pink (includes Haskell-native classes), and data types are rendered with a turquoise border."

-- | Creates a table that links to all generated type and class instance graphs.
mkGraphsTable :: FilePath -> FilePath -> [String] -> Contents
mkGraphsTable typePath clsInstPath pkgs = LlC $ llcc tableGraphRef $ Table 
  [S "Generated Type Graphs", S "Generated Class Instance Graphs"] -- Header row
  (graphTable "datatype" typePath "classInst" clsInstPath pkgs) -- Create the body of the table
  (S "Type Graphs") True -- Label the table

-- | Table of graphs reference.
tableGraphRef :: Reference
tableGraphRef = makeTabRef "TableOfGraphs"

-- * Dependency Graphs Subsection
--
-- $depGraphsSubSection
--
-- Links all the generated module dependency graphs, and gives an example
-- to show what these graphs mean and represent. These graphs show the
-- structure of modules within a given package.

-- | Creates the Package Dependency Graphs section.
graphSec :: FilePath -> [String] -> Section
graphSec path pkgs = 
  section 1 packDepGraphTitle -- Title
  (mkParagraph (S graphSecIntro) : displayGraphs ++ listOfLinkedGraphs ++ mkParagraph (S graphSecBwPkgs) : displayPkgsDepGraph) -- Contents
  [] $ makeSecRef "DependencyGraphs" $ S "Dependency Graphs" -- Section Reference
  where
    -- may want to display more graphs later, but for now we only display the "drasil-website"
    -- package dependencies. If you change this, you should also change the introduction.
    displayGraphs = map (dependencyGraphs path) ["drasil-website"]
    -- displays the graph showing dpendencies between all packages
    displayPkgsDepGraph = map (dependencyGraphs path) ["drasil-all-pkgs-deps"]
    -- these graphs are listed at the bottom of the Drasil website. They are links to the
    -- pdf versions of the package dependency graphs.
    listOfLinkedGraphs = [UlC $ ulcc $ folderList path pkgs]

-- | Package Dependency Graphs section title.
packDepGraphTitle :: Sentence
packDepGraphTitle = S "Package Dependency Graphs"

-- | Introduces the package dependency graphs.
graphSecIntro :: String
graphSecIntro = "The below list contains all of the different packages used to build the Drasil Framework. \
  \Each package and its dependencies are displayed in the form of a graph, with the tail of the arrow being the dependent module, \
  \and the tip of the arrow being the base module. In other words, the tip builds off of (or relies on) the tail to work. \
  \Links are available to a pdf version of each package's dependency graph at the bottom. For example, the graph for the website package \
  \is shown below. Each section is made from different modules that come together under the Drasil.Website.Body module and then \
  \are generated by Drasil.Website.Main. This result shows that the package structure has a pyramid-like hierarchy."

graphSecBwPkgs :: String
graphSecBwPkgs = "The graph displayed below shows the dependencies between the packages used to build the Drasil Framework."

-- | Function to create displayable versions of the graphs.
dependencyGraphs :: FilePath -> String -> Contents
dependencyGraphs path pkg = LlC $ llcc (makeFigRef $ "Figure" ++ pkg) $ fig (S $ "Package: " ++ pkg) $ drasilDisplayDepGraphPath path pkg

-- | Function to get the paths of graphs we want to display on the website.
drasilDisplayDepGraphPath :: FilePath -> FilePath -> String
drasilDisplayDepGraphPath path fldr = path ++ fldr ++ ".png" -- for some reason, svg doesn't show up on generated website, so use png for now

-- | Gets all the paths to the pdf graphs from a given list of packages.
drasilDepGraphPathsPDF :: FilePath -> [String] -> [String]
drasilDepGraphPathsPDF path = map (\x -> path ++ x ++ ".pdf")

-- | Create References to display as links for the dependency graph pdfs.
drasilDepGraphRefs :: FilePath -> [String] -> [Reference]
drasilDepGraphRefs path pkgs = zipWith (\x y -> makeURI x y $ shortname' $ S x) pkgs $ drasilDepGraphPathsPDF path pkgs

-- | Create the list of folders with the links to dependency graph pdfs.
folderList :: FilePath -> [String] -> RawContent
folderList path pkgs = Enumeration $ Bullet $ zip (folderListItems path pkgs) $ repeat Nothing

-- | Helper to create the list items for dependency graph pdfs.
folderListItems :: FilePath -> [String] -> [ItemType]
folderListItems path pkgs = map Flat $ zipWith namedRef (drasilDepGraphRefs path pkgs) $ map S pkgs
