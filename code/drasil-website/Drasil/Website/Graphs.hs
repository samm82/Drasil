module Drasil.Website.Graphs (graphSec, graphRefs) where

import Language.Drasil

----------------------------------
-- Dependency Graphs Section
----------------------------------

graphSec :: FilePath -> [String] -> Section
graphSec path pkgs = section packDepGraphTitle 
  (mkParagraph (S graphSecIntro) : displayGraphs ++ listOfLinkedGraphs) [] graphSecRef
  where
    -- may want to display more graphs later, but for now qw only display the "drasil-website"
    -- package dependencies. If you change this, you should also change the introduction.
    displayGraphs = map (dependencyGraphs path) ["drasil-website"]
    -- these graphs are listed at the bottom of the Drasil website. They are links to the
    -- pdf versions of the package dependency graphs.
    listOfLinkedGraphs = [UlC $ ulcc $ folderList path pkgs]

-- | Package Dependency Graphs section title.
packDepGraphTitle :: Sentence
packDepGraphTitle = S "Package Dependency Graphs"

-- | Introduces what the package dependency graphs are.
graphSecIntro :: String
graphSecIntro = "The below list contains all of the different packages used to build the Drasil Framework. \
  \Each package and its dependencies are displayed in the form of a graph, with the tail of the arrow being the dependent module, \
  \and the tip of the arrow being the base module. In other words, the tip builds off of (or relies on) the tail to work. \
  \Links are available to a pdf version of each package's dependency graph at the bottom. For example, the graph for the website package \
  \is shown below. Each section is made from different modules that come together under the Drasil.Website.Body module and then \
  \are generated by Drasil.Website.Main. This result shows that the package structure has a pyramid-like hierarchy."

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
drasilDepGraphRefs path pkgs = zipWith (\x y -> Reference x (URI y) $ shortname' $ S x) pkgs $ drasilDepGraphPathsPDF path pkgs

-- | Create the list of folders with the links to dependency graph pdfs.
folderList :: FilePath -> [String] -> RawContent
folderList path pkgs = Enumeration $ Bullet $ zip (folderListItems path pkgs) $ repeat Nothing

-- | Helper to create the list items for dependency graph pdfs.
folderListItems :: FilePath -> [String] -> [ItemType]
folderListItems path pkgs = map Flat $ zipWith namedRef (drasilDepGraphRefs path pkgs) $ map S pkgs

-- | Create section reference for Package Dependency Graphs.
graphSecRef :: Reference
graphSecRef = makeSecRef "DependencyGraphs" $ S "Dependency Graphs"

-- | All references used in this section.
graphRefs :: FilePath -> [String] -> [Reference]
graphRefs path pkgs = [graphSecRef, ref $ graphSec path pkgs] ++ drasilDepGraphRefs path pkgs

