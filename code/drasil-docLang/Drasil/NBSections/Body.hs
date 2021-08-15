module Drasil.NBSections.Body (reviewSec, mainIdeaSec, mthdAndanls, exampleSec) where

import Language.Drasil
import qualified Drasil.DocLang.Notebook as NB (review, mainIdea, methAndAnls, example)

-- **** Leave blank for now
--bodyIntro :: Contents
--bodyIntro = foldlSP [S ""]

-- Review
reviewSec :: [Contents] -> Section
reviewSec = NB.review 1

-- Main Idea
mainIdeaSec :: [Contents] -> Section
mainIdeaSec = NB.mainIdea 1

-- Method and Analysis
mthdAndanls :: [Contents] -> Section
mthdAndanls = NB.methAndAnls 1

-- Example
exampleSec :: [Contents] -> Section
exampleSec = NB.example 1