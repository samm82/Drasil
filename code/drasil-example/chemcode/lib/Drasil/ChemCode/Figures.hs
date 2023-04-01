module Drasil.ChemCode.Figures where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/chemcode/"

sysCtxFig :: LabelledContent
sysCtxFig = llcc (makeFigRef "sysCtxFig") $ 
  fig (titleize sysCont) (resourcePath ++ "sysCtxFig.png") 