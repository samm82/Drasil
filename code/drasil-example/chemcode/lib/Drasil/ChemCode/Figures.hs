module Drasil.ChemCode.Figures where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (physSyst, sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/chemcode/"

sysCtxFig :: LabelledContent
sysCtxFig = llcc (makeFigRef "sysCtxFig") $ 
  fig (titleize sysCont) (resourcePath ++ "sysCtxFig.png") 

physSysFig :: LabelledContent
physSysFig = llcc (makeFigRef "physSysFig") $ 
  fig (titleize physSyst) (resourcePath ++ "physSysFig.png") 