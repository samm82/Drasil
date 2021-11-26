module Main (main) where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.HGHC.Body (srs, printSetting, fullSI) --
-- import Drasil.HGHC.Choices (thisChoices, thisCode)
  
main :: IO ()            
main = do
  putStrLn "Entering main function."
  setLocaleEncoding utf8
  putStrLn "Set encoding."
  gen (DocSpec (docChoices SRS [HTML, TeX]) "HGHC_SRS") srs printSetting
  putStrLn "Generated document."
  genLog fullSI printSetting
  putStrLn "Generated log."
  -- When ready to generate code, uncomment this file and Choices.hs
  --genCode thisChoices thisCode
  -- When ready to generate traceability graphs, uncomment this and import genDot and fullSI:
  -- genDot fullSI
