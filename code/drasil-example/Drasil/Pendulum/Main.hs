module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Pendulum.Body (srs, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS     "Pendulum_SRS") srs printSetting
  gen (DocSpec Website "Pendulum_SRS") srs printSetting
