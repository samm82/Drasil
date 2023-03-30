module Drasil.ChemCode.IMods where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil

import Drasil.ChemCode.Quantities (eMat, unaryVec, xVec, zeroVec)

ims :: [InstanceModel]
ims = [chemEqIntLinProg]

chemEqIntLinProg :: InstanceModel
chemEqIntLinProg = imNoRefs
  (ilpModel "chemEqIntLinProg" (cn' "integer linear program for a chemical equation")
    $ minILP xVec unaryVec
      $ (sy eMat `mulRe` sy xVec $= sy zeroVec) NE.:| [sy xVec $> sy zeroVec, isIn (sy xVec ) (Vect Integer)])
      -- FIXME: ^ parentheses needed for precedence reasons?
  [qwUC eMat] -- FIXME: why should I need to convert the Unitals to QuantityDicts?
  xVec
  []
  Nothing -- TODO: Derivation
  "chemEqIntLinProg"
  [] -- TODO: Notes
