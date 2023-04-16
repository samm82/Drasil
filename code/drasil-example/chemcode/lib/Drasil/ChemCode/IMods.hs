module Drasil.ChemCode.IMods where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil

import Drasil.ChemCode.Quantities (aMat, bVec, cVec, eMat, inputChemEqn,
  unaryVec, xVec, zeroVec, elems, genE, genI)
import Drasil.ChemCode.TMods (intLinProg)

ims :: [InstanceModel]
ims = [matRepresentation, chemEqIntLinProg]

convertMatEq :: SimpleQDef
convertMatEq = mkQuantDef eMat (forall [
    (genI, abs_ $ apply elems [sy inputChemEqn]),
    (genE, apply elems [sy inputChemEqn])
  ] $ sy eMat)

matRepresentation :: InstanceModel
matRepresentation = imNoRefs
  (equationalModel' convertMatEq)
  [qwUC inputChemEqn] -- FIXME: why should I need to convert the Unitals to QuantityDicts?
  eMat
  []
  Nothing -- Derivation: (Just $ mkDerivName [S "parts of deriv"])
  "matRepresentation"
  [] -- TODO: Notes

chemEqIntLinProg :: InstanceModel
chemEqIntLinProg = imNoRefs
  (ilpModel "chemEqIntLinProg" (cn' "integer linear program for a chemical equation")
    $ minILP xVec unaryVec
      $ (sy eMat `mulRe` sy xVec $= sy zeroVec) NE.:| [sy xVec $> sy zeroVec, isIn (sy xVec) (Vect Integer)])
  [qwUC eMat] -- FIXME: why should I need to convert the Unitals to QuantityDicts?
  xVec
  []
  Nothing -- Derivation: (Just $ mkDerivName [S "parts of deriv"])
  "chemEqIntLinProg"
  [foldlSent [S "This is a specific instance of the ILP from", refS intLinProg,
    S "with" +:+. foldlList Comma List [
      E (($=) (sy aMat) (sy eMat)), E (($=) (sy bVec) (sy zeroVec)), E (($=) (sy cVec) (sy unaryVec))
    ], S "The goal and constraints are also modified"]] -- TODO: Notes
