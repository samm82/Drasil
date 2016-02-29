{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module PCMExample where
import ASTInternal (Expr(..))
import SI_Units
import Unicode (Tau(..), Delta(..), Rho(..), Nabla(..)) --, Phi(..))
import Symbol
import UnitalChunk
import PCMUnits
import SymbolAlphabet
import Chunk (ConceptChunk(..),VarChunk,symbol,makeCC,makeVC)
import EqChunk (fromEqn,EqChunk(..))
import Format (FormatC(..))
import Spec (Spec(..))
import Control.Lens ((^.))
import Unit

-- import Control.Lens ((^.))
pcmSymbols :: [UnitalChunk]
pcmSymbols = [coil_SA,hIn_SA,hOut_SA,htCap,htCap_Liq,htCap_W,tank_D,ht_gen_vol,
  ht_xfer_co,ht_xfer_CW,tank_L,mass,water_m, norm_vect,
  ht_flux, thFluxVect,
  ht_flux_C,ht_flux_in,ht_flux_out,time,temp, --temp_boil,
  temp_coil,temp_env,time_final,temp_init,temp_water,temp_diff,vol, --tank_vol,
  water_vol,density,water_dense,dummyVar]

coil_SA, hIn_SA, hOut_SA, htCap, htCap_Liq, htCap_W, tank_D, ht_gen_vol,
  ht_xfer_co,ht_xfer_CW, tank_L,mass,water_m,norm_vect,ht_flux,thFluxVect,
  ht_flux_C,ht_flux_in,ht_flux_out,time,temp,--temp_boil,
  temp_coil,temp_env,time_final,temp_init,temp_water,temp_diff,vol,--tank_vol,
  water_vol,density,water_dense,dummyVar :: UnitalChunk
coil_SA     = makeUC "A_C" "coil surface area" (sub cA cC) m_2
hIn_SA      = makeUC "A_in" "surface area over which heat is transferred in" 
              (sub cA (Atomic "in")) m_2
hOut_SA     = makeUC "A_out" "surface area over which heat is transferred out" 
              (sub cA (Atomic "out")) m_2
htCap       = makeUC "C" "specific heat capacity" cC heat_capacity
htCap_Liq   = makeUC "C^L" "specific heat capacity of a liquid" (sup cC cL)
              heat_capacity
htCap_W     = makeUC "C_W" "specific heat capacity of water" (sub cC cW)
              heat_capacity
tank_D      = makeUC "D" "diameter of tank" cD metre
ht_gen_vol  = makeUC "g" "volumetric heat generation per unit volume" lG
              thermFluxU
ht_xfer_co  = makeUC "h" "convective heat transfer coefficient" lH heat_transfer
ht_xfer_CW  = makeUC "h_C" "convective heat transfer between coil and water" 
              (sub lH cC) heat_transfer
tank_L      = makeUC "L" "length of tank" cL metre
mass        = makeUC "m" "mass" lM kilogram
water_m     = makeUC "m_W" "mass of water" (sub lM cW) kilogram
norm_vect   = makeUC "n_vect" "unit outward normal vector for a surface"
              (FormatS Vector (FormatS Hat lN)) unitless
  -- How do I make a symbol that needs one (or more) FormatC? Add to Symbol or
  -- pull FormatC out somehow?
ht_flux     = makeUC "q" "heat flux" lQ heat_transfer
thFluxVect  = makeUC "q_vect" "thermal flux vector" (FormatS Vector lQ)
                  thermFluxU
ht_flux_C   = makeUC "q_C" "heat flux from coil" (sub lQ cC) thermFluxU
ht_flux_in  = makeUC "q_in" "heat flux in" (sub lQ (Atomic "in")) thermFluxU
ht_flux_out = makeUC "q_out" "heat flux out" (sub lQ (Atomic "out")) thermFluxU
time        = makeUC "t" "time" lT second
temp        = makeUC "T" "temperature" cT centigrade
-- temp_boil   = makeUC "T_boil" "temperature at boiling point" 
              -- (sub cT (Atomic "boil")) centigrade
temp_coil   = makeUC "T_coil" "temperature of coil" 
              (sub cT cC) centigrade
temp_env    = makeUC "T_env" "temperature of environment" 
              (sub cT (Atomic "env")) centigrade
time_final  = makeUC "t_final" "time" (sub lT (Atomic "final")) second
temp_init   = makeUC "T_init" "initial temperature" 
              (sub cT (Atomic "init")) centigrade
temp_water  = makeUC "T_W" "temperature of water" 
              (sub cT cW) centigrade
temp_diff   = makeUC "deltaT" "temperature difference" 
              (Catenate (Special Delta) cT) centigrade
vol         = makeUC "V" "volume" cV m_3
--tank_vol    = makeUC "V_tank" "volume of the cylindrical tank" 
                -- (sub cV (Atomic "tank")) m_3
water_vol   = makeUC "V_W" "volume of water" (sub cV cW) m_3
density     = makeUC "rho" "density, mass per unit volume" (Special Rho_L) 
              densityU
water_dense = makeUC "rho_W" "density of water" (sub (Special Rho_L) cW) densityU
dummyVar    = makeUC "tau" "dummy variable for integration over time" 
                (Special Tau_L) second
--melt_frac   = makeUC "Phi" "melt fraction" (Special Phi) unitless

----VarChunks----
gradient :: VarChunk
gradient = makeVC "gradient" "the gradient operator" (Special Nabla)

----Acronyms-----
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,instanceMod,likelyChange,oDE,
  physSysDescr,requirement,softwareRS,sWHS,theoreticMod]
  
assumption,dataDefn,genDefn,goalStmt,instanceMod,likelyChange,oDE,
  physSysDescr,requirement,softwareRS,sWHS,theoreticMod :: ConceptChunk
assumption    = makeCC "A" "Assumption"
dataDefn      = makeCC "DD" "Data Definition"
genDefn       = makeCC "GD" "General Definition"
goalStmt      = makeCC "GS"  "Goal Statement"
instanceMod   = makeCC "IM" "Instance Model"
likelyChange  = makeCC "LC" "Likely Change"
oDE           = makeCC "ODE" "Ordinary Differential Equation"
physSysDescr  = makeCC "PS" "Physical System Description"
requirement   = makeCC "R" "Requirement"
softwareRS    = makeCC "SRS" "Software Requirements Specification"
sWHS          = makeCC "SWHS" "Solar Water Heating System"
theoreticMod  = makeCC "T" "Theoretical Model"

----EqChunks----
--Theoretical models--
t1consThermE :: EqChunk
t1consThermE = fromEqn "Conservation of thermal energy" t1descr NA 
                unitless cons_therm_eqn

cons_therm_eqn :: Expr
cons_therm_eqn = (-1) * (C gradient) * (C thFluxVect) + (C ht_gen_vol) := 
  (C density) * (C htCap) * (Deriv (C temp) (C time))

t1descr :: Spec
t1descr = 
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+: 
  (N $ htCap ^. symbol) :+: S "and density " :+: (N $ density ^. symbol) :+:
  S ", where " :+: (N $ thFluxVect ^. symbol)) 
  --TODO: Finish this description and do it better. I need to
  --  figure out the best way to encode this information.
  