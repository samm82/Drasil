module Main where

import Helpers
import System.IO
import Text.PrettyPrint.HughesPJ

data DocType = SRS
             | LPM
             | Code

             
gen = do
  outh <- openFile "SRS.tex" WriteMode
  hPutStrLn outh $ render $ createSRS
  hClose outh
  outh <- openFile "LPM.tex" WriteMode
  hPutStrLn outh $ render $ createLPM
  hClose outh
  
auth = "Spencer Smith"


spre = docclass [] "article" $$ usepackage "longtable" $$ usepackage "booktabs"
lpre = docclass "article" "cweb-hy" $$ usepackage "xr" $$ exdoc "-L" "hghc_SRS"

createSRS :: Doc  
createSRS = spre $$ title "Literate Programmer's Manual for $h_g$ and $h_c$" $$
            author auth $$ begin

createLPM :: Doc
createLPM = lpre $$ title "Literate Programmer's Manual for $h_g$ and $h_c$" $$
            author auth $$ begin
  
main = do
  gen