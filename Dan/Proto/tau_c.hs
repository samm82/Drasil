module Tau_c where
import Chunk
import Text.PrettyPrint

tau_c :: Chunk FName FDesc
tau_c = newChunk $
  [("Symbol",text "$\\tau_c$"),
   ("Equation", text "\\tau_{c}"),
   ("Description", text "clad thickness")
  ]