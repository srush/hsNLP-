module NLP.FSM.Draw where 

import Data.GraphViz
import Data.GraphViz.Types
import System.Process
import System.Random


dotGraph gr = setID (Str "depend") $ 
          graphToDot True gr [] ig (\(_, _, l) -> [Label (StrLabel $ show l)] )
              where ig _ = [] 


showDotGraph gr = do 
  file <- randomIO
  let filename = "/tmp/depend." ++ show (file::Double) 
  writeFile filename s
  runCommand $ "dotty " ++ filename 
    where s = printDotGraph $ gr
