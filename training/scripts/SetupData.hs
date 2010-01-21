import PreProcess
import TreeBank
import System (getArgs) 
import System.IO
import POS
import Common
import DataHelpers
main = do 
  [file1] <- getArgs
  cont <- readFile file1
  sentbundle <- getSentences file1
  sequence_ $ 
         map (\sents -> do  
                  putStrLn $ intercalate "\n\n" $ 
                           map (show . toWIS . augmentNP . reapply 20 (liftNode (isPOSComma . pos)) . reapply 5 (removeFront (isPOSComma . pos)) . reapply 5 (removeEnd (isPOSComma . pos)) .  filterNode (isPOSPunc . pos) . fromWIS) sents
                  putStrLn ""
   ) sentbundle
  where
    reapply 0 _ d = d 
    reapply n f d = reapply (n-1) f $ f d 
