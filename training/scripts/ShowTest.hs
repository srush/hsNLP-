import System (getArgs) 
import System.IO
import TreeBank
import TAG

main = do 
  [showFile] <- getArgs
  contents <- readFile showFile
  let sent = parseSentence showFile contents 
  let tree = convertToTree $ toTAGDependency sent
  print tree
