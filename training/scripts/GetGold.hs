import System.IO
import System (getArgs) 
import Safe (headDef)
main = do
    [c'] <- getArgs 
    let c = head c'
    l <- getContents
    putStrLn $ unlines $ map (dropWhile ((/=) '('))  $ filter ((==(c::Char)) . headDef ' ') $ lines l