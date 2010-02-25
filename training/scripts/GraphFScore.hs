import System
import System.IO
import Text.Printf
import System.Process
import HSH
import Graphics.Gnuplot.Simple

main = do
    let is = [6.6,6.7..7.7]
        s = [do 
              let cmd = (printf "python scripts/Score.py results C3 20 23 %1.1fe | python scripts/GetFMeasure.py" i)::String
              out <- run $ cmd
              return (i, read out)
             | i <- is]
    mydata <- sequence s
    plotPath [EPS "/tmp/graph.eps",Title "Validation (150 Sents)", XLabel "Objective Ratio (10^)", YLabel "FScore"]  (mydata::[(Double,Double)]) 
