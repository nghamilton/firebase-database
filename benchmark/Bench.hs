module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified MainBench

main :: IO ()
main = defaultMain
    [ bgroup "API" HuskBench.benchmarks
    ]
