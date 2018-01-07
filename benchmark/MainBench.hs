module MainBench (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Main

benchmarks :: [Benchmark]
benchmarks =
    [ bench "newQr" (nf newQr)
    ]

newQr = 1
