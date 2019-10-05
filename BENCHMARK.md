benchmarking small/removeEdges
time                 58.09 ns   (57.74 ns .. 58.53 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 58.06 ns   (57.80 ns .. 58.56 ns)
std dev              1.138 ns   (766.3 ps .. 1.692 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarking small/\\
time                 92.72 ns   (92.34 ns .. 93.13 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 93.04 ns   (92.61 ns .. 93.86 ns)
std dev              1.977 ns   (1.359 ns .. 3.129 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking medium/removeEdges
time                 151.5 μs   (150.9 μs .. 152.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 151.3 μs   (150.8 μs .. 152.2 μs)
std dev              2.068 μs   (1.167 μs .. 3.662 μs)

benchmarking medium/\\
time                 51.73 μs   (51.49 μs .. 52.03 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 51.82 μs   (51.60 μs .. 52.18 μs)
std dev              880.7 ns   (571.8 ns .. 1.225 μs)
variance introduced by outliers: 12% (moderately inflated)

benchmarking medium clique/removeEdges
time                 814.7 μs   (810.2 μs .. 819.8 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 814.7 μs   (811.4 μs .. 820.3 μs)
std dev              13.85 μs   (9.034 μs .. 18.81 μs)

benchmarking medium clique/\\
time                 71.85 μs   (71.55 μs .. 72.21 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 71.83 μs   (71.59 μs .. 72.21 μs)
std dev              1.019 μs   (686.9 ns .. 1.464 μs)

benchmarking big/removeEdges
time                 2.087 s    (NaN s .. 2.093 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.089 s    (2.088 s .. 2.091 s)
std dev              1.652 ms   (682.9 μs .. 2.173 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking big/\\
time                 5.974 ms   (5.934 ms .. 6.021 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 5.986 ms   (5.962 ms .. 6.031 ms)
std dev              99.87 μs   (64.03 μs .. 160.9 μs)

benchmarking big clique/removeEdges
time                 11.97 s    (11.94 s .. 12.01 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.98 s    (11.97 s .. 11.98 s)
std dev              7.358 ms   (5.587 ms .. 8.782 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking big clique/\\
time                 2.649 ms   (2.617 ms .. 2.681 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.701 ms   (2.674 ms .. 2.735 ms)
std dev              99.55 μs   (73.59 μs .. 135.8 μs)
variance introduced by outliers: 22% (moderately inflated)

