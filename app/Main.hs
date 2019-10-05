module Main where

  import Criterion.Main
  import Algebra.Graph.Undirected

  gsmall :: Graph Int
  gsmall = vertex 1

  gmedium :: Graph Int
  gmedium = overlay (vertices [1..5]) (clique [6..10])

  gbig :: Graph Int
  gbig = overlay (vertices [1..25]) (clique [26..50])

  group :: String -> Graph Int -> Benchmark
  group label g = bgroup label  [
                     bench "removeEdges" $ nf complement g,
                     bench "\\\\" $ nf complement' g
                   ]

  main :: IO ()
  main = defaultMain [
                        group "small" gsmall,
                        group "medium" gmedium,
                        group "medium clique" (clique [1..10]),
                        group "big" gbig,
                        group "big clique" (clique [1..50])
                     ]
