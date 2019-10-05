module Main where

  import Criterion.Main
  import Algebra.Graph.Undirected

  gsmall :: Graph Int
  gsmall = vertex 1

  gmedium :: Graph Int
  gmedium = overlay (vertices [1..5]) (clique [6..10])

  gsmallE :: [(Int, Int)]
  gsmallE = [(x, y) | x<-[1..10], y<-[1..10]]

  gbig :: Graph Int
  gbig = overlay (vertices [1..25]) (clique [26..50])

  gmediumE :: [(Int, Int)]
  gmediumE = [(x, y) | x<-[1..100], y<-[1..100]]

  gbigE :: [(Int, Int)]
  gbigE = [(x, y) | x<-[1..1000], y<-[1..1000]]

  ghugeE :: [(Int, Int)]
  ghugeE = [(x, y) | x<-[1..10000], y<-[1..10000]]

  group :: String -> Graph Int -> Benchmark
  group label g = bgroup label  [
                     bench "removeEdges" $ nf complement g,
                     bench "\\\\" $ nf complement' g
                   ]

  main :: IO ()
  main = defaultMain [bench "small" $ nf edges gsmallE, bench "medium" $ nf edges gmediumE, bench "big" $ nf edges gbigE, bench "huge" $ nf edges ghugeE ]
