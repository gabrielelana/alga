{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'Graph' and polymorphic functions defined in
-- "Algebra.Graph.HigherKinded.Class".
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Graph (
    -- * Testsuite
    testGraph
  ) where

import Data.Foldable

import Algebra.Graph
import Algebra.Graph.Test

import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

type G  = Graph Int
type II = Int -> Int
type IB = Int -> Bool
type IG = Int -> G

testGraph :: IO ()
testGraph = do
    putStrLn "\n============ Graph ============"
    test "Axioms of graphs"     $ (axioms   :: GraphTestsuite G)
    test "\nTheorems of graphs" $ (theorems :: GraphTestsuite G)

    putStrLn "\n============ empty ============"

    test "isEmpty     empty == True " $
          isEmpty    (empty :: G) == True

    test "hasVertex x empty == False" $ \(x :: Int) ->
          hasVertex x empty == False

    test "vertexCount empty == 0    " $
          vertexCount(empty :: G) == 0

    test "edgeCount   empty == 0    " $
          edgeCount  (empty :: G) == 0

    test "size        empty == 1    " $
          size       (empty :: G) == 1

    putStrLn "\n============ vertex ============"

    test "isEmpty     (vertex x) == False" $ \(x :: Int) ->
          isEmpty     (vertex x) == False

    test "hasVertex x (vertex x) == True " $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 (vertex 2 :: G) == False

    test "vertexCount (vertex x) == 1    " $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0    " $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    test "size        (vertex x) == 1    " $ \(x :: Int) ->
          size        (vertex x) == 1

    putStrLn "\n============ overlay ============"

    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y  " $ \(x :: G) y ->
          isEmpty     (overlay x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \(x :: G) y z ->
          hasVertex z (overlay x y) ==(hasVertex z x || hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x                 " $ \(x :: G) y ->
          vertexCount (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y " $ \(x :: G) y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x                   " $ \(x :: G) y ->
          edgeCount   (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y   " $ \(x :: G) y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    test "size        (overlay x y) == size x        + size y        " $ \(x :: G) y ->
          size        (overlay x y) == size x        + size y

    test "vertexCount (overlay 1 2) == 2                             " $
          vertexCount (overlay 1 2 :: G) == 2

    test "edgeCount   (overlay 1 2) == 0                             " $
          edgeCount   (overlay 1 2 :: G) == 0

    putStrLn "\n============ connect ============"

    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y  " $ \(x :: G) y ->
          isEmpty     (connect x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \(x :: G) y z ->
          hasVertex z (connect x y) ==(hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x                 " $ \(x :: G) y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y " $ \(x :: G) y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x                   " $ \(x :: G) y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y                   " $ \(x :: G) y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y " $ \(x :: G) y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \(x :: G) y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "size        (connect x y) == size x        + size y        " $ \(x :: G) y ->
          size        (connect x y) == size x        + size y

    test "vertexCount (connect 1 2) == 2                             " $
          vertexCount (connect 1 2 :: G) == 2

    test "edgeCount   (connect 1 2) == 1                             " $
          edgeCount   (connect 1 2 :: G) == 1

    putStrLn "\n============ foldg ============"

    test "foldg empty vertex        overlay connect        == id       " $ \(x :: G) ->
          foldg empty vertex        overlay connect x      == x

    test "foldg empty vertex        overlay (flip connect) == transpose" $ \(x :: G) ->
          foldg empty vertex        overlay (flip connect)x== transpose x

    test "foldg []    return        (++)    (++)           == toList   " $ \(x :: G) ->
          foldg []    return        (++)    (++) x         == toList x

    test "foldg 0     (const 1)     (+)     (+)            == length   " $ \(x :: G) ->
          foldg 0     (const 1)     (+)     (+) x          == length x

    test "foldg 1     (const 1)     (+)     (+)            == size     " $ \(x :: G) ->
          foldg 1     (const 1)     (+)     (+) x          == size x

    test "foldg True  (const False) (&&)    (&&)           == isEmpty  " $ \(x :: G) ->
          foldg True  (const False) (&&)    (&&) x         == isEmpty x

    putStrLn "\n============ vertices ============"

    test "vertices []            == empty       " $
          vertices []            == (empty :: G)

    test "vertices [x]           == vertex x    " $ \(x :: Int) ->
          vertices [x]           == vertex x

    test "hasVertex x . vertices == elem x      " $ \x (xs :: [Int]) ->
         (hasVertex x . vertices) xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \(xs :: [Int]) ->
         (vertexCount . vertices) xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet   . vertices) xs == Set.fromList xs

    putStrLn "\n============ overlays ============"

    test "overlays []        == empty      " $
          overlays []        ==(empty :: G)

    test "overlays [x]       == x          " $ \(x :: G) ->
          overlays [x]       == x

    test "overlays [x,y]     == overlay x y" $ \(x :: G) y ->
          overlays [x,y]     == overlay x y

    test "isEmpty . overlays == all isEmpty" $ \(xs :: [G]) ->
         (isEmpty . overlays) xs == all isEmpty xs

    putStrLn "\n============ connects ============"

    test "connects []        == empty      " $
          connects []        ==(empty :: G)

    test "connects [x]       == x          " $ \(x :: G) ->
          connects [x]       == x

    test "connects [x,y]     == connect x y" $ \(x :: G) y ->
          connects [x,y]     == connect x y

    test "isEmpty . connects == all isEmpty" $ \(xs :: [G]) ->
         (isEmpty . connects) xs == all isEmpty xs

    putStrLn "\n============ edge ============"

    test "edge x y               == connect (vertex x) (vertex y)" $ \(x :: Int) y ->
          edge x y               == connect (vertex x) (vertex y)

    test "hasEdge x y (edge x y) == True                         " $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1                            " $ \(x :: Int) y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1                            " $
          vertexCount (edge 1 1 :: G) == 1

    test "vertexCount (edge 1 2) == 2                            " $
          vertexCount (edge 1 2 :: G) == 2

    putStrLn "\n============ edges ============"

    test "edges []          == empty       " $
          edges []          ==(empty :: G)

    test "edges [(x,y)]     == edge x y    " $ \(x :: Int) y ->
          edges [(x,y)]     == edge x y

    test "edgeCount . edges == length . nub" $ \(xs :: [(Int, Int)]) ->
         (edgeCount . edges) xs == (length . nubOrd) xs

    putStrLn "\n============ graph ============"

    test "graph []  []      == empty                           " $
          graph []  []      ==(empty :: G)

    test "graph [x] []      == vertex x                        " $ \(x :: Int) ->
          graph [x] []      == vertex x

    test "graph []  [(x,y)] == edge x y                        " $ \(x :: Int) y ->
          graph []  [(x,y)] == edge x y

    test "graph vs  es      == overlay (vertices vs) (edges es)" $ \(vs :: [Int]) es ->
          graph vs  es      == overlay (vertices vs) (edges es)

    putStrLn "\n============ isSubgraphOf ============"

    test "isSubgraphOf empty         x             == True " $ \(x :: G) ->
          isSubgraphOf empty         x             == True

    test "isSubgraphOf (vertex x)    empty         == False" $ \x ->
          isSubgraphOf (vertex x)   (empty :: G)   == False

    test "isSubgraphOf x             (overlay x y) == True " $ \(x :: G) y ->
          isSubgraphOf x             (overlay x y) == True

    test "isSubgraphOf (overlay x y) (connect x y) == True " $ \(x :: G) y ->
          isSubgraphOf (overlay x y) (connect x y) == True

    test "isSubgraphOf (path xs)     (circuit xs)  == True " $ \xs ->
          isSubgraphOf (path xs :: G)(circuit xs)  == True

    putStrLn "\n============ isEmpty ============"

    test "isEmpty empty                       == True " $
          isEmpty (empty :: G)                == True

    test "isEmpty (overlay empty empty)       == True " $
          isEmpty (overlay empty empty :: G)  == True

    test "isEmpty (vertex x)                  == False" $ \(x :: Int) ->
          isEmpty (vertex x)                  == False

    test "isEmpty (removeVertex x $ vertex x) == True " $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \(x :: Int) y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ size ============"

    test "size empty         == 1              " $
          size (empty :: G)  == 1

    test "size (vertex x)    == 1              " $ \(x :: Int) ->
          size (vertex x)    == 1

    test "size (overlay x y) == size x + size y" $ \(x :: G) y ->
          size (overlay x y) == size x + size y

    test "size (connect x y) == size x + size y" $ \(x :: G) y ->
          size (connect x y) == size x + size y

    putStrLn "\n============ hasVertex ============"

    test "hasVertex x empty            == False      " $ \(x :: Int) ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True       " $ \(x :: Int) ->
          hasVertex x (vertex x)       == True

    test "hasVertex x . removeVertex x == const False" $ \(x :: Int) y ->
          hasVertex x (removeVertex x y)==const False y

    putStrLn "\n============ hasEdge ============"

    test "hasEdge x y empty            == False      " $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False      " $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True       " $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
          hasEdge x y (removeEdge x y z)==const False z

    putStrLn "\n============ vertexCount ============"

    test "vertexCount empty      == 0                   " $
          vertexCount (empty :: G) == 0

    test "vertexCount (vertex x) == 1                   " $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "vertexCount            == Set.size . vertexSet" $ \(x :: G) ->
          vertexCount x          ==(Set.size . vertexSet) x

    putStrLn "\n============ edgeCount ============"

    test "edgeCount empty      == 0                " $
          edgeCount (empty :: G) == 0

    test "edgeCount (vertex x) == 0                " $ \(x :: Int) ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1                " $ \(x :: Int) y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \(x :: G) ->
          edgeCount x          == (length . edgeList) x

    putStrLn "\n============ vertexList ============"

    test "vertexList empty      == []        " $
          vertexList (empty :: G) == []

    test "vertexList (vertex x) == [x]       " $ \(x :: Int) ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \(xs :: [Int]) ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

    putStrLn "\n============ edgeList ============"

    test "edgeList empty          == []            " $
          edgeList (empty :: G )  == []

    test "edgeList (vertex x)     == []            " $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]       " $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList (star 2 [3,1]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges        == nub . sort    " $ \(xs :: [(Int, Int)]) ->
         (edgeList . edges) xs    ==(nubOrd . sort) xs

    putStrLn "\n============ vertexSet ============"

    test "vertexSet empty      == Set.empty    " $
          vertexSet(empty :: G)== Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x== Set.singleton x

    test "vertexSet . vertices == Set.fromList " $ \(xs :: [Int]) ->
         (vertexSet . vertices) xs == Set.fromList xs

    test "vertexSet . clique   == Set.fromList " $ \(xs :: [Int]) ->
         (vertexSet . clique) xs == Set.fromList xs

    putStrLn "\n============ vertexIntSet ============"

    test "vertexIntSet empty      == IntSet.empty    " $
          vertexIntSet(empty :: G)== IntSet.empty

    test "vertexIntSet . vertex   == IntSet.singleton" $ \(x :: Int) ->
         (vertexIntSet . vertex) x== IntSet.singleton x

    test "vertexIntSet . vertices == IntSet.fromList " $ \(xs :: [Int]) ->
         (vertexIntSet . vertices) xs == IntSet.fromList xs

    test "vertexIntSet . clique   == IntSet.fromList " $ \(xs :: [Int]) ->
         (vertexIntSet . clique) xs == IntSet.fromList xs

    putStrLn "\n============ edgeSet ============"

    test "edgeSet empty      == Set.empty          " $
          edgeSet (empty :: G) == Set.empty

    test "edgeSet (vertex x) == Set.empty          " $ \(x :: Int) ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \(x :: Int) y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList       " $ \(xs :: [(Int, Int)]) ->
         (edgeSet . edges) xs== Set.fromList xs

    putStrLn "\n============ path ============"

    test "path []    == empty   " $
          path []    ==(empty :: G)

    test "path [x]   == vertex x" $ \(x :: Int) ->
          path [x]   == vertex x

    test "path [x,y] == edge x y" $ \(x :: Int) y ->
          path [x,y] == edge x y

    putStrLn "\n============ circuit ============"

    test "circuit []    == empty               " $
          circuit []    ==(empty :: G)

    test "circuit [x]   == edge x x            " $ \(x :: Int) ->
          circuit [x]   == edge x x

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \(x :: Int) y ->
          circuit [x,y] == edges [(x,y), (y,x)]

    putStrLn "\n============ clique ============"

    test "clique []      == empty                      " $
          clique []      ==(empty :: G)

    test "clique [x]     == vertex x                   " $ \(x :: Int) ->
          clique [x]     == vertex x

    test "clique [x,y]   == edge x y                   " $ \(x :: Int) y ->
          clique [x,y]   == edge x y

    test "clique [x,y,z] == edges [(x,y), (x,z), (y,z)]" $ \(x :: Int) y z ->
          clique [x,y,z] == edges [(x,y), (x,z), (y,z)]

    putStrLn "\n============ biclique ============"

    test "biclique []      []      == empty                                     " $
          biclique []      []      ==(empty :: G)

    test "biclique [x]     []      == vertex x                                  " $ \(x :: Int) ->
          biclique [x]     []      == vertex x

    test "biclique []      [y]     == vertex y                                  " $ \(y :: Int) ->
          biclique []      [y]     == vertex y

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1 :: Int) x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    putStrLn "\n============ star ============"

    test "star x []    == vertex x            " $ \(x :: Int) ->
          star x []    == vertex x

    test "star x [y]   == edge x y            " $ \(x :: Int) y ->
          star x [y]   == edge x y

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \(x :: Int) y z ->
          star x [y,z] == edges [(x,y), (x,z)]

    putStrLn "\n============ mesh ============"

    test "mesh xs     []   == empty                  " $ \xs ->
          mesh xs     []   == (empty :: Graph (Int, Int))

    test "mesh []     ys   == empty                  " $ \ys ->
          mesh []     ys   == (empty :: Graph (Int, Int))

    test "mesh [x]    [y]  == vertex (x, y)          " $ \(x :: Int) (y :: Int) ->
          mesh [x]    [y]  == vertex (x, y)

    test "mesh xs     ys   == box (path xs) (path ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          mesh xs     ys   == box (path xs) (path ys)

    test ("mesh [1..3] \"ab\" == <correct result>       ") $
         mesh [1..3] "ab"  == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))
                                    , ((2,'a'),(3,'a')), ((2,'b'),(3,'b')), ((3,'a'),(3 :: Int,'b')) ]

    putStrLn "\n============ torus ============"

    test "torus xs     []   == empty                        " $ \xs ->
          torus xs     []   == (empty :: Graph (Int, Int))

    test "torus []     ys   == empty                        " $ \ys ->
          torus []     ys   == (empty :: Graph (Int, Int))

    test "torus [x]    [y]  == edge (x, y) (x, y)           " $ \(x :: Int) (y :: Int) ->
          torus [x]    [y]  == edge (x, y) (x, y)

    test "torus xs     ys   == box (circuit xs) (circuit ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          torus xs     ys   == box (circuit xs) (circuit ys)

    test ("torus [1..2] \"ab\" == <correct result>             ") $
         torus [1..2] "ab"  == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))
                                     , ((2,'a'),(1,'a')), ((2,'a'),(2,'b')), ((2,'b'),(1,'b')), ((2,'b'),(2 :: Int,'a')) ]

    putStrLn "\n============ deBruijn ============"
    test "deBruijn k []    == empty                                               " $ \k ->
          deBruijn k []    == (empty :: Graph [Int])

    test "deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]" $
          deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1 :: Int]) ]

    test "deBruijn 2 \"0\"   == edge \"00\" \"00\"                                      " $
          deBruijn 2 "0"   == edge "00" "00"

    test ("deBruijn 2 \"01\"  == <correct result>                                    ") $
          deBruijn 2 "01"  == edges [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
                                    , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]

    putStrLn "\n============ removeVertex ============"

    test "removeVertex x (vertex x)       == empty         " $ \(x :: Int) ->
          removeVertex x (vertex x)       == empty

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (y :: G) ->
         (removeVertex x . removeVertex x)y==removeVertex x y

    putStrLn "\n============ removeEdge ============"

    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \(x :: Int) y ->
          removeEdge x y (edge x y)       == vertices [x, y]

    test "removeEdge x y . removeEdge x y == removeEdge x y " $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y)z==removeEdge x y z

    test "removeEdge x y . removeVertex x == removeVertex x " $ \(x :: Int) y z ->
         (removeEdge x y . removeVertex x)z==removeVertex x z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2      " $
          removeEdge 1 1 (1 * 1 * 2 * 2)  ==(1 * 2 * (2 :: G))

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2  " $
          removeEdge 1 2 (1 * 1 * 2 * 2)  ==(1 * 1 + 2 * (2 :: G))

    putStrLn "\n============ replaceVertex ============"

    test "replaceVertex x x            == id                    " $ \x (y :: G) ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y              " $ \x (y :: Int) ->
          replaceVertex x y (vertex x) == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y z          == mergeVertices (== x) y (z :: G)

    putStrLn "\n============ mergeVertices ============"

    test "mergeVertices (const False) x    == id               " $ \x (y :: G) ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y (z :: G) ->
          mergeVertices (== x) y z         == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1            " $
          mergeVertices even 1 (0 * 2)     ==(1 * 1 :: G)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1            " $
          mergeVertices odd  1 (3 + 4 * 5) ==(4 * 1 :: G)

    putStrLn "\n============ splitVertex ============"

    test "splitVertex x []                   == removeVertex x   " $ \x (y :: G) ->
         (splitVertex x []) y                == removeVertex x y

    test "splitVertex x [x]                  == id               " $ \x (y :: G) ->
         (splitVertex x [x]) y               == y

    test "splitVertex x [y]                  == replaceVertex x y" $ \x y (z :: G) ->
         (splitVertex x [y]) z               == replaceVertex x y z

    test "splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)" $
         (splitVertex 1 [0, 1] $ 1 * (2 + 3))==((0 + 1) * (2 + 3 :: G))

    putStrLn "\n============ transpose ============"

    test "transpose empty       == empty   " $
          transpose empty       ==(empty :: G)

    test "transpose (vertex x)  == vertex x" $ \(x :: Int) ->
          transpose (vertex x)  == vertex x

    test "transpose (edge x y)  == edge y x" $ \(x :: Int) y ->
          transpose (edge x y)  == edge y x

    test "transpose . transpose == id      " $ \(x :: G) ->
         (transpose . transpose) x == x

    putStrLn "\n============ fmap ============"

    test "fmap f empty      == empty           " $ \(apply -> f :: II) ->
          fmap f empty      == empty

    test "fmap f (vertex x) == vertex (f x)    " $ \(apply -> f :: II) x ->
          fmap f (vertex x) == vertex (f x)

    test "fmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          fmap f (edge x y) == edge (f x) (f y)

    test "fmap id           == id              " $ \(x :: G) ->
          fmap id x         == x

    test "fmap f . fmap g   == fmap (f . g)    " $ \(apply -> f :: II) (apply -> g :: II) (x :: G) ->
         (fmap f . fmap g) x== fmap (f . g) x

    putStrLn "\n============ >>= ============"

    test "empty >>= f       == empty                  " $ \(apply -> f :: IG) ->
         (empty >>= f)      == empty

    test "vertex x >>= f    == f x                    " $ \(apply -> f :: IG) x ->
         (vertex x >>= f)   == f x

    test "edge x y   >>= f  == connect (f x) (f y)    " $ \(apply -> f :: IG) x y ->
         (edge x y   >>= f) == connect (f x) (f y)

    test "vertices xs >>= f == overlays (map f xs)    " $ mapSize (min 10) $ \xs (apply -> f :: IG) ->
         (vertices xs >>= f)== overlays (map f xs)

    test "x >>= const empty == empty                  " $ \(x :: G) ->
         (x >>= const empty)==(empty :: G)

    test "x >>= vertex      == x                      " $ \(x :: G) ->
         (x >>= vertex)     == x

    test "(x >>= f) >>= g   == x >>= (\\y -> f y >>= g)" $ mapSize (min 10) $ \x (apply -> f :: IG) (apply -> g :: IG) ->
         ((x >>= f) >>= g)  ==(x >>= (\y  -> f y >>= g))

    putStrLn "\n============ induce ============"

    test "induce (const True)  x      == x                        " $ \(x :: G) ->
          induce (const True)  x      == x

    test "induce (const False) x      == empty                    " $ \(x :: G) ->
          induce (const False) x      == empty

    test "induce (/= x)               == removeVertex x           " $ \x (y :: G) ->
          induce (/= x) y             == removeVertex x y

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p :: IB) (apply -> q :: IB) (y :: G) ->
         (induce p . induce q) y      == induce (\x -> p x && q x) y

    test "isSubgraphOf (induce p x) x == True                     " $ \(apply -> p :: IB) (x :: G) ->
          isSubgraphOf (induce p x) x == True

    putStrLn "\n============ box ============"
    let unit = fmap $ \(a, ()) -> a
        comm = fmap $ \(a,  b) -> (b, a)
    test "box x y             ~~ box y x                    " $ mapSize (min 10) $ \(x :: G) (y :: G) ->
          comm (box x y)      == box y x

    test "box x (overlay y z) == overlay (box x y) (box x z)" $ mapSize (min 10) $ \(x :: G) (y :: G) z ->
          box x (overlay y z) == overlay (box x y) (box x z)

    test "box x (vertex ())   ~~ x                          " $ mapSize (min 10) $ \(x :: G) ->
     unit(box x (vertex ()))  == x

    test "box x empty         ~~ empty                      " $ mapSize (min 10) $ \(x :: G) ->
     unit(box x empty)        == empty

    let assoc = fmap $ \(a, (b, c)) -> ((a, b), c)
    test "box x (box y z)     ~~ box (box x y) z            " $ mapSize (min 10) $ \(x :: G) (y :: G) (z :: G) ->
      assoc (box x (box y z)) == box (box x y) z
