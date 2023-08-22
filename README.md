# Kozlov
A package written to help with the investigation of graph constraint satisfcation problems using topological methods as in [this paper](https://arxiv.org/abs/2003.11351).

## Graph Module
### Graph Creation Functions
- `newGraph :: Int -> [(Int, Int)] -> Graph` 
    - This is the standard graph creation function
    - Example usage: `newGraph 2 [(1, 2)]` (creates a directed graph with two vertices and a single directed edge 1->2)
- `newUndirected :: Int -> [(Int, Int)] -> Graph`
    - This just adds the inverse edge for each edge provided in the argument edge list, resulting in an undirected graph
    - Example usage: `newUndirected 2 [(1, 2)]` (creates an undirected graph with two vertices and a single undirected edge 1<->2 / two directed edges 1->2 and 1<-2)
- `clique :: Int -> Graph`
    - Given a number n, it returns a clique with n vertices
- `graphProduct :: Graph-> Graph -> Graph`
    - Returns a graph that is isomorphic to the product of the two graphs listed as arguments
### Graph "getters"
- `edgeList :: Graph -> [(Int, Int)]`
    - Returns the list of edges of the argument graph
    - Example usage: `edgeList (clique 3)` returns `[(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]`.
- `noVertices :: Graph -> Int`
    - Returns the number of vertices of the argument graph
    - Example usage: `noVertices (clique 5)` returns `5`.
### Graph Morphism Functions
- `morphismsList :: Graph -> Graph -> [[(Int, Int)]]`
    - Returns a list of mappings from the first graph argument to the second
    - Example usage: `morphismsList (clique 3) (clique 3)` returns `[[(1,1),(2,2),(3,3)],[(1,1),(2,3),(3,2)],[(1,2),(2,1),(3,3)],[(1,2),(2,3),(3,1)],[(1,3),(2,1),(3,2)],[(1,3),(2,2),(3,1)]]`.
- `findMorphism :: Graph -> Graph -> Maybe [(Int, Int)]`
    - `findMorphism g1 g2` returns `Nothing` if no graph morphism from `g1` to `g2` exists and `Just l` if one does exist, where `l` is the list representation of the morphism mapping.
    - Example usage: `findMorphism (clique 3) (clique 2)` returns `Nothing`; `findMorphism (clique 2) (clique 2)` returns `Just [(1,1),(2,2)]`.
- `hasMorphism :: Graph -> Graph -> Bool`
    - `hasMorphism g1 g2` returns `True` if a graph morphism exists from `g1` to `g2` and `False` otherwise.

### Topology Related Functions
- `toBoxComplex :: Graph -> SimplicialComplex`
    - returns the box-complex associated to the graph provided as an argument.
    - Example usage: `toBoxComplex (clique 3)` returns `SimplicialComplex [(1,2),(1,6),(2,4),(3,4),(3,5),(5,6)]`.
- `boxHomologySequence :: Graph -> [[Integer]]`
    - returns the ℤ-homology modules of the box-complex associated to the argument graph. See the homology-sequence function in the `Simplicial` module for more details.
    - example usage: `boxHomologySequence (clique 5)` returns `[[0],[],[],[0],[],[]]`.

## Simplicial Module
### The Simplex Type
- `newtype Simplex = Simplex [Int]`
### SimplicialComplex Creation Functions
- `newSimplicialComplex :: Int -> [[Int]] -> SimplicialComplex`
    - Given the number of vertices of the simplicial complex and a (not necessarily downwards-closed) set of simplices, returns a SimplicialComplex corresponding to the given arguments.
    - Note: A simplicialComplex object only stores the maximal faces provided.
    - Example usage: `newSimplicialComplex 3 [[1], [1, 2], [2, 3], [1, 3]]` returns `SimplicialComplex [(1,2),(1,3),(2,3)]`
### Predefined Simplicial Complexes
- `triangle :: SimplicialComplex`
    - a triangulation of a circle.
- `rp2 :: SimplicialComplex`
    - a triangulation of the real projective plane.
- `eight :: SimplicialComplex`
    - a triangulation of a figure 8.
- `torus :: SimplicialComplex`
    - a triangulation of a torus.
### Topology Related Functions
- `nSimplices :: SimplicialComplex -> Int -> [Simplex]`
    - Returns the list of n-simplices contained in the simplicial complex provided as an argument
    - Example usage: `nSimplices (toBoxComplex $ clique 3) 1` returns `[(1,2),(2,4),(3,4),(3,5),(1,6),(5,6)]`.
- `nthHomology :: SimplicialComplex -> Int -> ZModule`
    - `nthHomology cpx n` returns a list representing the n-th homology module of `cpx`. If the list `[t1, t2, ..., tk]` is returned, it is to represent the module $\mathbb{Z}/t_1\mathbb{Z} \oplus \dots \oplus \mathbb{Z} / t_n \mathbb{Z}$.
    - Example usage: `nthHomology rp2 1` returns `[2]` and `nthHomology torus 1` returns `[0, 0]`
- `homologySequence :: SimplicialComplex -> [[Integer]]`
    - Returns the list of homology modules. The highest homology group computed corresponds to the largest dimension of a simplex contained in the complex.
    - Example usage: `homologySequence rp2` returns `[[0], [2], []]` and `homologySequence torus` returns `[[0], [0,0], [0]]`.
- `nthBettiNumber :: SimplicialComplex -> Int -> Int`
    - Returns the n-th Betti number associated to the given simplicial complex.
- `nthTorsionNumbers :: SimplicialComplex -> Int -> [Integer]`
    - Return the n-th torsion coefficients associated to the simplicial complex.