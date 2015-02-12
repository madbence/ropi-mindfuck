import Data.List

type Node = Int
type Edge = (Node, Node)
type Edges = [Edge]
type Pairing = Edges
type Nodes = [Node]
type Path = Edges
type Paths = [Path]
type Graph = ((Nodes, Nodes), Edges)

maxPairing :: Graph -> Pairing -> Pairing
maxPairing g p = case augmentingPath g p of
  Nothing   -> p
  Just path ->
    let indexedPath     = zip path [1..length path]
        replacement     = concatMap keepOddEdges indexedPath
        improvedPairing = (p `clean` path) ++ replacement
    in maxPairing g improvedPairing
  where
    keepOddEdges (edge, index) = if index `mod` 2 == 1
                                    then [edge]
                                    else []
    notPresent p (u, v) = (u, v) `notElem` p && (v, u) `notElem` p
    clean p p' = filter (notPresent p') p

augmentingPath :: Graph -> Pairing -> Maybe Path
augmentingPath g p =
  let ((topNodes, bottomNodes), edges) = g
      from = filter (disconnected p) bottomNodes
      to   = filter (disconnected p) topNodes
      paths = concatMap (alternatingPaths g p) [(u, v) | u <- from, v <- to]
  in case paths of
    []       -> Nothing
    (path:_) -> Just path
  where
    disconnected es w = all (\(u, v) -> u /= w && v /= w) es

alternatingPaths :: Graph -> Pairing -> (Node, Node) -> Paths
alternatingPaths g p (from, to) = ap from [from]
  where
    ap current visited =
      let ns = neighbours current
      in if to `elem` ns
            then makePath (to:visited)
            else concatMap pathsFrom (filter goodCandidate ns)
      where
        pathsFrom n = ap n (n:visited)
        goodCandidate n = let marked = (n, current) `elem` p || (current, n) `elem` p
                              parity = length visited `mod` 2 == 1
                          in parity == True && not marked || parity == False && marked
        makePath ns = [zip ns (drop 1 ns)]

        neighbours n =
          let ((top, bottom), edges) = g
          in if n `elem` bottom
               then filter (\v -> (v, n) `elem` edges) top
               else filter (\v -> (n, v) `elem` edges) bottom
