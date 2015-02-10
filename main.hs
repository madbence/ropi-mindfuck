import Data.List

type Node = Int
type Edge = (Node, Node)
type Edges = [Edge]
type Pairing = Edges
type Nodes = [Node]
type Path = Edges
type Paths = [Path]
type Graph = ((Nodes, Nodes), Edges)

-- max pairing with initial pairing
maxPairing :: Graph -> Pairing -> Pairing
maxPairing g p =
  case augmentingPath g p of

    -- if there is no augmenting path, the pairing is maximal
    Nothing   -> p

    -- if there is an augmenting path, construct new pairing
    Just path -> let

      -- every odd edge is part of the new pairing
      replacement = concatMap
        (\(edge, index) -> if index `mod` 2 == 1 then [edge] else [])
        (zip path [1..length path])

      -- construct new pairing
      improvedPairing = (p `clean` path) ++ replacement

      in maxPairing g improvedPairing
  where

    -- edge-list difference
    clean p p' = filter (\(u, v) -> (u, v) `notElem` p' && (v, u) `notElem` p') p

-- find augmenting path, if possible
augmentingPath :: Graph -> Pairing -> Maybe Path
augmentingPath g p = let
  ((topNodes, bottomNodes), edges) = g

  -- select disconnected nodes
  from = filter (disconnected p) bottomNodes
  to   = filter (disconnected p) topNodes

  -- search for alternating paths from bottom nodes to top nodes
  paths = concatMap (alternatingPaths g p) [(u, v) | u <- from, v <- to]
  in case paths of

    -- If there are no good alternating paths, there is no augmenting path
    []       -> Nothing

    -- the first found path is just fine
    (path:_) -> Just path
  where
    disconnected es w = all (\(u, v) -> u /= w && v /= w) es

-- alternating paths
alternatingPaths :: Graph -> Pairing -> (Node, Node) -> Paths
alternatingPaths g p (from, to) = ap from [from]
  where

    -- dfs
    ap current visited = let

      -- get neighbour nodes
      ns = neighbours current

      in
        -- if target is neighbour
        if to `elem` ns

          -- we're done
          then makePath (to:visited)

          -- if target is not a neighbour, then search recursively from neighbours
          else let

            -- map every candidate to Path-list
            f = \n -> ap n (n:visited)

            -- select candidate nodes based on the current parity
            candidates = let

              f = \n -> let

                -- neighbour is marked, if connecting edge is part of the current pairing
                marked = (n, current) `elem` p || (current, n) `elem` p

                -- parity is calculated from path length
                parity = length visited `mod` 2 == 1

                -- node is candidate if we are at the bottom half, and node is not marked
                -- or if we are at the top half and node is marked
                in parity == True && not marked || parity == False && marked

              -- select candidates
              in filter f ns

            -- concat path-list from candidates
            in concatMap f candidates

    -- restore Edges from Nodes
    makePath :: Nodes -> [Edges]
    makePath ns = [zip ns (drop 1 ns)]

    -- neighbour nodes
    neighbours :: Node -> Nodes
    neighbours n = let
      ((top, bottom), edges) = g
      in if n `elem` bottom
        then filter (\v -> (v, n) `elem` edges) top
        else filter (\v -> (n, v) `elem` edges) bottom
