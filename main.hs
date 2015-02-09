type Node = Int
type Edge = (Node, Node)
type Edges = [Edge]

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten (x:xs) = x ++ flatten xs

paired :: Edges -> Node -> Bool
paired [] n = False
paired ((u, v):ps) w
  | u == w    = True
  | v == w    = True
  | otherwise = paired ps w

edges :: Edges -> Node -> Edges
edges [] _ = []
edges (e@(u, v):es) w
  | u == w    = (e:edges es w)
  | v == w    = (e:edges es w)
  | otherwise = edges es w

nodes :: Edges -> [Node]
nodes 


isInEdgeList :: Edges -> Edge -> Bool
isInEdgeList [] _ = False
isInEdgeList ((u, v):es) e@(u', v')
  | u == u' && v == v' = True
  | otherwise          = isInEdgeList es e

isAugmenting :: Edges -> Edges -> Bool
isAugmenting ps path = all isRight (zip path [1..(length path)])
  where isRight (e, n) = case n `mod` 2 of 0 -> isInEdgeList ps e
                                           1 -> not (isInEdgeList ps e)

augmentingPath :: Edges -> Edges -> Maybe Edges
augmentingPath es ps = augmentingPath' es ps (nodes ps)

augmentingPath' es ps [] = Nothing
augmentingPath' es ps (n:ns) = case augmentingPath'' es ps n [] of Nothing -> augmentingPath' es ps ns
                                                                   Just path -> path

-- TODO: augmentingPath'''
