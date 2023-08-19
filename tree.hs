data Tree = Leaf | Node Int Tree Tree deriving Show
 
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)


treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value left right) = value + treeSum left + treeSum right

-- TODO: Convert from tree to list