{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

-- AA search trees
data AATree a
    = Null
    | Node a Int (AATree a) (AATree a)
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Null

-- |Return (Just value) if it exists, otherwise Nothing.
-- Complexity: O(lg n)
--
-- (Note: Since our structure is actually a BST node we just go left or right
-- until we find the correct value, the added level indicating the 2-3
-- difference is irrelevant for searching.)
get :: Ord a => a -> AATree a -> Maybe a
get _ Null = Nothing
get val (Node v _ l r)
    | val == v  = Just v    -- Found value
    | val < v   = get val l -- Look left if searched for value is less
    | otherwise = get val r -- Look right if searched for value is greater

-- |Insert value into tree.
-- Complexity: O(lg n)
insert :: Ord a => a -> AATree a -> AATree a
insert val Null = (Node val 1 Null Null)
insert val n@(Node v lvl l r)
    | val < v   = split . skew $ (Node v lvl (insert val l) r)
    | val > v   = split . skew $ (Node v lvl l (insert val r))
    | otherwise = n -- Value is already in list

-- |Rebalance a 2-3 node where we have a left child on the same level by
-- rotation.
-- Complexity: O(1)
skew :: AATree a -> AATree a
skew Null                   = Null
skew n@(Node _ _ Null _)    = n
skew n@(Node nv lvl (Node lv llvl ll lr) r)
    | lvl /= llvl     = n -- Nothing to skew
    | otherwise       =
        Node lv lvl ll (Node nv lvl lr r)

-- |Split a 4 node (i.e., right grand child at same level) into two 2-nodes.
-- Complexity: O(1)
split :: AATree a -> AATree a
split Null                             = Null
split n@(Node _ _ _ Null)              = n
split n@(Node _ _ _ (Node _ _ _ Null)) = n
split x@(Node xv xlvl xl {-y@-}(Node yv ylvl yl z@(Node _ zlvl _ _)))
    | xlvl /= zlvl = x -- Nothing to split
    | otherwise    =
        -- Reorder into 2-nodes with the middle as new parent
        Node yv (ylvl+1) (Node xv xlvl xl yl) z

-- |Return the values stored in the tree as a list sorted in ascending order.
-- Complexity: O(n^2)
--
-- Complexity of our own traverals is O(n) and complexity of inner work (++) is
-- O(n), making total complexity O(n^2)
inorder :: AATree a -> [a]
inorder Null = []
inorder (Node v _ l r) = (inorder l) ++ v:(inorder r)

-- |Return the number of nodes in the tree.
-- Complexity: O(n)
size :: AATree a -> Int
size Null = 0
size (Node _ _ l r) = 1 + size l + size r

-- |Return height of tree.
-- Complexity: O(n)
--
-- (NOTE: that the conceptual height is different from the actual height in an
-- AATree. I.e., we think conceptually of a 2-3 node as having a right child
-- that's on the same level, but since the actual structure is a BST this is not
-- true. To calculate the actual height we calculate the height identically to
-- how we would for a BST.)
height :: AATree a -> Int
height Null = 0
height (Node _ _ l r) = 1 + max (height l) (height r)

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
-- Complexity: O(n)
isSorted :: Ord a => [a] -> Bool
isSorted []        = True
isSorted (_:[])    = True
isSorted (x:y:xys) = x <= y && isSorted (y:xys)

-- Check if the AA tree invariant is true for a single AA node
checkLevels :: AATree a -> Bool
checkLevels Null = True
checkLevels node
    = leftChildOk node && rightChildOk node && rightGrandChildOk node
  where
    -- Left Child
    leftChildOk Null                = False -- Impossible, only to quelch warning
    -- INVARIANT: Level of leaf must be 1
    leftChildOk (Node _ lvl Null _) = lvl == 1
    -- INVARIANT: Level of left child must be one smaller than parent
    leftChildOk (Node _ lvl (Node _ left_lvl _ _) _)
        = left_lvl == lvl-1

    -- Right Child
    rightChildOk Null                = False -- Impossible, only to quelch warning
    -- INVARIANT: Level of leaf must be 1
    rightChildOk (Node _ lvl _ Null) = lvl == 1
    -- INVARIANT: Level of right child must be equal to or one smaller than parent
    rightChildOk (Node _ lvl _ (Node _ right_lvl _ _))
        = right_lvl == lvl || right_lvl == lvl-1

    
    -- Right Grand Child
    rightGrandChildOk Null                           = False -- Impossible, only to quelch warning
    rightGrandChildOk (Node _ _ _ Null)              = True  -- Only for warnings sake, cond checked in rightChildOk
    rightGrandChildOk (Node _ _ _ (Node _ _ _ Null)) = True
    -- INVARIANT: Right grandchild's level is strictly less than grandparent
    rightGrandChildOk (Node _ lvl _ (Node _ _ _ (Node _ right_gc_lvl _ _)))
        = right_gc_lvl < lvl


isEmpty :: AATree a -> Bool
isEmpty Null = True
isEmpty _    = False

leftSub :: AATree a -> AATree a
leftSub Null           = Null
leftSub (Node _ _ l _) = l

rightSub :: AATree a -> AATree a
rightSub Null           = Null
rightSub (Node _ _ _ r) = r
