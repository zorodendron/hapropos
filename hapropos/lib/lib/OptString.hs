module OptString where

import PLProp as PL
import Data.Word (Word8)
import qualified Data.Vector as V

plSize :: (Num a) => PLProp -> a
plSize (PL.TruthValue _) = 1
plSize (Var s)           = 1
plSize (Not p)           = 1 + plSize p
plSize (And p q)         = 1 + plSize p + plSize q
plSize (Or p q)          = 1 + plSize p + plSize q
plSize (Implies p q)     = 1 + plSize p + plSize q

-- why use 2 for strings vs 1:
-- 2 enables indexing unless a separate array maps strings to Var node indices.
-- 1 allows plSize to track subtree sizes for replaceStringsList.
-- extra space for strings breaks VariantDataInt arrays since Var indices already store VariantDataInt, which can't hold strings.

-- avoid concatenation since all functions are O(n) or O(n+m).
-- instead, preallocate the vector after counting nodes.




data VariantDataW = VTW | VFW | VVW Word8 | VNW | VAW | VOW | VIW deriving (Show, Eq)

-- this may also be o(n^2) but e only have to do it once
replaceStringsList :: PL.PLProp -> Word8 -> [VariantDataW]
replaceStringsList (PL.TruthValue True) _   = [VTW]
replaceStringsList (PL.TruthValue False) _  = [VFW]
replaceStringsList (PL.Var s) preorderIndex = [VVW preorderIndex]
replaceStringsList (PL.Not p) preorderIndex = VNW : replaceStringsList p (preorderIndex + 1)
replaceStringsList (PL.And p q) preorderIndex =
  VAW : replaceStringsList p (preorderIndex + 1)
      ++ replaceStringsList q (preorderIndex + 1 + plSize p)
replaceStringsList (PL.Or p q) preorderIndex =
  VOW : replaceStringsList p (preorderIndex + 1)
      ++ replaceStringsList q (preorderIndex + 1 + plSize p)
replaceStringsList (PL.Implies p q) preorderIndex =
  VIW : replaceStringsList p (preorderIndex + 1)
      ++ replaceStringsList q (preorderIndex + 1 + plSize p)

-- o(n) as long as plSize is o(1) or i memoize it. but plSize is o(n), so it’s o(n) each time, making it o(n^2) in the worst case.
replaceStringsVector
  :: PLProp                 -- the current formula
  -> Word8                  -- current node's preorder index
  -> V.Vector VariantDataW  -- the "big" vector we're filling
  -> V.Vector VariantDataW  -- result with updates
replaceStringsVector (TruthValue True) preOrderIndex vec =
  let i = fromIntegral preOrderIndex
  in vec V.// [(i, VTW)]

replaceStringsVector (TruthValue False) preOrderIndex vec =
  let i = fromIntegral preOrderIndex
  in vec V.// [(i, VFW)]

replaceStringsVector (Var _) preOrderIndex vec =
  let i = fromIntegral preOrderIndex
  in vec V.// [(i, VVW preOrderIndex)]
  -- or store something else if you want, e.g. a hashing or indexing scheme

replaceStringsVector (Not p) preOrderIndex vec =
  let i  = fromIntegral preOrderIndex
      -- store this node in the vector
      vec1 = vec V.// [(i, VNW)]
      -- the child goes at preOrderIndex + 1
  in replaceStringsVector p (preOrderIndex + 1) vec1

replaceStringsVector (And p q) preOrderIndex vec =
  let i  =fromIntegral preOrderIndex
      -- store this node
      vec1 = vec V.// [(i, VAW)]
      -- left child is next index
      vec2 = replaceStringsVector p (preOrderIndex + 1) vec1
      -- skip over the size of the left subtree to find right child's index
      leftSize = plSize p
      vec3 = replaceStringsVector q (preOrderIndex + 1 + fromIntegral leftSize) vec2
  in vec3

replaceStringsVector (Or p q) preOrderIndex vec =
  let i  = fromIntegral preOrderIndex
      vec1 = vec V.// [(i, VOW)]
      vec2 = replaceStringsVector p (preOrderIndex + 1) vec1
      leftSize = plSize p
      vec3 = replaceStringsVector q (preOrderIndex + 1 + fromIntegral leftSize) vec2
  in vec3

replaceStringsVector (Implies p q) preOrderIndex vec =
  let i  = fromIntegral preOrderIndex
      vec1 = vec V.// [(i, VIW)]
      vec2 = replaceStringsVector p (preOrderIndex + 1) vec1
      leftSize = plSize p
      vec3 = replaceStringsVector q (preOrderIndex + 1 + fromIntegral leftSize) vec2
  in vec3

  -- maybe you can do this
  -- construct a list of all the indices and corresponding VariantDataW values
  -- update all of them for the vector at once instead of individually
  -- if we did this we would:
  -- initialise a vector with the same size as the AST
  -- construct a list of all the indices and corresponding VariantDataW values
  -- update all of them for the vector at once instead of individually
