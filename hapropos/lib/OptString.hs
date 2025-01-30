module OptString where

import PLProp as PL
import Data.Word (Word8)
import Data.Vector


plSize :: PL.Prop -> Int
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

populateVector :: PL.Prop -> Vector Word8 -> Vector Word8

lookupStringInVector :: String -> Vector Word8 -> Maybe Word8
lookupStringInVector s v = Data.Vector.findIndex s v -- findIndex is o(n) in the length of the vector!

optString :: PL.Prop -> Vector String -> Vector VariantDataInt
optString (PL.TruthValue True) sv =

-- we want to replace every string with an int that is its index in the string storage
type VariantDataW = VTW | VFW | VVW Word8 | VNW | VAW | VOW | VIW

-- this may also be o(n^2) but e only have to do it once
replaceStringsList :: PL.Prop -> Word8 -> [VariantDataInt]
replaceStringsList (PL.TruthValue True) _   = VTW
replaceStringsList (PL.TruthValue False) _  = VFW
replaceStringsList (PL.Var s) preorderIndex = VVW preorderIndex
replaceStringsList (PL.Not p) preorderIndex = VNW : replaceStringsList p (preorderIndex + 1)
replaceStringsList (PL.And p q) preorderIndex     = VAW : replaceStringsList p (preorderIndex + 1) ++ replaceStringsList q (preorderIndex + plSize p) --plSize might be an off by one error
replaceStringsList (PL.Or p q) preorderIndex      = VOW : replaceStringsList p (preorderIndex + 1) ++ replaceStringsList q (preorderIndex + plSize p)
replaceStringsList (PL.Implies p q) preorderIndex = VIW : replaceStringsList p (preorderIndex + 1) ++ replaceStringsList q (preorderIndex + plSize p)

-- o(n) as long as plSize is o(1) or you memoize it. but plSize is naive above, so it’s o(n) each time, making it o(n^2) in the worst case.
replaceStringsVector
  :: PLProp                 -- the current formula
  -> Word8                  -- current node's preorder index
  -> Vector VariantDataInt  -- the "big" vector we're filling
  -> Vector VariantDataInt  -- result with updates
replaceStringsVector (TruthValue True) preOrderIndex vec =
  let i = toInt preOrderIndex
  in vec // [(i, VTW)]

replaceStringsVector (TruthValue False) preOrderIndex vec =
  let i = toInt preOrderIndex
  in vec // [(i, VFW)]

replaceStringsVector (Var _) preOrderIndex vec =
  let i = toInt preOrderIndex
  in vec // [(i, VVW preOrderIndex)]
  -- or store something else if you want, e.g. a hashing or indexing scheme

replaceStringsVector (Not p) preOrderIndex vec =
  let i  = toInt preOrderIndex
      -- store this node in the vector
      vec1 = vec // [(i, VNW)]
      -- the child goes at preOrderIndex + 1
  in replaceStringsVector p (preOrderIndex + 1) vec1

replaceStringsVector (And p q) preOrderIndex vec =
  let i  = toInt preOrderIndex
      -- store this node
      vec1 = vec // [(i, VAW)]
      -- left child is next index
      vec2 = replaceStringsVector p (preOrderIndex + 1) vec1
      -- skip over the size of the left subtree to find right child's index
      leftSize = plSize p
      vec3 = replaceStringsVector q (preOrderIndex + 1 + fromIntegral leftSize) vec2
  in vec3

replaceStringsVector (Or p q) preOrderIndex vec =
  let i  = toInt preOrderIndex
      vec1 = vec // [(i, VOW)]
      vec2 = replaceStringsVector p (preOrderIndex + 1) vec1
      leftSize = plSize p
      vec3 = replaceStringsVector q (preOrderIndex + 1 + fromIntegral leftSize) vec2
  in vec3

replaceStringsVector (Implies p q) preOrderIndex vec =
  let i  = toInt preOrderIndex
      vec1 = vec // [(i, VIW)]
      vec2 = replaceStringsVector p (preOrderIndex + 1) vec1
      leftSize = plSize p
      vec3 = replaceStringsVector q (preOrderIndex + 1 + fromIntegral leftSize) vec2
  in vec3
