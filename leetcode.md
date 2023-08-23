# Problem [1](https://leetcode.com/problems/two-sum/) - Two Sum

```haskell
twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum n t =
  let n' = zip n [0 ..]
      n'' = sortOn (\(x, y) -> x) n'
      r = reverse n''
   in twoSum' n'' r t
  where
    twoSum' _ [] _ = Nothing
    twoSum' [] _ _ = Nothing
    twoSum' ((n, ni):ns) ((r, ri):rs) t
      | n + r == t = Just (ni, ri)
      | n + r < t = twoSum' ns ((r, ri) : rs) t
      | otherwise = twoSum' ((n, ni) : ns) rs t
```
