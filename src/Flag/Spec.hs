module Internal.Spec where

import           Flag.Internal
import           Test.QuickCheck

spec_splitBy :: Char -> String -> Bool
spec_splitBy x xs = xs == test
  where
    test = case _splitBy x xs of
             Just (y,z) -> y ++ [x] ++ z
             Nothing    -> xs
