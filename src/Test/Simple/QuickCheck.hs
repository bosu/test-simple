module Test.Simple.QuickCheck where

import Data.Monoid
import Test.QuickCheck.Gen (Gen)

class Mutable a where
    -- | Generates next state 'b' along with tests 't' to bring us there
    mutate :: (Mutable b, Monoid t) => a -> Gen (b, t)

-- | Walks starting with 'a' for 'Int' steps producing resulting tests 't'
mutableWalk :: (Mutable a, Monoid t) => Int -> a -> Gen t
mutableWalk = go mempty
    where go m i a | i > 0 = do (b, t) <- mutate a
                                go (m `mappend` t) (i - 1) b
                   | otherwise = return m
