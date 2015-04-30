{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances #-}
module Test2.Right where

import Test2.Top

data U = U Int

instance C a U where
  -- c :: [a] -> U -> Int
  c _ (U n) = n

