{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances #-}
module Test2.Left where

import Test2.Top

data T = T Int

instance C T b where
  -- c :: T -> [b] -> Int
  c (T n) _ = n

