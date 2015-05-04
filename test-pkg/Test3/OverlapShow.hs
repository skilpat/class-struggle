{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Test3.OverlapShow where

data T = T { tStr :: String }

instance Show [T] where
  show ts = concat $ map tStr ts

