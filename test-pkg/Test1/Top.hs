module Test1.Top where
  data MyInt = I Int deriving (Eq, Show)

  data T a = T [a]
