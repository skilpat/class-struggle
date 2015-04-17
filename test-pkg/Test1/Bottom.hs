module Test1.Bottom where
  import Data.Set
  import Test1.Top
  import Test1.Left
  import Test1.Right

  instance Num MyInt where
    (I m) + (I n) = I (m + n)
    (I m) * (I n) = I (m * n)
    (I m) - (I n) = I (m - n)
    abs (I m) = I (abs m)
    signum (I m) = I (signum m)
    fromInteger i = I (fromInteger i :: Int)

  --instance Real MyInt where
  --  toRational (I m) = toRational m

  funny :: Set MyInt
  funny = insR (I 1) (insL (I 1) (insL (I 2) empty))

  t :: T Int
  t = T [1,2,3]
  b = t == t

  main = print (toAscList funny)
