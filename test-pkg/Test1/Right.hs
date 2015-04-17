module Test1.Right where
  import Data.Set
  import Test1.Top

  instance Ord MyInt where
    compare (I m) (I n) = compare n m

  instance Eq (T a) where
    (T x) == (T y) = True

  insR :: MyInt -> Set MyInt -> Set MyInt
  insR = insert
