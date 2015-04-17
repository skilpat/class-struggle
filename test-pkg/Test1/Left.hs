module Test1.Left where
  import Data.Set
  import Test1.Top

  instance Ord MyInt where
    compare (I m) (I n) = compare m n

  instance Eq a => Eq (T a) where
    (T x) == (T y) = x == y

  insL :: MyInt -> Set MyInt -> Set MyInt
  insL = insert
