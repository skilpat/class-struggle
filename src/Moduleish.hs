{-# LANGUAGE StandaloneDeriving #-}
module Moduleish where

-- import Control.Monad
import Data.Hashable

-- GHC imports
import Module
import Outputable
import Unique

-- | Basically just a Module, but with an additional bit that determines whether
--   this refers to a boot file (True) or a normal module implementation
--   (False).
data Moduleish = Moduleish { mish_mod  :: !Module
                           , mish_boot :: !Bool
                           , mish_uniq :: !Unique }

-- Instead of deriving Eq, try to short-circuit by checking uniqs, which, if
-- not equal, indicate that the mods+boots won't be equal. But if uniqs are
-- equal, we need to check the mods+boots.
instance Eq Moduleish where
  m1 /= m2
    | mish_uniq m1 /= mish_uniq m2 = True
    | otherwise =
      (mish_boot m1 /= mish_boot m2) || (mish_mod m1 == mish_mod m2)
  m1 == m2
    | mish_uniq m1 == mish_uniq m2 =
      (mish_boot m1 == mish_boot m2) && (mish_mod m1 == mish_mod m2)
    | otherwise = False

-- Compare based only on module and boot, not the cached unique.
instance Ord Moduleish where
  compare m1 m2 = case compare (mish_mod m1) (mish_mod m2) of
    LT -> LT
    GT -> GT
    EQ -> compare (mish_boot m1) (mish_boot m2)

-- | The Unique is simply that of the module if this is not a boot file; if it
--   is a boot file, then tack a 0 onto the module's Unique just to make it
--   distinct.
instance Uniquable Moduleish where
  getUnique = mish_uniq

-- Hash of Moduleish is just hash of the Unique's key
instance Hashable Moduleish where
  hashWithSalt salt mish = hashWithSalt salt $ getKey $ mish_uniq mish

-- | Calculate the Unique for the Moduleish's constituent parts.
calcModBootUniq :: Module -> Bool -> Unique
calcModBootUniq mod boot | boot      = deriveUnique mod_uniq 0
                      | otherwise = mod_uniq
    where
      mod_uniq = getUnique $! mod

-- | Make a Moduleish.
mkModuleish :: Module -> Bool -> Moduleish
mkModuleish mod boot = Moduleish mod boot $! calcModBootUniq mod boot

-- | Make a non-boot Moduleish for the given Module.
mkModuleishImpl :: Module -> Moduleish
mkModuleishImpl mod = mkModuleish mod False

mkModuleishFull :: String -> Moduleish
mkModuleishFull s = mkModuleish mod boot
  where
    (ps, ':':mbs) = span (/= ':') s
    pid = stringToPackageId ps
    (ms, rest) = span (/= '[') mbs
    mname = mkModuleName ms
    mod = mkModule pid mname
    boot = not $ null rest

mishPkgStr :: Moduleish -> String
mishPkgStr mish = packageIdString $ modulePackageId $ mish_mod mish

mishModStr :: Moduleish -> String
mishModStr mish = moduleNameString $ moduleName $ mish_mod mish

mishSamePkg :: Moduleish -> Moduleish -> Bool
mishSamePkg mish1 mish2 = pkg1 == pkg2
  where
    pkg1 = modulePackageId $ mish_mod mish1
    pkg2 = modulePackageId $ mish_mod mish2

-- | Do the two Moduleishes point to the same Module but different boot status?
similarish :: Moduleish -> Moduleish -> Bool
similarish mish1 mish2 = mish_mod mish1 == mish_mod mish2
                         && ((b1 && not b2) || (not b1 && b2))
  where
    b1 = mish_boot mish1
    b2 = mish_boot mish2

instance Show Moduleish where
  show mish | mish_boot mish = s ++ "[boot]"
            | otherwise      = s
    where
      s = mishPkgStr mish ++ ":" ++ mishModStr mish

instance Outputable Moduleish where
  ppr mish = text $ show mish

-- instance Show Moduleish where
--   show mish | mish_boot mish = mod_str ++ "[boot]"
--             | otherwise      = mod_str
--     where
--       mod_str = moduleNameString $ moduleName $ mish_mod mish
