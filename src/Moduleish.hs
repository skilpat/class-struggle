{-# LANGUAGE StandaloneDeriving #-}
module Moduleish where

-- import Control.Monad

-- GHC imports
import Module
import Outputable
import Unique

-- | Basically just a Module, but with an additional bit that determines whether
--   this refers to a boot file (True) or a normal module implementation
--   (False).
data Moduleish = Moduleish { mish_mod  :: !Module
                           , mish_boot :: !Bool }

deriving instance Eq Moduleish
deriving instance Ord Moduleish

-- | Make a non-boot Moduleish for the given Module.
mkModuleish :: Module -> Moduleish
mkModuleish mod = Moduleish mod False

mishPkgStr :: Moduleish -> String
mishPkgStr mish = packageIdString $ modulePackageId $ mish_mod mish

mishModStr :: Moduleish -> String
mishModStr mish = moduleNameString $ moduleName $ mish_mod mish

mkModuleishFull :: String -> Moduleish
mkModuleishFull s = Moduleish mod boot
  where
    (ps, ':':mbs) = span (/= ':') s
    pid = stringToPackageId ps
    (ms, rest) = span (/= '[') mbs
    mname = mkModuleName ms
    mod = mkModule pid mname
    boot = not $ null rest

-- | Do the two Moduleishes point to the same Module but different boot status?
similarish :: Moduleish -> Moduleish -> Bool
similarish mish1 mish2 = mish_mod mish1 == mish_mod mish2
                         && ((b1 && not b2) || (not b1 && b2))
  where
    b1 = mish_boot mish1
    b2 = mish_boot mish2

-- | The Unique is simply that of the module if this is not a boot file; if it
--   is a boot file, then tack a 0 onto the module's Unique just to make it
--   distinct.
instance Uniquable Moduleish where
  getUnique mish | mish_boot mish = deriveUnique mod_uniq 0
                 | otherwise      = mod_uniq
    where
      mod_uniq = getUnique $ mish_mod mish

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
