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
data Moduleish = Moduleish { mish_mod  :: Module
                           , mish_boot :: Bool }

deriving instance Eq Moduleish
deriving instance Ord Moduleish


-- | The Unique is simply that of the module if this is not a boot file; if it
--   is a boot file, then tack a 0 onto the module's Unique just to make it
--   distinct.
instance Uniquable Moduleish where
  getUnique mish | mish_boot mish = deriveUnique mod_uniq 0
                 | otherwise      = mod_uniq
    where
      mod_uniq = getUnique $ mish_mod mish



instance Outputable Moduleish where
  ppr mish | mish_boot mish = mod_sdoc <> text "[boot]"
           | otherwise      = mod_sdoc
    where
      mod_sdoc = ppr $ mish_mod mish