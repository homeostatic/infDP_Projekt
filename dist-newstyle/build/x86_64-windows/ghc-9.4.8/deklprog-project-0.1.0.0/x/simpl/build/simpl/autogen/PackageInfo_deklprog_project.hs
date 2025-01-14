{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_deklprog_project (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "deklprog_project"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Project template for the Prolog interpreter project"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
