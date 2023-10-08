{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_practice (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "practice"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Practice exercises"
copyright :: String
copyright = ""
homepage :: String
homepage = "porguate.com"
