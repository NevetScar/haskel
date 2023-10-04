{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Fudgets
import ContribFudgets (filePickF)
import AllFudgets (quitButtonF)
import Data.Char (toUpper)

main :: IO ()
main = do
        fudlogue (shellF "Hello" (mapF (fmap (map toUpper)) >==< filePickF))


