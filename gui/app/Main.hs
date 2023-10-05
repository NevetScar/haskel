{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.GI.Base ( on )
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
        Gtk.init Nothing
        win <- Gtk.windowNew Gtk.WindowTypeToplevel
        box <- Gtk.boxNew Gtk.OrientationVertical 2
        buttonO <- Gtk.buttonNewWithLabel "Open"
        fc <- Gtk.fileChooserWidgetNew Gtk.FileChooserActionSelectFolder
        on win #destroy Gtk.mainQuit
        Gtk.after buttonO #clicked $ do
                                      txt <- Gtk.fileChooserGetFilename fc
                                      print txt
        #add win box
        #add box buttonO
        #add box fc
        #showAll win
        Gtk.main
        putStrLn "Program finished"
