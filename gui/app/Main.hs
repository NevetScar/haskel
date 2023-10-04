{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.GI.Base ( on, AttrOp((:=)), new )
import qualified GI.Gtk as Gtk
import GI.Gtk (FileChooserAction(FileChooserActionOpen))

main :: IO ()
main = do
        Gtk.init Nothing
        win <- new Gtk.Window [#title := "Choose a file"]
        fc <- new Gtk.FileChooserDialog [ #action := FileChooserActionOpen ]
        button <- new Gtk.Button [ #label := "Open dialog"]
        on button #clicked(putStrLn "Hello world" )
        on win #destroy Gtk.mainQuit
        #add win button
        #showAll win
        Gtk.main
        putStrLn "Program finished"


trial :: Gtk.FileChooserDialog -> IO FileChooserAction
trial x = Gtk.fileChooserGetAction x
