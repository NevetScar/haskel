{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.GI.Base ( on )
import System.Process ( callCommand )
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
        _ <- Gtk.init Nothing
        win <- Gtk.windowNew Gtk.WindowTypeToplevel
        box <- Gtk.boxNew Gtk.OrientationVertical 2
        buttonO <- Gtk.buttonNewWithLabel "Open"
        fc <- Gtk.fileChooserWidgetNew Gtk.FileChooserActionOpen
        _ <- on win #destroy Gtk.mainQuit
        _ <- Gtk.after buttonO #clicked $ do
                                      txt <- Gtk.fileChooserGetFilename fc
                                      case txt of
                                       Nothing -> putStrLn "Nothing selected"
                                       Just s -> do
                                                  callCommand $ "xdg-open " ++ show s
                                                  Gtk.mainQuit
        #add win box
        #add box buttonO
        #add box fc
        #showAll win
        Gtk.main
        putStrLn "Program finished"
