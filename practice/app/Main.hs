{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
  ( unless,
    when,
    (<=<),
  )
import qualified Data.Char as C
import Data.GI.Base (on)
import Data.List (sort)
import Data.Text (Text, concat, isInfixOf, isSuffixOf, pack, stripPrefix, takeWhile, toLower)
import qualified Data.Text.IO as TIO
import qualified GI.Gtk as Gtk
import Pdf.Document
  ( catalogPageNode,
    defaultUserPassword,
    document,
    documentCatalog,
    isEncrypted,
    pageExtractText,
    pageNodeNKids,
    pageNodePageByNum,
    setUserPassword,
    withPdfFile,
  )
import System.Directory (getDirectoryContents)
import Prelude hiding (concat, takeWhile)

main :: IO ()
main = myIo

proctoSearch :: (String, [(Bool, FilePath)]) -> IO ()
proctoSearch (d, sorte) =
  do
    let z = map (stripPrefix (pack d) . (takeWhile (/= '_') . pack . snd)) sorte
    putStrLn $ "----------Se encontraron: " ++ show (length z) ++ " coincidencias en " ++ d ++ "----------"
    mapM_ mayfil z
  where
    mayfil :: Maybe Text -> IO ()
    mayfil Nothing = putStrLn "Nothing to remove"
    mayfil (Just x) = TIO.putStrLn (x <> pack ",")

genericSearch :: (String, [(Bool, FilePath)]) -> IO ()
genericSearch (d, sorte) =
  do
    putStrLn $ "----------Se encontraron: " ++ show (length sorte) ++ " coincidencias en " ++ d ++ "----------"
    mapM_ (TIO.putStrLn . pack . snd) sorte

findIn :: String -> [FilePath] -> IO [(Bool, FilePath)]
findIn searchTerm lstPdf =
  do
    txts <- getTextFromEach lstPdf
    let y = map (`hasSearchTerm` searchTerm) txts
    let zipped = zip y lstPdf
    pure (filter fst zipped)

getListOfPDFs :: FilePath -> IO [FilePath]
getListOfPDFs d =
  do
    a <- getDirectoryContents d
    let b = filter (isSuffixOf (pack "pdf") . pack) a
    let x = (d <>) <$> b
    pure x

getTextFromEach :: [FilePath] -> IO [Text]
getTextFromEach = mapM getTextPdf

getTextPdf :: FilePath -> IO Text
getTextPdf fp =
  withPdfFile fp $ \pdf -> do
    encrypted <- isEncrypted pdf
    when encrypted $ do
      ok <- setUserPassword pdf defaultUserPassword
      unless ok $
        fail "need password"
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    countx <- pageNodeNKids rootNode
    let sizei = [0 .. (countx - 1)]
    concat <$> mapM (pageExtractText <=< pageNodePageByNum rootNode) sizei

hasSearchTerm :: Text -> String -> Bool
hasSearchTerm txt searchTerm = pack searchTerm `isInfixOf` toLower txt

myIo :: IO ()
myIo = do
  putStrLn "Presione p para proctoSearch o deje en blanco para búsqueda genérica: "
  option <- getLine
  putStrLn "Termino a buscar: "
  searchTerm <- getLine
  putStrLn "Escoja el directorio"
  _ <- Gtk.init Nothing
  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  box <- Gtk.boxNew Gtk.OrientationVertical 2
  buttonO <- Gtk.buttonNewWithLabel "Open"
  fc <- Gtk.fileChooserWidgetNew Gtk.FileChooserActionSelectFolder
  _ <- on win #destroy Gtk.mainQuit
  _ <- Gtk.after buttonO #clicked $ do
    txt <- Gtk.fileChooserGetFilename fc
    case txt of
      Nothing -> putStrLn "Nothing selected"
      Just s -> do
        let cs = s ++ "/"
        lstPdf <- getListOfPDFs cs
        bp <- map C.toLower searchTerm `findIn` lstPdf
        let ss = (cs, sort bp)
        if option == "p" then proctoSearch ss else genericSearch ss
        Gtk.mainQuit
  #add win box
  #add box buttonO
  #add box fc
  #showAll win
  Gtk.main
  putStrLn "Program finished"