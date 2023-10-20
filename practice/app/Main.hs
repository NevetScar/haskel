module Main (main) where

import Pdf.Document
import Control.Monad
import Data.Text
import System.Directory
import qualified Data.Text.IO as Data.Text
import qualified Data.List as Prelude
import qualified Data.Char as Prelude

main :: IO ()
main =
  do
    putStrLn "Presione p para proctoSearch o deje en blanco para búsqueda genérica: "
    option <- getLine
    ss <- getSortedSearch
    if option == "p" then proctoSearch ss else genericSearch ss

getSortedSearch :: IO (String, [(Bool, FilePath)])
getSortedSearch =
  do
    putStrLn "Termino a buscar: "
    searchTerm <- getLine
    putStrLn "Escoja el directorio"
    d <- getLine
    lstPdf <- getListOfPDFs d
    bp <- Prelude.map Prelude.toLower searchTerm `findIn` lstPdf
    pure (d, Prelude.sort bp)

proctoSearch :: (String, [(Bool, FilePath)]) -> IO ()
proctoSearch (d, sorte) = 
  do
    let z = Prelude.map (Data.Text.stripPrefix (pack d) . (Data.Text.takeWhile (/= '_') . pack . snd)) sorte
    putStrLn $ "----------Se encontraron: " ++ show (Prelude.length z) ++ " coincidencias en " ++ d ++ "----------"
    mapM_ mayfil z
  where
    mayfil :: Maybe Text -> IO ()
    mayfil Nothing = putStrLn "Nothing to remove"
    mayfil (Just x)  = Data.Text.putStrLn (x <> pack ",")

genericSearch :: (String, [(Bool, FilePath)]) -> IO ()
genericSearch (d, sorte) = 
  do
    putStrLn $ "----------Se encontraron: " ++ show (Prelude.length sorte) ++ " coincidencias en " ++ d ++ "----------"
    mapM_ (Data.Text.putStrLn . pack . snd) sorte

findIn ::  String -> [FilePath] -> IO [(Bool, FilePath)]
findIn searchTerm lstPdf =
  do
    txts <- getTextFromEach lstPdf
    let y = Prelude.map (`hasSearchTerm` searchTerm) txts
    let zipped = Prelude.zip y lstPdf
    pure (Prelude.filter fst zipped)

getListOfPDFs :: FilePath -> IO [FilePath]
getListOfPDFs d =
  do
    a <- getDirectoryContents d
    let b = Prelude.filter (Data.Text.isSuffixOf (pack "pdf") . pack) a
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
    let sizei = [0..(countx - 1)]
    Data.Text.concat <$> mapM (pageExtractText <=< pageNodePageByNum rootNode) sizei

hasSearchTerm :: Text -> String -> Bool
hasSearchTerm txt searchTerm = pack searchTerm `isInfixOf` toLower txt
