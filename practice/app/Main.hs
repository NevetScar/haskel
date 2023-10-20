module Main (main) where

import Control.Monad
  ( unless,
    when,
    (<=<),
  )
import qualified Data.Char as C
import Data.List (sort)
import Data.Text (Text, concat, isInfixOf, isSuffixOf, pack, stripPrefix, takeWhile, toLower)
import qualified Data.Text.IO as TIO
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
    bp <- map C.toLower searchTerm `findIn` lstPdf
    pure (d, sort bp)

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
