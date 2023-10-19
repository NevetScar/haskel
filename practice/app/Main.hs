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
    putStrLn "Termino a buscar: "
    searchTerm <- getLine
    bp <- matchSearchIn (Prelude.map Prelude.toLower searchTerm) getListOfPDFs
    let sorte = Prelude.sort bp
    let z = Prelude.map (Data.Text.stripPrefix (pack currentDir) . (Data.Text.takeWhile (/= '_') . pack . snd)) sorte
    putStrLn $ "----------Se encontraron: " ++ show (Prelude.length z) ++ " coincidencias----------"
    mapM_ mayfil z


mayfil :: Maybe Text -> IO ()
mayfil Nothing = putStrLn "Nothing to remove"
mayfil (Just x)  = Data.Text.putStrLn (x <> pack ",")

matchSearchIn :: String -> IO [FilePath] -> IO [(Bool, FilePath)]
matchSearchIn searchTerm lstPdf = Prelude.filter fst <$> (Prelude.zip <$> filterEx lstPdf searchTerm  <*> lstPdf)


filterEx :: IO [FilePath] -> String -> IO [Bool]
filterEx lstPdf searchTerm =
  do
    lstPdfx <- lstPdf
    txts <- getTextFromEach lstPdfx
    let y = Prelude.map (`hasSearchTerm` searchTerm) txts
    pure y

getListOfPDFs :: IO [FilePath]
getListOfPDFs =
  do
    a <- getDirectoryContents currentDir
    let b = Prelude.filter (Data.Text.isSuffixOf (pack "pdf\"") . (pack . show)) a
    let x = (currentDir <>) <$> b
    pure x

currentDir :: FilePath
currentDir = "/home/ft/Documents/AnalisisProcto/"

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
