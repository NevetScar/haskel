module Main where

import Pdf.Document
import Control.Monad
import Data.Text
import System.Directory
import qualified Data.Text.IO as Data.Text

main :: IO ()
main =
  do
    bp <- getExpulsados
    let x = Prelude.filter fst bp
    let z = Prelude.map snd x
    print $ Prelude.length z
    mapM_ (Data.Text.putStrLn . pack) z


getExpulsados :: IO [(Bool, FilePath)]
getExpulsados = Prelude.filter fst <$> zipp

zipp :: IO [(Bool, FilePath)]
zipp = Prelude.zip <$> filterEx <*> getListOfPDFs


filterEx :: IO [Bool]
filterEx =
  do
    lstPdf <- getListOfPDFs
    txts <- getTextFromEach lstPdf
    let y = Prelude.map hasExpulsado txts
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
    page <- pageNodePageByNum rootNode 0
    pageExtractText page

hasExpulsado :: Text -> Bool
hasExpulsado txt = pack "expulsado" `isInfixOf` toLower txt