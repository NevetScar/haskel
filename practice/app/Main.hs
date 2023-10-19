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
    let z = Prelude.map (Data.Text.stripPrefix (pack currentDir) . (Data.Text.takeWhile (/= '_') . pack . snd)) x
    print $ Prelude.length z
    mapM_ mayfil z


mayfil :: Maybe Text -> IO ()
mayfil Nothing = putStrLn "Nothing to remove"
mayfil (Just x)  = Data.Text.putStrLn x

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
    countx <- pageNodeNKids rootNode
    let sizei = [0..(countx - 1)]
    Data.Text.concat <$> mapM (pageExtractText <=< pageNodePageByNum rootNode) sizei

hasExpulsado :: Text -> Bool
hasExpulsado txt = pack "interrumpido" `isInfixOf` toLower txt
