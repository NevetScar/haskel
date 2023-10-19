module Main (main) where

import Pdf.Document
import Control.Monad
import Data.Text
import System.Directory
import qualified Data.Text.IO as Data.Text
import qualified Data.List as Prelude

main :: IO ()
main =
  do
    bp <- getMatchFrom getListOfPDFs
    let sorte = Prelude.sort bp
    let z = Prelude.map (Data.Text.stripPrefix (pack currentDir) . (Data.Text.takeWhile (/= '_') . pack . snd)) sorte
    print $ Prelude.length z
    mapM_ mayfil z


mayfil :: Maybe Text -> IO ()
mayfil Nothing = putStrLn "Nothing to remove"
mayfil (Just x)  = Data.Text.putStrLn x

getMatchFrom :: IO [FilePath] -> IO [(Bool, FilePath)]
getMatchFrom lstPdf = Prelude.filter fst <$> (Prelude.zip <$> filterEx lstPdf <*> lstPdf)


filterEx :: IO [FilePath] -> IO [Bool]
filterEx lstPdf =
  do
    lstPdfx <- lstPdf
    txts <- getTextFromEach lstPdfx
    let y = Prelude.map hasSearchTerm txts
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

hasSearchTerm :: Text -> Bool
hasSearchTerm txt = pack "interrumpido" `isInfixOf` toLower txt
