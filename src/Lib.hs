{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( mkSFBook
  , genSFBookPdf
  , SFBook(..)
  ) where

import           Control.Monad
import qualified Data.Char         as C
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Command
import           Text.HTML.Scalpel

data SFBook = LF
    | PLF
    | VFA
    | QC
    | VC
    | SLF

instance Show SFBook where
  show LF  = "lf"
  show PLF = "plf"
  show VFA = "vfa"
  show QC  = "qc"
  show VC  = "vc"
  show SLF = "slf"

mkSFBook :: String -> Maybe SFBook
mkSFBook str =
  case map C.toLower str of
    "lf"  -> Just LF
    "plf" -> Just PLF
    "vfa" -> Just VFA
    "qc"  -> Just QC
    "vc"  -> Just VC
    "slf" -> Just SLF
    _     -> Nothing

sfShowLong :: SFBook -> String
sfShowLong LF  = "LogicalFoundations"
sfShowLong PLF = "ProgrammingLanguageFoundations"
sfShowLong VFA = "VerifiedFuncationalAlgorithms"
sfShowLong QC  = "QuickChickPropertyBasedTestingInCoq"
sfShowLong VC  = "VerifiableC"
sfShowLong SLF = "SeparationLogicFoundations"

sfBookRootURL :: SFBook -> URL
sfBookRootURL sfBook =
  concat
    ["https://softwarefoundations.cis.upenn.edu/", show sfBook, "-current/"]

downloadSFBookArchive :: FilePath -> SFBook -> IO FilePath
downloadSFBookArchive destDir sfBook = do
  cmd downloadCommand :: IO ()
  return (destDir ++ archiveFileName)
  where
    archiveFileName :: FilePath
    archiveFileName = show sfBook ++ ".tgz"
    archiveURL :: URL
    archiveURL = sfBookRootURL sfBook ++ archiveFileName
    downloadCommand :: String
    downloadCommand = concat ["wget -P ", destDir, " ", archiveURL]

extractSFBookArchive :: FilePath -> FilePath -> SFBook -> IO FilePath
extractSFBookArchive workingDir archiveFilePath sfBook = do
  cmd extractCommand :: IO ()
  return extractedFilesDir
  where
    extractCommand :: String
    extractCommand = unwords ["tar", "zxf", archiveFilePath, "-C", workingDir]
    extractedFilesDir :: FilePath
    extractedFilesDir = concat [workingDir, show sfBook, "/"]

scrapeSFBookTocPages :: FilePath -> IO [FilePath]
scrapeSFBookTocPages workingDir = do
  tocHtml <- readFile tocHtmlFile
  let tocPages :: Maybe [FilePath]
      tocPages = scrapeStringLike tocHtml tocPagesScraper
  guard (isJust tocPages)
  return (fromJust tocPages)
  where
    tocHtmlFile :: FilePath
    tocHtmlFile = workingDir ++ "toc.html"
    tocPagesScraper :: Scraper String [FilePath]
    tocPagesScraper = chroots "h2" (attr "href" "a")

pdfExt :: String
pdfExt = ".pdf"

convertHtmlToPdf :: FilePath -> IO ()
convertHtmlToPdf htmlFile = cmd convertHtmlToPdfCommand :: IO ()
  where
    convertHtmlToPdfCommand :: String
    convertHtmlToPdfCommand =
      unwords
        [ "wkhtmltopdf"
        , "--enable-local-file-access"
        , htmlFile
        , (head . splitOn ".") htmlFile ++ pdfExt
        ]

combinePdfs :: [FilePath] -> FilePath -> IO ()
combinePdfs pdfFiles outPdfFile = cmd combinePdfsCommand :: IO ()
  where
    combinePdfsCommand :: String
    combinePdfsCommand =
      unwords
        [ "pdfjam"
        , "--fitpaper=false"
        , "--rotateoversize=false"
        , unwords pdfFiles
        , "-o"
        , outPdfFile
        ]

genSFBookPdf :: FilePath -> SFBook -> IO ()
genSFBookPdf workingDir sfBook = do
  localArchiveFilePath <- downloadSFBookArchive workingDir sfBook
  extractedFilesDir <-
    extractSFBookArchive workingDir localArchiveFilePath sfBook
  tocPages <- scrapeSFBookTocPages extractedFilesDir
  let htmlFiles :: [FilePath]
      htmlFiles =
        map (extractedFilesDir ++) ("index.html" : "toc.html" : tocPages)
  mapM_ convertHtmlToPdf htmlFiles
  let pdfFiles :: [FilePath]
      pdfFiles = map ((++ pdfExt) . head . splitOn ".") htmlFiles
  combinePdfs pdfFiles (sfShowLong sfBook ++ pdfExt)
