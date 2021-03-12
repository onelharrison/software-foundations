module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.String.Utils
import           Lib                (SFBook, genSFBookPdf, mkSFBook)
import qualified System.Directory   as SysDir
import qualified System.Environment as SysEnv

usageMsg :: String
usageMsg =
  unlines
    [ "Usage: software-foundations [OPTIONS]... [BOOK]..."
    , ""
    , "Downloads and converts books in the Software Foundations series to PDFs"
    , ""
    , "BOOK codes"
    , "  lf  - Logical Foundations"
    , "  plf - Programming Language Foundations"
    , "  vfa - Verified Functional Algorithms"
    , "  qc  - QuickChick: Property-Based Testing in Coq"
    , "  vc  - Verifiable C"
    , ""
    , "OPTIONS"
    , "  -h, --help        Display this message"
    , ""
    , "Examples"
    , "  software-foundations lf plf vfa qc vc"
    , "  software-foundations qc lf"
    ]

createSFBookWorkingDir :: SFBook -> IO FilePath
createSFBookWorkingDir sfBook = do
  let dir = "working/" ++ show sfBook ++ "/"
  dirExists <- SysDir.doesDirectoryExist dir
  if dirExists
    then do
      SysDir.removeDirectoryRecursive dir
      SysDir.createDirectoryIfMissing True dir
    else SysDir.createDirectoryIfMissing True dir
  return dir

handleArgs :: [String] -> IO ()
handleArgs [] = putStrLn usageMsg
handleArgs ("--help":_) = putStrLn usageMsg
handleArgs ("-h":_) = putStrLn usageMsg
handleArgs sfBooks = do
  let sfBooks' :: [SFBook]
      sfBooks' = (catMaybes . filter isJust . map mkSFBook) sfBooks
  case sfBooks' of
    [] -> putStrLn usageMsg
    _ -> do
      putStrLn $ "Creating PDFs for: " ++ intercalate ", " (map show sfBooks')
      workingDirs <- mapM createSFBookWorkingDir sfBooks'
      mapM_ (uncurry genSFBookPdf) (zip workingDirs sfBooks')
      SysDir.removeDirectoryRecursive "working/"

main :: IO ()
main = SysEnv.getArgs >>= handleArgs
