module Main ( main ) where

import Control.Monad.Trans
import Data.Maybe
import System.Directory
import System.IO
import System.Time

import Utils

data Directory = Directory [(String,Directory)] [(String,String)] deriving Show

main :: IO ()
main = do
  d <- captureDirectory
  t <- getClockTime
  writeFile "TextData.hs" $ unlines
    [ "module  TextData ( Directory (..) , srcDir , writeDir , buildDate ) where"
    , ""
    , "import System.Directory"
    , "import System.IO"
    , ""
    , "buildDate :: String"
    , "buildDate = \"" ++ show t ++ "\""
    , ""
    , "data Directory = Directory [(FilePath,Directory)] [(FilePath,String)] deriving Show"
    , ""
    , "srcDir :: Directory"
    , "srcDir = " ++ show d
    , ""
    , "writeDir :: Directory -> IO ()"
    , "writeDir (Directory subs files) = do"
    , "  mapM_ (\\ (n,c) -> writeFile n c) files"
    , "  mapM_ writeDir' subs"
    , "  where"
    , "  writeDir' :: (FilePath,Directory) -> IO ()"
    , "  writeDir' (name,dir) = do"
    , "    home <- getCurrentDirectory"
    , "    createDirectoryIfMissing True name"
    , "    setCurrentDirectory name"
    , "    writeDir dir"
    , "    setCurrentDirectory home"
    , ""
    ]

makeTextData :: [(String,FilePath)] -> IO ()
makeTextData files = do
  let (names,paths) = unzip files
  texts <- mapM readFile paths
  writeFile ("TextData.hs") ("module  TextData ( allTextData" ++ concatMap (", " ++ ) names ++ " ) where \n" ++ concatMap dumpText (zip names texts) ++ dumpAll (zip names paths))

  where

  dumpText :: (String,String) -> String
  dumpText (name,text) = name ++ " = " ++ show text ++ "\n"

  dumpAll :: [(String,String)] -> String
  dumpAll stuff = "allTextData = [" ++ tail (concatMap (\ (name,path) -> "," ++ "(\"" ++ path ++ "\"," ++ name ++ ")") stuff) ++ "]\n"

captureDirectory :: IO Directory
captureDirectory = do
  contents <- getDirectoryContents "."
  subs  <- mapMaybeIO captureDirectory' contents
  files <- mapMaybeIO captureFile contents
  return (Directory subs files)

captureDirectory' :: FilePath -> IO (Maybe (FilePath,Directory))
captureDirectory' name | name == "CVS" || head name == '.' = return Nothing
captureDirectory' name | head name /= '.' = do
  isDir <- doesDirectoryExist name
  if not isDir
    then return Nothing
    else do
      home <- getCurrentDirectory
      setCurrentDirectory name 
      d <- captureDirectory
      setCurrentDirectory home
      return (Just (name,d))

mapMaybeIO :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeIO _ [] = return []
mapMaybeIO f (a:b) = do
  b' <- mapMaybeIO f b
  t  <- f a
  case t of
    Nothing -> return b'
    Just a' -> return (a':b')

captureFile :: FilePath -> IO (Maybe (FilePath,String))
captureFile name | length name < 4 || not (takeTail 3 name == ".hs" || takeTail 4 name == ".txt")  = return Nothing
captureFile name = do
  c <- readFileNow name
  return (Just (name,c))

