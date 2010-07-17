-- | Common utilities not found in the Haskell standard library.

module Language.TRS.Utils
  ( readFileNow
  , safeSetCurrentDirectory
  , splitPath
  , split
  , joinPath
  , join
  , relativePath
  , commonPrefixPath
  , debug
  , debug2
  , point
  , hash
  , hex
  , bin
  , binToHex
  , takeTail
  , dropTail
  , dropTake
  , replace
  , enum
  , power
  , bitsRequired
  , fold
  , UId   (..)
  , hGetStr
  , formatXML
  , unformatXML
  ) where

import Control.Monad (replicateM)
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import System.Directory
import System.IO
import System.IO.Unsafe

-- | Reads a file immediately.
readFileNow :: FilePath -> IO String
readFileNow file = do
  s <- readFile file
  if length s == 0 then return s else return s

-- | Attempts to set the currenet directory.  Posts a clear message if it fails.
safeSetCurrentDirectory :: FilePath -> IO ()
safeSetCurrentDirectory dir = do
  isDir <- doesDirectoryExist dir
  if isDir then setCurrentDirectory dir else do
    c <- getCurrentDirectory
    error ("Directory does not exist:  " ++ dir ++ "  from  " ++ c)

-- | Splits a string with delimiter chars as string.
split :: String -> String -> [String]
split "" _        = []
split (c:s) delimiters | elem c delimiters   = split s delimiters
split s     delimiters                       = s0 : split s1 delimiters where (s0, s1) = span (flip notElem delimiters) s

-- | Splits a 'FilePath' into a list of directories and a file.
splitPath :: FilePath -> [String]
splitPath ""          = []
splitPath ('/':path)  = splitPath path
splitPath path        = let (name, path') = span ('/' /=) path in
                        name : splitPath path'

-- | Joins a path.
joinPath :: [String] -> FilePath
joinPath []   = error "Empty path."
joinPath path = tail (concatMap ("/" ++) path)

-- | Joins a list of strings with a delimiter string.
join :: [String] -> String -> String
join [] _ = ""
join [a] _ = a
join (a:b) d = a ++ d ++ join b d

-- | Greatest common path prefix from a list of paths.
commonPrefixPath :: [FilePath] -> FilePath
commonPrefixPath paths = joinPath (foldl1 common (map splitPath paths))
  where
  common :: Eq a => [a] -> [a] -> [a]
  common (a1:a) (b1:b) | a1 == b1 = a1 : common a b
  common _ _ = []

-- | Relative path from one path to another.
relativePath :: FilePath -> FilePath -> FilePath
relativePath fromPath toPath = if null path then "." else joinPath path
  where
  commonPrefix = length (splitPath (commonPrefixPath [fromPath,toPath]))
  up = replicate (length (splitPath fromPath) - commonPrefix) ".."
  down = drop commonPrefix (splitPath toPath)
  path = up ++ down







-- | Debugging only.
debug :: Show a => String -> a -> a
debug note value = unsafePerformIO (debug' note value)
  where
  debug' note value = do
    putStrLn ("DEBUG: " ++ note ++ " : " ++ show value)
    return value

-- | Debugging only.
debug2 :: (Show a, Show b) => String -> a -> b -> b
debug2 note v1 v2 = unsafePerformIO $ do
    putStrLn ("DEBUG: " ++ note ++ " : " ++ show v1)
    return v2

-- | Break point.
point :: String -> a -> a
point note value = unsafePerformIO (point' note value)
  where
  point' note value = do
    putStrLn ("POINT: " ++ note)
    return value

-- | Generates a puedo random hash.
hash :: String -> String
hash s = hex 16 (foldl hash' 0 s)
  where
  hash' :: Word64 -> Char -> Word64
  hash' misr c = rotateR (xor misr (fromIntegral (ord c))) 1  --XXX Might need to strengthen hash function.

-- | Takes from the end of the list.
takeTail :: Int -> [a] -> [a]
takeTail i l = reverse (take i (reverse l))

-- | Drops from the end of the list
dropTail :: Int -> [a] -> [a]
dropTail i l = reverse (drop i (reverse l))

-- | Drops then takes from a list.
dropTake :: Int -> Int -> [a] -> [a]
dropTake d t l = take t $ drop d l

-- | Simple search and replace.
replace :: String -> String -> String -> String
replace _ _ "" = ""
replace old new s@(a:b) = if isPrefixOf old s then new ++ replace old new (drop (length old) s) else a : replace old new b

-- | Converts an integral into a bin string.
bin :: Integral a => Int -> a -> String
bin n _ | n <= 0 = ""
bin n i = bin (n - 1) (i `div` 2) ++ (if i `mod` 2 == 0 then "0" else "1")

-- | Converts an integral into a hex string.
hex :: Integral a => Int -> a -> String
hex n _ | n <= 0 = ""
hex n i = hex (n - 1) (i `div` 16) ++ nib i
  where
  nib :: Integral a => a -> String
  nib i = case i `mod` 16 of
                    0  -> "0"
                    1  -> "1"
                    2  -> "2"
                    3  -> "3"
                    4  -> "4"
                    5  -> "5"
                    6  -> "6"
                    7  -> "7"
                    8  -> "8"
                    9  -> "9"
                    10 -> "A"
                    11 -> "B"
                    12 -> "C"
                    13 -> "D"
                    14 -> "E"
                    15 -> "F"
                    _  -> error "ERROR  Int is not a valid hex nibble."

-- | Converts a bin string to hex.
binToHex :: String -> String
binToHex b = nib $ reverse b
  where

  nib :: String -> String
  nib "" = ""
  nib (b0:b1:b2:b3:bin) = nib bin ++ nib' [b3,b2,b1,b0]
  nib bin = nib (bin ++ "0")

  nib' :: String -> String
  nib' bin = case bin of
               "0000" -> "0"
               "0001" -> "1"
               "0010" -> "2"
               "0011" -> "3"
               "0100" -> "4"
               "0101" -> "5"
               "0110" -> "6"
               "0111" -> "7"
               "1000" -> "8"
               "1001" -> "9"
               "1010" -> "A"
               "1011" -> "B"
               "1100" -> "C"
               "1101" -> "D"
               "1110" -> "E"
               "1111" -> "F"
               _      -> error "String is not binary."

-- | Enumerates a list.
enum :: [a] -> [(Int,a)]
enum a = zip [0 .. length a - 1] a

-- | Integral power.
power :: Integral a => a -> a -> a
power _ m | m < 0   = error "Power must be positive."
power _ m | m == 0  = 1
power n m           = n * power n (m - 1)

-- | Required bits to represent a enumerate a number.
bitsRequired :: Int -> Int
bitsRequired 0 = 0
bitsRequired n = check 1 2
  where
  check i j = if j >= n then i else check (i + 1) (j * 2)

-- | Foldl swaped.
fold :: (a -> b -> b) -> b -> [a] -> b
fold f = foldl (flip f)

-- | Class for data that has unique Int identifiers.
class UId a where
  uid :: a -> Int
  uniqueName :: a -> String
  uniqueName a = "n" ++ show (uid a)

-- | Read a 'String' of given length.
hGetStr :: Handle -> Int -> IO String
hGetStr _ n | n <= 0 = return ""
hGetStr h n          = replicateM n $ hGetChar h

-- | Formats a string to XML or HTML.
formatXML :: String -> String
formatXML "" = ""
formatXML (c:s) = case c of
  '"'  -> "&quot;" ++ formatXML s
  '\'' -> "&apos;" ++ formatXML s
  '&'  -> "&amp;"  ++ formatXML s
  '<'  -> "&lt;"   ++ formatXML s
  '>'  -> "&gt;"   ++ formatXML s
  _    ->         c : formatXML s

-- | Unformats an XML string to text.
unformatXML :: String -> String
unformatXML s = case s of
  "" -> ""
  '&':'q':'u':'o':'t':';':s' -> '"'  : unformatXML s'
  '&':'a':'p':'o':'s':';':s' -> '\'' : unformatXML s'
  '&':'a':'m':'p':';':s'     -> '&'  : unformatXML s'
  '&':'l':'t':';':s'         -> '<'  : unformatXML s'
  '&':'g':'t':';':s'         -> '>'  : unformatXML s'
  c:s'                       -> c    : unformatXML s'

