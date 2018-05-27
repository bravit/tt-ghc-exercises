module Util where

import System.Environment ( getArgs )
import System.Directory (doesFileExist, getCurrentDirectory)
import System.IO
import System.FilePath ((</>))

parseArgs :: [String] -> (String, [String])
parseArgs args = case iter args "" [""] of
                   ("", _) -> error "filename is missing"
                   ret     -> ret
    where iter :: [String] -> String -> [String] -> (String, [String])
          iter [] filename searchpath = (filename, searchpath)
          iter ("-I":a:as) filename searchpath = iter as filename (a:searchpath)
          iter (a:as) filename searchpath
              | filename /= "" = error "Only one file allowed"
              | otherwise      = iter as a searchpath

findFile :: String -> [String] -> IO String
findFile filename [] = 
    error $ "Could not find " ++ filename ++ " in the search path"
findFile filename (path:searchpath) =
    do let fullpath = path </> filename
       exists <- doesFileExist fullpath
       if exists 
         then return fullpath 
         else findFile filename searchpath

-- Supports a single argument of the file to load, along with 
-- any number of pairs of the form "-I path".  Each such "path"
-- will be added to the searchpath
getContentsFromCmdLine :: IO String
getContentsFromCmdLine = do args <- getArgs
                            file <- (uncurry findFile) $ parseArgs args
                            readFile file

getFileContents :: String -> IO String
getFileContents filename = do dir <- getCurrentDirectory
                              let fullpath = dir </> filename
                              exists <- doesFileExist fullpath
                              if exists
                                then readFile fullpath
                                else error $ "Could not find " ++ filename
