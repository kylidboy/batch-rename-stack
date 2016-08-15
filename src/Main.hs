{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.FilePath
import Control.Exception
import qualified Data.Map.Lazy as DM
-- import qualified Date.ByteString.Lazy as BL
import Control.Monad (mapM_)


data MyArgs = MyArgs
  { dir :: FilePath
  , ref :: FilePath }

margs :: Parser MyArgs
margs = MyArgs
  <$> strOption
  (
    long "directory"
    <> short 'd'
    <> metavar "DIR"
    <> help "Directory that contains the downloaded files"
  )
  <*> strOption
  (
    long "reference"
    <> short 'r'
    <> metavar "REF"
    <> help "Reference file that contains the original names and new names"
  )

renameDownloads :: MyArgs -> IO ()
renameDownloads MyArgs {..} = do
  putStrLn $ "Rename maps: " ++ ref
  putStrLn $ "Source directory: " ++ dir
  r         <- getRenameRef ref
  fnames    <- getDirectoryContents dir
  let rnMap = DM.fromList r
  rename dir rnMap fnames
  putStrLn "Finished"

rename :: FilePath -> DM.Map FilePath String -> [FilePath] -> IO ()
rename dir renameMap = mapM_ rename'
  where rename' filename = case DM.lookup filename renameMap of
                             Nothing -> return ()
                             Just val -> renameFile (dir </> filename) (dir </> val)

getRenameRef :: FilePath -> IO [(FilePath, String)]
getRenameRef fpath = do
  h <- openFile fpath ReadMode
  hSetEncoding h utf8
  contents <- hGetContents h
  parse contents
  where
    parse = return . alist . parse'
    parse' = map words . lines
    alist = convert2alist . filterValidPath
    filterValidPath = filter (\inp -> case inp of
                                     (x:xs) -> True
                                     _ -> False
                                 )
    convert2alist = map (\(x:xs) -> (x, unwords xs))

main :: IO ()
main = execParser opts >>= renameDownloads
  where opts = info (helper <*> margs)
               (
                 fullDesc
                 <> progDesc "Batch rename the audio files downloaded from 喜马拉雅听书"
               )
