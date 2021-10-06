{-# LANGUAGE OverloadedLists #-}

-- |
-- Module      : Main
-- Description : Speaker list program for student division meetings in the
-- Computer Science and Engineering Division of the Chalmers Student Union.
-- Maintainer  : love.lyckaro@dtek.se
-- Stability   : experimental
-- Portability : POSIX
module Main where
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory
  ( createDirectoryIfMissing,
    getHomeDirectory,
  )
import System.IO (hFlush, stdout)
import Types
import Parsers
import PrettyPrinters
import Logic

-- | prints a pretty header
printHeader :: IO ()
printHeader = do
  clearScreen
  setCursorPosition 0 0
  putStrLn
    ( unlines
        [ "Talarlista input:",
          "'x' or 'remove' to remove speaker",
          "'x NAME' or 'remove NAME' to remove specific speaker",
          "'NAME' or 'add NAME' to add person to list",
          "'push' to enter new list",
          "'pop' to throw away current list and go back"
        ]
    )
  hFlush stdout

-- | Main loop
loop :: SpeakerLists -> FilePath -> IO ()
loop l homedir = do
  printHeader
  inp <- getLine
  let action = parseInput inp
  n <- case action of
    Left _ -> do
      return l
    Right value -> do
      let new = handleAction value l
      writeFile (homedir <> "/.talarlista/output") (printLists new)
      writeFile (homedir <> "/.talarlista/gstats") (printGStats new)
      return new
  loop n homedir

-- |  main = loop empty
main :: IO ()
main = do
  homedir <- getHomeDirectory
  createDirectoryIfMissing True (homedir <> "")
  writeFile (homedir <> "/.talarlista/output") ""
  writeFile (homedir <> "/.talarlista/gstats") ""
  loop empty homedir
