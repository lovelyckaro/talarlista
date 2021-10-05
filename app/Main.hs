{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Char (toLower, toUpper)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as SQ
import Data.Set (Set, insert, member)
import qualified Data.Set as S
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import Data.List.Extra (trim)

type Person = String
data Action = 
  RemoveTop
  | RemovePerson Person
  | Push
  | Pop
  | Reset
  | AddPerson Person
    deriving Show

data SpeakerList = SpeakerList {lista1 :: Seq Person, lista2 :: Seq Person, talat :: Set Person}
  deriving (Eq, Show)

type AmountSpoken = Int

data SpeakerLists = SpeakerLists {lists :: [SpeakerList], globalStats :: Map Person AmountSpoken}
  deriving (Show)

-- |'clearPerson' checks both lists
clearPerson :: SpeakerList -> SpeakerList
clearPerson (SpeakerList (x :<| xs) l2 set) = SpeakerList xs l2 (insert x set)
clearPerson (SpeakerList [] (x :<| xs) set) = SpeakerList [] xs (insert x set)
clearPerson (SpeakerList [] [] set) = SpeakerList [] [] set

removeName :: Person -> SpeakerList -> SpeakerList
removeName person (SpeakerList l1 l2 set) = SpeakerList (SQ.filter (/= person) l1) (SQ.filter (/= person) l2) set

suc :: Person -> Map Person AmountSpoken -> Map Person AmountSpoken
suc person = M.insertWith (+) person 1 

pushLists :: SpeakerLists -> SpeakerLists
pushLists (SpeakerLists lists gstats) = SpeakerLists (emptyList : lists) gstats

popLists :: SpeakerLists -> SpeakerLists
popLists (SpeakerLists [] gstats) = error "unreachable"
popLists (SpeakerLists [x] gstats) = SpeakerLists [emptyList] gstats
popLists (SpeakerLists (x:xs) gstats) = SpeakerLists xs gstats

removeTop :: SpeakerLists -> SpeakerLists
removeTop (SpeakerLists (list : rest) gstats) = SpeakerLists (clearPerson list : rest) gstats

removePerson :: Person -> SpeakerLists -> SpeakerLists
removePerson name (SpeakerLists (list : rest) gstats )= SpeakerLists (removeName name list : rest) gstats

addPerson :: Person -> SpeakerLists -> SpeakerLists
addPerson name (SpeakerLists (list : rest) gstats) 
  | name `member` talat list = SpeakerLists (list {lista2 = lista2 list |> name} : rest) (suc name gstats)
  | otherwise                = SpeakerLists (list {lista1 = lista1 list |> name} : rest) (suc name gstats)

handleAction :: Action -> SpeakerLists -> SpeakerLists
handleAction Push = pushLists 
handleAction Pop = popLists 
handleAction RemoveTop = removeTop
handleAction (RemovePerson name) = removePerson name
handleAction (AddPerson name) = addPerson name
handleAction Reset = const empty

emptyList :: SpeakerList
emptyList = SpeakerList [] [] S.empty

empty :: SpeakerLists
empty = SpeakerLists [emptyList] M.empty

-- Parsers go here
type Parser = Parsec Void String

pRemove, pRemovePerson, pPush, pPop, pReset, pAddPerson, pAction, pAddExplicit :: Parser Action
pRemove = do
  void (char' 'x') <|> void (string' "remove")
  return RemoveTop

pRemovePerson = do
  void (char' 'x') <|> void (string' "remove")
  space
  name <- some printChar
  return (RemovePerson name)

pPush = string' "push" >> return Push
pPop = string' "pop" >> return Pop
pReset = string' "reset" >> return Reset
pAddExplicit = do
  string' "add" 
  space 
  name <- some printChar
  return (AddPerson name)
pAddPerson = AddPerson <$> some printChar 

actionParsers :: [Parser Action]
actionParsers = [pRemovePerson, pRemove, pPush, pPop, pReset, pAddExplicit, pAddPerson]

pAction = foldr1 (<|>) (map try actionParsers)

-- Showers go here

nameCap :: String -> String
nameCap [] = []
nameCap (p:erson) = toUpper p : erson

printList :: SpeakerList -> String
printList SpeakerList {..} = unlines (toList (nameCap <$> (lista1 <> ["—————————————————————————"] <>  lista2)))

printListor :: SpeakerLists -> String
printListor (SpeakerLists [] _) = error "Något har gått riktigt fel"
printListor (SpeakerLists (x : xs) gstats) = "Talarlista " <> show (length xs) <> "\n" <> printList x

printHeader :: IO ()
printHeader = do
  clearScreen
  setCursorPosition 0 0
  putStrLn (unlines 
    ["Talarlista input:"
    ,"'x' or 'remove' to remove speaker"
    ,"'x NAME' or 'remove NAME' to remove specific speaker"
    ,"'NAME' or 'add NAME' to add person to list"
    ,"'push' to enter new list"
    ,"'pop' to throw away current list and go back"])
  hFlush stdout

sanitize :: String -> String
sanitize = map toLower . trim

loop :: SpeakerLists -> IO ()
loop l = do
  printHeader
  inp <- getLine
  let sanitized = sanitize inp
  let action = parse pAction "Input" sanitized
  n <- case action of
    Left _ -> do
      return l
    Right value -> do
      let new = handleAction value l
      writeFile "output" (printListor new)
      writeFile "gstats" (show $ globalStats new)
      return new
  loop n

main :: IO ()
main = loop empty
