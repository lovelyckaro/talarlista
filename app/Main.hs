{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Main
Description : Speaker list program for student division meetings in the
Computer Science and Engineering Division of the Chalmers Student Union.
Maintainer  : love.lyckaro@dtek.se
Stability   : experimental
Portability : POSIX
-}
module Main where

import Control.Monad (void)
import Data.Char (toLower, toUpper)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.List.Extra (trim)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq ((:<|)), (|>))
import qualified Data.Sequence as SQ
import Data.Set (Set, insert, member)
import qualified Data.Set as S
import Data.Void (Void)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    parse,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char', printChar, space, string')
import Text.Read (readMaybe)

type Person = String

-- | The 'Action' type describes the supported actions
data Action
  = -- | Remove the current speaker from the list
    RemoveTop
  | -- | Remove a specific speaker from the list
    RemovePerson Person
  | -- | Move to a new speaker list, for example for a point of order.
    Push
  | -- | Discard the current list and move back to the previous list
    Pop
  | -- | ResetAll all lists and global stats
    ResetAll
  | -- | Add a new person to the list
    AddPerson Person
  deriving (Show)

-- | A 'SpeakerList' is really two lists of people and a set of people that have
--  spoken. When adding a new person, check whether they've already spoken (using
--  the set) and if so add to seconday list. Resulting in first-time-speakers
--  getting priority
data SpeakerList = SpeakerList
  { -- | list of first time speakers
    lista1 :: Seq Person,
    -- | list of second or more time speakers
    lista2 :: Seq Person,
    -- | Set of speakers so far
    talat :: Set Person
  }
  deriving (Eq, Show)

type AmountSpoken = Int

-- | 'SpeakersLists' is a collection of 'SpeakerList' as well as a counter for
--  how much each person has spoken.
data SpeakerLists = SpeakerLists
  { -- | Stack of speakerlists
    lists :: [SpeakerList],
    -- | 'Map' from speaker to the amount of times they've spoken.
    globalStats :: Map Person AmountSpoken
  }
  deriving (Show)

-- | 'clearNext' checks both lists and removes the person next in line to
-- speak.
clearNext :: SpeakerList -> SpeakerList
clearNext (SpeakerList (x :<| xs) l2 set) = SpeakerList xs l2 (insert x set)
clearNext (SpeakerList [] (x :<| xs) set) = SpeakerList [] xs (insert x set)
clearNext (SpeakerList [] [] set) = SpeakerList [] [] set

-- | 'removeName' checks both lists and removes all occurences of a speaker.
removeName :: Person -> SpeakerList -> SpeakerList
removeName person (SpeakerList l1 l2 set) =
  SpeakerList (SQ.filter (/= person) l1) (SQ.filter (/= person) l2) set

-- | 'suc' increases a persons amount spoken by one.
suc :: Person -> Map Person AmountSpoken -> Map Person AmountSpoken
suc person = M.insertWith (+) person 1

-- | 'pushLists' switches to a new empty 'SpeakerList'
pushLists :: SpeakerLists -> SpeakerLists
pushLists (SpeakerLists lists gstats) =
  SpeakerLists (emptyList : lists) gstats

-- |  'popLists' discards the current speaker lists and jumps back to the
--  previous one.
popLists :: SpeakerLists -> SpeakerLists
popLists (SpeakerLists [] gstats) = error "unreachable"
popLists (SpeakerLists [x] gstats) = SpeakerLists [emptyList] gstats
popLists (SpeakerLists (x : xs) gstats) = SpeakerLists xs gstats

-- | Remove top speaker from current active list.
removeTop :: SpeakerLists -> SpeakerLists
removeTop (SpeakerLists (list : rest) gstats) =
  SpeakerLists (clearNext list : rest) gstats

-- | Remove specific person from current active list.
removePerson :: Person -> SpeakerLists -> SpeakerLists
removePerson name (SpeakerLists (list : rest) gstats) =
  SpeakerLists (removeName name list : rest) gstats

-- | Add person to current active list.
addPerson :: Person -> SpeakerLists -> SpeakerLists
addPerson name (SpeakerLists (list : rest) gstats)
  | name `member` talat list = SpeakerLists (list {lista2 = lista2 list |> name} : rest) (suc name gstats)
  | otherwise = SpeakerLists (list {lista1 = lista1 list |> name} : rest) (suc name gstats)

-- |  'handleAction' takes an action and modifies the 'SpeakerLists' accordingly
handleAction :: Action -> SpeakerLists -> SpeakerLists
handleAction Push = pushLists
handleAction Pop = popLists
handleAction RemoveTop = removeTop
handleAction (RemovePerson name) = removePerson name
handleAction (AddPerson name) = addPerson name
handleAction ResetAll = const empty

-- | Default, empty 'SpeakerList'
emptyList :: SpeakerList
emptyList = SpeakerList [] [] S.empty

-- | Default, empty 'SpeakerLists'
empty :: SpeakerLists
empty = SpeakerLists [emptyList] M.empty

-- Parsers go here
type Parser = Parsec Void String

-- * Parsers
-- | Here be some parsers of Actions

pRemove, pRemovePerson, pPush, pPop, pReset, pAddPerson, pAction, pAddExplicit :: Parser Action

-- |Parses 'RemoveTop' 'Action's
pRemove = do
  void (char' 'x') <|> void (string' "remove")
  return RemoveTop
-- |Parses 'RemovePerson' 'Action's

pRemovePerson = do
  void (char' 'x') <|> void (string' "remove")
  space
  name <- some printChar
  return (RemovePerson name)

-- |Parses 'Push' 'Action's
pPush = string' "push" >> return Push

-- |Parses 'Pop' 'Action's
pPop = string' "pop" >> return Pop

-- |Parses 'ResetAll' 'Action's
pReset = string' "reset" >> return ResetAll

-- |Parses 'AddPerson' 'Action's with explicit add
pAddExplicit = do
  string' "add"
  space
  name <- some printChar
  return (AddPerson name)

-- |Parses 'AddPerson' 'Action's without explicit add
pAddPerson = AddPerson <$> some printChar

-- |List of action parsers
actionParsers :: [Parser Action]
actionParsers = [pRemovePerson, pRemove, pPush, pPop, pReset, pAddExplicit, pAddPerson]

-- | Parser for any action
pAction = foldr1 (<|>) (map try actionParsers)

-- |trim and lowercase a string (done before parsing)
sanitize :: String -> String
sanitize = map toLower . trim

-- * Pretty printers and IO
-- | here be some pretty printers and IO shit.

-- | Capitalize first letter in string
nameCap :: String -> String
nameCap [] = []
nameCap (p : erson) = toUpper p : erson

-- | Pretty format for a 'SpeakerList'
printList :: SpeakerList -> String
printList SpeakerList {..} = unlines (toList (nameCap <$> (lista1 <> ["—————————————————————————"] <> lista2)))

-- | Pretty format for 'SpeakerLists'
printListor :: SpeakerLists -> String
printListor (SpeakerLists [] _) = error "Något har gått riktigt fel"
printListor (SpeakerLists (x : xs) gstats) = "Talarlista " <> show (length xs) <> "\n" <> printList x

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

-- | main = loop empty
main :: IO ()
main = loop empty
