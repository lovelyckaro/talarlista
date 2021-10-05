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

data TalarLista = TalarLista {lista1 :: Seq Person, lista2 :: Seq Person, talat :: Set Person}
  deriving (Eq, Show)

type AmountSpoken = Int

data TalarListor = TalarListor {lists :: [TalarLista], globalStats :: Map Person AmountSpoken}
  deriving (Show)

clearPerson :: TalarLista -> TalarLista
clearPerson (TalarLista (x :<| xs) l2 set) = TalarLista xs l2 (insert x set)
clearPerson (TalarLista [] (x :<| xs) set) = TalarLista [] xs (insert x set)
clearPerson (TalarLista [] [] set) = TalarLista [] [] set

removeName :: Person -> TalarLista -> TalarLista
removeName person (TalarLista l1 l2 set) = TalarLista (SQ.filter (/= person) l1) (SQ.filter (/= person) l2) set

suc :: Person -> Map Person AmountSpoken -> Map Person AmountSpoken
suc person = M.insertWith (+) person 1 

pushLists :: TalarListor -> TalarListor
pushLists (TalarListor lists gstats) = TalarListor (emptyList : lists) gstats

popLists :: TalarListor -> TalarListor
popLists (TalarListor [] gstats) = error "unreachable"
popLists (TalarListor [x] gstats) = TalarListor [emptyList] gstats
popLists (TalarListor (x:xs) gstats) = TalarListor xs gstats

removeTop :: TalarListor -> TalarListor
removeTop (TalarListor (list : rest) gstats) = TalarListor (clearPerson list : rest) gstats

removePerson :: Person -> TalarListor -> TalarListor
removePerson name (TalarListor (list : rest) gstats )= TalarListor (removeName name list : rest) gstats

addPerson :: Person -> TalarListor -> TalarListor
addPerson name (TalarListor (list : rest) gstats) 
  | name `member` talat list = TalarListor (list {lista2 = lista2 list |> name} : rest) (suc name gstats)
  | otherwise                = TalarListor (list {lista1 = lista1 list |> name} : rest) (suc name gstats)

handleAction :: Action -> TalarListor -> TalarListor
handleAction Push = pushLists 
handleAction Pop = popLists 
handleAction RemoveTop = removeTop
handleAction (RemovePerson name) = removePerson name
handleAction (AddPerson name) = addPerson name
handleAction Reset = const empty

emptyList :: TalarLista
emptyList = TalarLista [] [] S.empty

empty :: TalarListor
empty = TalarListor [emptyList] M.empty

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

printList :: TalarLista -> String
printList TalarLista {..} = unlines (toList (nameCap <$> (lista1 <> ["—————————————————————————"] <>  lista2)))

printListor :: TalarListor -> String
printListor (TalarListor [] _) = error "Något har gått riktigt fel"
printListor (TalarListor (x : xs) gstats) = "Talarlista " <> show (length xs) <> "\n" <> printList x

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

loop :: TalarListor -> IO ()
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
