{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

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

type Person = String

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

handleInput :: String -> TalarListor -> TalarListor
handleInput "push" (TalarListor lists gstats) = TalarListor (emptyList : lists) gstats
handleInput "pop" (TalarListor [] gstats) = TalarListor [emptyList] gstats
handleInput "pop" (TalarListor [x] gstats) = TalarListor [emptyList] gstats
handleInput "pop" (TalarListor (list : rest) gstats) = TalarListor rest gstats
handleInput "x" (TalarListor (list : rest) gstats) = TalarListor (clearPerson list : rest) gstats
handleInput "clear" ls = handleInput "x" ls
handleInput ('x' : ' ' : name) (TalarListor (list : rest) gstats) = TalarListor (removeName name list : rest) gstats
handleInput "n" _ = empty
handleInput "new" l = handleInput "n" l
handleInput person (TalarListor (list : rest) gstats)
  | person `member` talat list = TalarListor (list {lista2 = lista2 list |> person} : rest) (suc person gstats)
  | otherwise =  TalarListor (list {lista1 = lista1 list |> person} : rest) (suc person gstats)

emptyList :: TalarLista
emptyList = TalarLista [] [] S.empty

empty :: TalarListor
empty = TalarListor [emptyList] M.empty

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
  putStrLn "Talarlista input"
  hFlush stdout

sanitize :: String -> String
sanitize = map toLower

loop :: TalarListor -> IO ()
loop l = do
  printHeader
  inp <- getLine
  let sanitized = sanitize inp
  let new = handleInput sanitized l
  writeFile "output" (printListor new)
  writeFile "gstats" (show $ globalStats new)
  loop new

main :: IO ()
main = loop empty
