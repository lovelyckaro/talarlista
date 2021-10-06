{-# LANGUAGE OverloadedLists #-}
module Logic where

import Types
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as SQ
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set, member, insert)
import qualified Data.Set as S

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
popLists (SpeakerLists [x] gstats) = SpeakerLists [x] gstats
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
