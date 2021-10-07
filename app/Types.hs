module Types where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)

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
  | -- | Clear current list
    Clear
  deriving (Show)

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