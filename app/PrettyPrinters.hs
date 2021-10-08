{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module PrettyPrinters where

import Data.Char (toUpper)
import Data.Foldable (toList)
import Types
import Data.Bifoldable

-- | Capitalize first letter in string
nameCap :: String -> String
nameCap [] = []
nameCap (p : erson) = toUpper p : erson

-- | Pretty format for a 'SpeakerList'
printList :: SpeakerList -> String
printList SpeakerList {..} = unlines (toList (nameCap <$> (lista1 <> ["────────────────────────────────────"] <> lista2)))

-- | Pretty format for 'SpeakerLists'
printLists :: SpeakerLists -> String
printLists (SpeakerLists [] _) = error "Något har gått riktigt fel"
printLists (SpeakerLists (x : xs) gstats) = "Talarlista " <> 
                                            show (length xs) <> " " <> listEmoji (length xs) <> "\n" <> 
                                            "Använd gärna tal-lapparna 😁" <> "\n" <> 
                                            printList x

-- | Pretty format for 'globalStats', written in a horribly point-free form
printGStats :: SpeakerLists -> String
printGStats = bifoldMap (<> ": ") ((<> "\n") . show) . globalStats

-- | Associates an emoji depending on the level of meta discussions
listEmoji :: Int -> String
listEmoji 0 = "😊"
listEmoji 1 = "😮"
listEmoji 2 = "😬"
listEmoji 3 = "😡"
listEmoji l = "📮" 