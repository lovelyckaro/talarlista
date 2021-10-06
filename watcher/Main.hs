module Main where

import System.Process ( callCommand )

main :: IO ()
main = callCommand "watch -d -n 0.5 -t cat ~/.talarlista/output"