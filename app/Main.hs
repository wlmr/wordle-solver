module Main where

import System.IO
import Control.Monad
import Lib


main :: IO ()
main = writeFile "words_5_alpha.txt" . unlines . filter (( == 5) . length) . words =<< readFile "words_alpha.txt"

