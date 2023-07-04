{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where 

import qualified BasicServer as B

main :: IO ()
main = B.runServer