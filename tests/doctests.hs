module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "-isrc"
               , "src/Data/LCA/Online.hs"
               , "src/Data/LCA/Online/Monoidal.hs"
               ]
