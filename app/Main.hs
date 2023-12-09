
module Main (main) where

import CaseBuilder (buildCaseFromString )
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    a <- getContents
    BS.putStr $ buildCaseFromString a
