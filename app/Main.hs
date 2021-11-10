module Main where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( unless )
import           Parser
import           System.Environment             ( getArgs )
import           System.IO


main :: IO ()
main = do
    args <- getArgs
    unless (null args) $ do
        handle   <- openFile ("examples/" ++ head args) ReadMode
        contents <- hGetContents handle
        case regularParse program contents of
            Left  pe  -> print pe
            Right des -> mapM_ print des
        hClose handle
