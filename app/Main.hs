module Main where

import           Control.Applicative            ( (<|>) )
import           Lexer                          ( whiteSpace )
import           Parser
import           System.IO
import           Text.Parsec                    ( many )

main :: IO ()
main = do
    putStrLn ""
    f1
    putStrLn ""
    f2
  where
    f1 = do
        handle   <- openFile "examples/test1.ml" ReadMode
        contents <- hGetContents handle
        print $ regularParse (whiteSpace *> expr) contents
        hClose handle
    f2 = do
        handle   <- openFile "examples/test2.ml" ReadMode
        contents <- hGetContents handle
        case
                regularParse
                    (many $ whiteSpace *> typeDef <|> funDef <|> varDef)
                    contents
            of
                Left  pe  -> print pe
                Right des -> mapM_ print des
        hClose handle
