module Main where

import SGF
import Game
import Canvas

main = do c <- getContents
          case parseSGF c of
                Left e -> do putStrLn "Error parsing input:"
                             print e
                Right r -> do res <- printOn stCanvas "" "a.sgf" (advanceWhile (\x -> (moveNum x) < 5) $ start (head r))
                              print res
