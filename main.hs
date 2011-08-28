module Main where

import SGF
import Game
import Canvas
import Graphics.GD

main = do c <- getContents
          case parseSGF c of
                Left e -> do putStrLn "Error parsing input:"
                             print e
                Right r -> do res <- printOn imgCanvas undefined (advanceWhile (\x -> (moveNum x) < 5) $ start (head r))
                              savePngFile "out.png" res
