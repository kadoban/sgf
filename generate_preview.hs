module Main where

import SGF
import Game
import Canvas
import qualified Graphics.Rendering.Cairo as C

main = do c <- getContents
          case parseSGF c of
                Left e -> do putStrLn "Error parsing input:"
                             print e
                Right r -> do let g = (advanceWhile (\x -> (moveNum x) < 5) $ start (head r))
                              res <- printOn imgCanvas undefined g
                              C.surfaceWriteToPNG res "out_test.png"
                              js <- printOn jsCanvas "" g
                              print js
