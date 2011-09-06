module Main where

import SGF
import Game
import Canvas
import qualified Graphics.Rendering.Cairo as C
import Network.CGI
import System.Directory
import System.FilePath
import Data.Maybe
import System.IO

sgfDirectory = "/var/www/usgo.org/drupal/sites/default/files/weekly_problem/"
inSgfDirectory fn =
  do c <- getDirectoryContents sgfDirectory
     return $ fn `elem` (filter (\x -> not $ x `elem` [".", ".."]) c)

sgfContents req =
  do let (sgfFn, ext) = splitExtension req
     isIn <- inSgfDirectory sgfFn
     if isIn
       then do h <- openFile (sgfDirectory ++ sgfFn) ReadMode
               c <- hGetContents h
               return $ Just (parseSGF c, ext)
       else return Nothing

                            -- for now, just advance to the first branch or end of game
startOfProblem collection = (advanceWhile (\x -> True) $ start (head collection))

cgiMain :: CGI CGIResult
cgiMain = output "Hello World!"

main = runCGI (handleErrors cgiMain)

--main = do c <- getContents
--          case parseSGF c of
--                Left e -> do putStrLn "Error parsing input:"
--                             print e
--                Right r -> do let g = (advanceWhile (\x -> (moveNum x) < 5) $ start (head r))
--                              res <- printOn imgCanvas undefined g
--                              C.surfaceWriteToPNG res "out_test.png"
--                              js <- printOn jsCanvas "" g
--                              print js
