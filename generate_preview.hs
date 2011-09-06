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
import qualified Data.ByteString.Lazy as BS

sgfDirectory = "/var/www/usgo.org/drupal/sites/default/files/weekly_problem/"
inDirectory dir fn =
  do c <- getDirectoryContents dir
     return $ fn `elem` (filter (\x -> not $ x `elem` [".", ".."]) c)

sgfContents req =
  do let (sgfFn, ext) = splitExtension req
     isIn <- inDirectory sgfDirectory sgfFn
     if isIn
       then do h <- openFile (sgfDirectory ++ sgfFn) ReadMode
               c <- hGetContents h
               return $ Just (parseSGF c, (sgfFn, ext))
       else return Nothing

                            -- for now, just advance to the first branch or end of game
startOfProblem collection = (advanceWhile (\x -> True) $ start (head collection))

generateOutputs req =
  do sgfC <- liftIO $ sgfContents req
     case sgfC of
       Nothing -> outputNotFound $ "Source SGF file of " ++ req
       Just (parsed, (sgfFn, ext)) ->
         case parsed of
           Left e -> outputError 500 "Can't process that SGF file" ["Cant' process SGF file", req, show e]
           Right parsed ->
             do let probStart = startOfProblem parsed
                img <- liftIO $ printOn imgCanvas undefined probStart
                liftIO $ C.surfaceWriteToPNG img (sgfFn ++ ".png")
                toplay <- liftIO $ printOn toplayCanvas undefined probStart
                liftIO $ writeFile (sgfFn ++ ".toplay") toplay
                outputRequestedFile sgfFn ext

outputRequestedFile sgfFn ext =
  do case ext of
      ".png" -> do setHeader "Content-type" "image/png"
                   outputIt
      ".toplay" -> do setHeader "Content-type" "text/plain"
                      outputIt
      otherwise -> outputNotFound (sgfFn ++ ext)
   where fullName = sgfFn ++ ext
         outputIt = do isIn <- liftIO $ inDirectory "." fullName
                       if isIn
                           then do handle <- liftIO $ openFile fullName ReadMode
                                   bs <- liftIO $ BS.hGetContents handle
                                   outputFPS bs
                           else outputNotFound fullName



cgiMain :: CGI CGIResult
cgiMain =
  do req <- getInput "req"
     case req of
       Nothing -> outputError 400 "The request you have made is not valid and should not be attempted again." ["generate_preview called with no req input"]
       Just req -> generateOutputs req

main = runCGI (handleErrors cgiMain)
