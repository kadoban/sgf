module Canvas where

import Control.Monad.State
import Game
import SGF

data Canvas s c = -- stateType and canvas result type
    Canvas { setupCanvas :: String                    -- filename of SGF file
                         -> Point                     -- board size
                         -> (Point, Point)            -- view rectangle (UL, BR), inclusive
                         -> (Int, Int)                -- available screen space (pixels)
                         -> StateT s IO c
           , nextPlayer :: Color
                        -> c
                        -> StateT s IO c
           , placeStone :: (Point, Color)
                        -> c
                        -> StateT s IO c
           , placeMark :: (Point, MarkType)
                       -> c
                       -> StateT s IO c
           , finalize :: c
                      -> StateT s IO c
           }

stSetup fname size view screen = return $ fname ++ "\n" ++ show size ++ "\n" ++ show view ++ "\n"

stToPlay _ c = return c
stPlaceStone _ c = return c
stPlaceMark _ c = return c
stFinalize c = return c

stCanvas = Canvas { setupCanvas = stSetup
                  , nextPlayer = stToPlay
                  , placeStone = stPlaceStone
                  , placeMark = stPlaceMark
                  , finalize = stFinalize
                  }

printOn :: (Show a, Show s) => Canvas s a -> s -> String -> Game -> IO a
printOn c init fname g = do (a, s) <- runStateT ((setupCanvas c) fname (19, 19) ((0, 0), (18, 18)) (200, 200)) init
                            return a
