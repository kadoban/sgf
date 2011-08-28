module Canvas where

import qualified Data.Map as M
import Control.Monad.State
import Text.Printf
import Data.List
import qualified Graphics.GD as G
import Game
import SGF

screenSpace = (150, 150)

data Canvas s c = -- stateType and canvas result type
    Canvas { setupCanvas :: Point                     -- board size
                         -> (Point, Point)            -- view rectangle (UL, BR), inclusive
                         -> (Integer, Integer)        -- available screen space (pixels)
                         -> c
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
           , initState :: s
           }

printOn :: Canvas s a -> a -> Game -> IO a
printOn c init g =
    do (a, s) <- runStateT (setupStep
                        >>= nextPlayerStep
                        >>= stoneSteps
                        >>= markSteps
                        >>= finalizeStep) (initState c)
       return a
    where setupStep = (setupCanvas c) bs v screenSpace init
          nextPlayerStep = (nextPlayer c) (toPlay g)
          stoneSteps x = foldl' (>>=) (return x) (map (placeStone c) (M.assocs (stones $ board g)))
          markSteps x = foldl' (>>=) (return x) (map (placeMark c) (M.assocs (marks g)))
          finalizeStep = (finalize c)
          bs = size $ board g
          v  = simpleView (view g) bs

simpleView :: [Point] -> Point -> (Point, Point)
simpleView pps (a, b) =
    fixup $ foldl' minMax init pps
    where init = ((a, b), (-1, -1))
          minMax ((lx, ly), (gx, gy)) (x, y) = ((min x lx, min y ly), (max x gx, max y gy))
          (a', b') = (a - 1, b - 1)
          -- fixup too small view, including negative view if pps == []
          fixup ((lx, ly), (gx, gy)) = if (gx - lx) <= 5 || (gy - ly) <= 5
                                       then ((0, 0), (a', b'))
                                       else fixup' $
                                            ((max (lx - margin) 0 , max (ly - margin) 0),
                                             (min (gx + margin) a', max (gy + margin) b'))
          margin = 2 -- extra space around view
          -- fixup' move view to board edges if it's already close
          -- makes board sized problems look much nicer
          fixup' ((lx, ly), (gx, gy)) = ((lx', ly'), (gx', gy'))
              where lx' = if lx <= 3 then 0 else lx
                    ly' = if ly <= 3 then 0 else ly
                    gx' = if a' - gx <= 3 then a' else gx
                    gy' = if b' - gy <= 3 then b' else gy

-- integer division
(//) :: (Integral a) => a -> a -> a
a // b = truncate $ (fromIntegral a) / (fromIntegral b)

pixelMap :: (Point, Point) -> (Integer, Integer) -> ((Point -> Point), Integer)
pixelMap v scSz =
    ((\(a, b) -> (a * stoneSize, b * stoneSize)), stoneSize)
    where stoneSize = min (stoneSize' vxl vxg scSzx) (stoneSize' vyl vyg scSzy)
          stoneSize' l g sz = sz // (g - l + 1)
          ((vxl,vyl), (vxg, vyg)) = v
          (scSzx, scSzy) = scSz

stSetup size view screen c = return $ c ++ show size ++ "\n" ++ show view ++ "\n"
stToPlay _ c = return c
stPlaceStone _ c = return c
stPlaceMark _ c = return c
stFinalize c = return c
stCanvas = Canvas { setupCanvas = stSetup
                  , nextPlayer = stToPlay
                  , placeStone = stPlaceStone
                  , placeMark = stPlaceMark
                  , finalize = stFinalize
                  , initState = ()
                  }

jsSetup sz v scSz c =
    do put (pixelMap v scSz)
       return $ c ++ "window.bbls_pts =\n[\n"
jsPlaceStone (p, col) c =
    do (m, sz) <- get
       let (x', y') = m p
       let sz' = sz // 2
       return $ c ++ (printf "[%d,%d,%d,%s],\n" x' y' sz' col')
    where col' = if col == Black then "\"000000\"" else "\"d0d0d0\""
jsToPlay _ c = return c
jsPlaceMark _ c = return c
jsFinalize c = return $ c ++ "\n]"
jsCanvas = Canvas { setupCanvas = jsSetup
                  , nextPlayer = jsToPlay
                  , placeStone = jsPlaceStone
                  , placeMark = jsPlaceMark
                  , finalize = jsFinalize
                  , initState = undefined :: ((Point -> Point), Integer)
                  }

convertPoint (a, b) = (fromInteger a, fromInteger b)
trans = G.rgba 255 0 0 127

imgSetup sz v scSz c =
    do let (m, ssz) = pixelMap v scSz
       let ssz' = fromInteger ssz
       w <- liftIO $ G.loadPngFile "white_stone.png" >>= G.resizeImage ssz' ssz'
       b <- liftIO $ G.loadPngFile "black_stone.png" >>= G.resizeImage ssz' ssz'
       put (convertPoint . m, \x -> if x == White then w else b)
       let ((vlx, vly), (vgx, vgy)) = v
       let boardSz = (fromInteger $ (vgx - vlx + 1) * ssz,
                      fromInteger $ (vgy - vly + 1) * ssz)
       board <- liftIO $ G.newImage boardSz
       liftIO $ G.fillImage trans board
       return board
imgToPlay _ c = return c
imgPlaceStone (pnt, col) c =
    do (m, stf) <- get
       let stImg = stf col
       sz <- liftIO $ G.imageSize stImg
       liftIO $ G.copyRegion (0, 0) sz stImg (m pnt) c
       return c
imgPlaceMark _ c = return c
imgFinalize c = return c
imgCanvas = Canvas { setupCanvas = imgSetup
                   , nextPlayer = imgToPlay
                   , placeStone = imgPlaceStone
                   , placeMark = imgPlaceMark
                   , finalize = imgFinalize
                   , initState = undefined :: (Point -> G.Point, Color -> G.Image)
                   }
