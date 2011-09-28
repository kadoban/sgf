module Canvas where

import qualified Data.Map as M
import Control.Monad.State
import Text.Printf
import Data.List
import Game
import SGF
import qualified Graphics.Rendering.Cairo as C
import Data.Ord (comparing)
import Config

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
printOn c init g' =
    do (a, s) <- runStateT (setupStep
                        >>= nextPlayerStep
                        >>= stoneSteps
                        >>= markSteps
                        >>= finalizeStep) (initState c)
       return a
    where setupStep = (setupCanvas c) (bs g) (v g) screenSpace init
          nextPlayerStep = (nextPlayer c) (toPlay g)
          stoneSteps x = foldl' (>>=) (return x) (map (placeStone c) (M.assocs (stones $ board g)))
          markSteps x = foldl' (>>=) (return x) (map (placeMark c) (M.assocs (marks g)))
          finalizeStep = (finalize c)
          bs g = size $ board g
          v g = simpleView ((M.keys $ stones $ board g) ++ (M.keys $ marks g) ++ (view g))
                           (bs g)
          resultingStoneSize g = snd $ pixelMap (v g) screenSpace
          g = maxBy resultingStoneSize g' (flipBoard g')
          maxBy f a b = if f a < f b then b else a

simpleView :: [Point] -> Point -> (Point, Point)
simpleView pps (a, b) =
    fixup $ foldl' minMax init pps
    where init = ((a, b), (-1, -1))
          minMax ((lx, ly), (gx, gy)) (x, y) = ((min x lx, min y ly), (max x gx, max y gy))
          (a', b') = (a - 1, b - 1)
          -- fixup too small view, including negative view if pps == []
          fixup ((lx, ly), (gx, gy)) = if (((gx - lx) < 4 && (gy - ly) < 4) ||
                                           (gx - lx) <= 0 || (gy - ly) <= 0)
                                       then ((0, 0), (a', b'))
                                       else fixup' $
                                            ((max (lx - margin) 0 , max (ly - margin) 0),
                                             (min (gx + margin) a', min (gy + margin) b'))
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
    ((\(a, b) -> ((a - vxl) * stoneSize, (b - vyl) * stoneSize)), stoneSize)
    where stoneSize = if odd stoneSize' then stoneSize' else stoneSize' - 1
          stoneSize' = min (stoneSize'' vxl vxg scSzx) (stoneSize'' vyl vyg scSzy)
          stoneSize'' l g sz = sz // (g - l + 1)
          ((vxl,vyl), (vxg, vyg)) = v
          (scSzx, scSzy) = scSz

noopSetup _ _ _ = return
noopToPlay _ = return
noopPlaceStone _ = return
noopPlaceMark _ = return
noopFinalize = return
noopCanvas = Canvas { setupCanvas = noopSetup
                    , nextPlayer = noopToPlay
                    , placeStone = noopPlaceStone
                    , placeMark = noopPlaceMark
                    , finalize = noopFinalize
                    , initState = ()
                    }

toplayToPlay color _ = return $ show color
toplayCanvas = noopCanvas{nextPlayer = toplayToPlay}

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

convertPoint ssz (a, b) = (ssz / 2 + fromIntegral a, ssz / 2 + fromIntegral b)

createBoard scSz@(scw, sch) v@((lx, ly), (gx, gy)) (bw, bh) =
  do board <- C.createImageSurface C.FormatARGB32 boardw boardh
     C.renderWith board drawBoard
     return (board, toPixel, ssz')
    where (boardw, boardh) = (fromInteger $ (gx - lx + 1) * ssz,
                              fromInteger $ (gy - ly + 1) * ssz)
          (m, ssz) = pixelMap v scSz
          ssz' = fromIntegral ssz
          toPixel = convertPoint ssz' . m
          toPixelx x = fst $ toPixel (x, undefined)
          toPixely y = snd $ toPixel (undefined, y)
          boardEdgex x = x == 0 || x == bw - 1
          boardEdgey y = y == 0 || y == bh - 1
          drawBoard =
            do let (bwp, bhp) = (fromIntegral boardw, fromIntegral boardh)
               C.setSourceRGBA 0 0 0 0
               C.rectangle 0 0 bwp bhp
               C.fill
               let lxp = if boardEdgex lx then toPixelx lx else 0.5
               let lyp = if boardEdgey ly then toPixely ly else 0.5
               let gxp = if boardEdgex gx then toPixelx gx else bwp + 0.5
               let gyp = if boardEdgey gy then toPixely gy else bhp + 0.5
               let setupLine edge = if edge
                                         then C.setSourceRGB 0 0 0
                                         else C.setSourceRGB 0.6 0.6 0.6
               let drawLinex x = do setupLine (boardEdgex x)
                                    let xp = toPixelx x
                                    C.moveTo xp lyp
                                    C.lineTo xp gyp
                                    C.stroke
               let drawLiney y = do setupLine (boardEdgey y)
                                    let yp = toPixely y
                                    C.moveTo lxp yp
                                    C.lineTo gxp yp
                                    C.stroke
               C.setSourceRGB 0.86667 0.73725 0.41960 -- woodish color
               C.rectangle lxp lyp (gxp - lxp) (gyp - lyp)
               C.fill -- color board
               C.setAntialias C.AntialiasNone
               C.setLineWidth 1
               C.setLineCap C.LineCapSquare
               let xlines = map (\x -> (boardEdgex x, drawLinex x)) [lx..gx]
               let ylines = map (\y -> (boardEdgey y, drawLiney y)) [ly..gy]
               -- draw the board edges last, so they don't get overwritten
               foldl1 (>>) $ map snd $ sortBy (comparing fst) $ xlines ++ ylines
               let inView (x, y) = x <= gx && y <= gy && x >= lx && y >= ly
               let hoshis = filter inView $ hoshiPoints bw bh
               let drawHoshi p = do setupLine False
                                    let (x, y) = toPixel p
                                    let rad = if ssz >= 10 then 2 else 1
                                    C.arc x y rad 0 (2 * pi)
                                    C.strokePreserve
                                    C.fill
               foldl' (>>) (return ()) $ map drawHoshi hoshis

hoshiPoints w h | w /= h = []
                | w < 3 || w == 4 = []
                | w == 3 = [(1, 1)]
                | w == 5 = [(1, 1), (1, 3), (2, 2), (3, 1), (3, 3)]
                | otherwise = cornerHoshi ++ sideHoshi ++ centerHoshi
  where (w', h') = (w - 1, h - 1)
        ledge = if w < 12 then 2 else 3
        hedge = w' - ledge
        mid = w // 2
        cornerHoshi = [(ledge, ledge), (hedge, hedge), (ledge, hedge), (hedge, ledge)]
        sideHoshi = if odd w && w > 12 then [(ledge, mid), (mid, ledge), (hedge, mid), (mid, hedge)] else []
        centerHoshi = if odd w then [(mid, mid)] else []

drawStone col pnt (m, ssz) =
    do if col == Black then C.setSourceRGB 0 0 0 else C.setSourceRGB 1 1 1
       let (xp, yp) = m pnt
       C.setAntialias C.AntialiasDefault
       C.setLineWidth 1
       C.arc xp yp (ssz / 2 - 0.5) 0 (2 * pi)
       C.fillPreserve
       if ssz > 6 then C.setSourceRGB 0 0 0 else return ()
       C.stroke

drawMark p _ (m, ssz) =
    do let (xp, yp) = m p
       C.setAntialias C.AntialiasDefault
       C.setLineWidth (if ssz > 14 then 2 else 1)
       C.setSourceRGB 1 0 0
       let drawSize = if ssz < 8 then 0.5 else ssz / 6
       C.arc xp yp drawSize 0 (2 * pi)
       C.stroke

imgSetup sz v scSz _ =
    do (board, pixelMap, ssz) <- liftIO $ createBoard scSz v sz
       put (pixelMap, ssz)
       return board
imgToPlay _ c = return c
imgPlaceStone (pnt, col) c =
    do st <- get
       liftIO $ C.renderWith c (drawStone col pnt st)
       return c
imgPlaceMark (p, mrk) c =
    do st <- get
       liftIO $ C.renderWith c (drawMark p mrk st)
       return c
imgFinalize c = return c
imgCanvas = Canvas { setupCanvas = imgSetup
                   , nextPlayer = imgToPlay
                   , placeStone = imgPlaceStone
                   , placeMark = imgPlaceMark
                   , finalize = imgFinalize
                   , initState = undefined :: (Point -> (Double, Double), Double)
                   }
