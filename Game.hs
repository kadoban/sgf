-- Copyright 2011 by Joshua Simmons <joshua.simmons@emptypath.com>
{-# LANGUAGE NamedFieldPuns #-}

module Game where

import SGF
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.List (foldl',sort,sortBy)
import qualified Data.List as L
import Data.Ord (comparing)

data Board = Board { stones    :: Map Point Color
                   , size      :: Point
                   , prisoners :: [Color]
                   }
    deriving (Show, Eq)

empty = Board {stones = M.empty, size = (19, 19), prisoners = []}

data Game = Game { board   :: Board
                 , moveNum :: Integer
                 , toPlay  :: Color
                 , tree    :: GameTree
                 , view    :: [Point]
                 , comment :: String
                 , marks   :: M.Map Point MarkType
                 , applied :: [Property]
                 }
    deriving (Show, Eq)

start tree = Game {board = empty
                  , toPlay = Black
                  , moveNum = 0
                  , tree = tree
                  , view = []
                  , comment = ""
                  , marks = M.empty
                  , applied = []
                  }

flipBoard g@Game{board, view, marks} = g{ board = board { size = swap size
                                                        , stones = swapKeys stones
                                                        }
                                        , view = map swap view
                                        , marks = swapKeys marks
                                        }
    where Board{stones, size} = board
          swap (a, b) = (b, a)
          swapKeys = M.mapKeys swap

peekToPlay g =
    if toPlaySet then g else -- if there was a ToPlay, trust it
         case nextMoves (tree g) of
           [] -> g
           ms -> g{toPlay = mostCommonColor ms}
    where toPlaySet = [] /= filter isToPlay (applied g)
          isToPlay (ToPlay _) = True
          isToPlay _          = False
          mostCommonColor ms = head $ last $ sortBy (comparing length) $ L.group $ sort ms
          colorMove ns = head $ (dropWhile isNothing (map colorMove' ns)) ++ [Nothing]
          colorMove' (Move c _) = Just c
          colorMove' (ToPlay c) = Just c
          colorMove' _          = Nothing
          nextMoves gt@(GameTree (n:ns) vss) = case colorMove n of
                                                 Nothing -> nextMoves (gt{nodes = ns})
                                                 Just c  -> [c]
          nextMoves    (GameTree _      vss) = concat $ map nextMoves vss

applyProperty :: Game -> Property -> Game
applyProperty game prop =
    let g = game{applied = prop : (applied game)}
        b = board game
        m = marks game in
    case prop of
        BoardSize sz  -> g{board = b{size = sz}}
        Add c ps      -> g{board = b{stones = stones'}}
            where stones' = foldl' insStone (stones b) ps
                  insStone = flip (c `ins'`)
                  ins' = flip M.insert
        Clear ps      -> g{board = b{stones = stones'}}
            where stones' = foldl' (flip M.delete) (stones b) ps
        Move c p      -> g{board = b''',
                           moveNum = (moveNum g) + 1,
                           toPlay = other c}
            where b'   = b{stones = M.insert p c (stones b)}
                  b''  = foldl' captureLifeless b' (neighbors b' p)
                  b''' = captureLifeless b'' p
        View ps       -> g{view = ps}
        Comment s     -> g{comment = s}
        a@(Mark {})   -> g{marks = insertMarks m a}
        Labels ps     -> g{marks = m'}
            where m' = foldl' insertMarks m (map toMark ps)
                  toMark (p, s) = Mark (L s) [p]
        ToPlay c      -> g{toPlay = c}
        Handicap n | n >= 2 && (moveNum g) == 0 -> g {toPlay = White}
                   | otherwise -> g
        otherwise     -> g

insertMarks :: (M.Map Point MarkType) -> Property -> (M.Map Point MarkType)
insertMarks m (Mark t ps) = foldl' insM m ps
    where insM = flip (t `ins`)
          ins  = flip M.insert
insertMarks m _ = m

clearTransient :: Game -> Game
clearTransient g = g{comment = "", marks = M.empty}

applyNode :: Game -> [Property] -> Game
applyNode game node = foldl' applyProperty (clearTransient game) node

advance :: Game -> Game
advance game | (nodes $ tree $ game) == [] = game
             | otherwise = applyNode (game {tree = tree'}) n
    where tree' = (tree game) {nodes = ns}
          n:ns = nodes $ tree $ game

advanceWhile f = result . myIterate
    where result (a:gss@(b:gs)) | f a = result gss
                                | otherwise = a
          result [a] = a
          myIterate g | (nodes $ tree $ g) == [] = [g]
                      | otherwise = g : (myIterate (advance g))

onBoard board point = px >= 0 && py >= 0 && px < bx && py < by
    where px = fst point
          py = snd point
          bx = fst (size board)
          by = fst (size board)

neighbors board point = filter (onBoard board) possible
    where possible = [(x, y) | x <- [px - 1, px + 1],
                               y <- [py - 1, py + 1]]
          px = fst point
          py = snd point

group board point | M.notMember point board' = []
                  | otherwise = group' board [point] point' [point] stoneColor
    where (Just stoneColor) = M.lookup point board'
          board' = stones board
          point' = S.singleton point

group' b g v [] _ = g
group' b g v (x:xs) c = group' b g' v' xs' c
    where v' = foldl' (flip S.insert) v (neighbors b x)
          g' = newGroupMembers ++ g
          xs' = newGroupMembers ++ xs
          newGroupMembers = (filter sameColor (neighbors b x))
          sameColor p = (M.member p (stones b)) && (M.lookup p (stones b)) == (Just c)

liberties' b p = length $ liberties b p

liberties board point = S.toList $ foldr S.union S.empty (map libs' (group board point))
    where libs' p = S.fromList $ filter isEmpty (neighbors board p)
          isEmpty p = M.notMember p (stones board)

capture board point = foldl' capture' board (group board point)
    where capture' b p = b{stones = stones', prisoners = prisoners'}
           where stones' = M.delete p (stones board)
                 prisoners' = case (M.lookup p (stones board)) of
                                   Nothing -> (prisoners board)
                                   Just c  -> c : (prisoners board)

captureLifeless b p | liberties' b p == 0 = capture b p
                    | otherwise = b
