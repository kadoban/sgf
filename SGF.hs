module SGF (Property(..),
            GameTree(..),
            Point(..),
            Color(..),
            MarkType(..),
            other,
            parseSGF) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Data.Maybe
import Data.List

data Property = BoardSize Point
              | Handicap Integer
              | Add Color [Point]
              | Clear [Point]
              | Move Color Point
              | Comment String
              | Mark MarkType [Point]
              | Labels [(Point, String)]
              | ToPlay Color
              | View [Point]
              | Property String [String]
    deriving (Show, Eq, Ord)
data GameTree = GameTree {nodes :: [[Property]], variations :: [GameTree]}
    deriving (Show, Eq)

data MarkType = Sq | Cr | Tr | L String
    deriving (Show, Eq, Ord)

data None = None
    deriving (Show, Eq)
type Point = (Integer, Integer)
data Double = One | Two
    deriving (Show, Eq, Ord)
data Color = Black | White
    deriving (Show, Eq, Ord)
other Black = White
other White = Black
compose a b = do x <- a
                 char ':'
                 y <- b
                 return (x, y)
pointCoords = ['a'..'z'] ++ ['A'..'Z']
point = do x <- oneOf pointCoords
           y <- oneOf pointCoords
           return (toInt x, toInt y)
    where toInt i = fromIntegral $ fromMaybe 0 (elemIndex i pointCoords)
move = point <|> (return (19, 19))
boardSize = (try both) <|> single
    where both = compose natural natural
          single = do x <- natural
                      return (x, x)
cpoint = (try cpoint') <|> point'
    where point' = do p <- point
                      return [p]
          cpoint' = do ((ax, ay), (bx, by)) <- compose point point
                       return [(x, y) | x <- [ax..bx], y <- [ay..by]]
none = return None
pointList = do ps <- many1 (brackets cpoint)
               return $ sort (concat ps)
epointList = (try emptyList) <|> pointList
    where emptyList = (brackets none) >> return []
labelList = do ps <- many1 (brackets (compose point simpleText))
               return $ sort ps
color = black <|> white
    where black = oneOf "bB" >> return Black
          white = oneOf "wW" >> return White
def = P.LanguageDef{P.commentStart    = "",
                    P.commentEnd      = "",
                    P.commentLine     = "",
                    P.nestedComments  = False,
                    P.identStart      = upper,
                    P.identLetter     = upper,
                    P.opStart         = oneOf "",
                    P.opLetter        = oneOf "",
                    P.reservedNames   = [],
                    P.reservedOpNames = [],
                    P.caseSensitive   = True}
tok = P.makeTokenParser def

parens      = P.parens     tok
semi        = P.semi       tok
semiSep1    = P.semiSep1   tok
lexeme      = P.lexeme     tok
ident       = P.identifier tok
whiteSpace  = P.whiteSpace tok
natural     = P.natural    tok
brackets parser = lexeme $ try br'
    where br' = do char '['
                   contents <- parser
                   char ']'
                   return contents

collection = do whiteSpace
                many1 (gameTree <?> "GameTree")
gameTree = parens gt' <?> "variation"
    where gt' = do ns <- nodeSeq
                   gs <- many gameTree
                   return (GameTree ns gs)
nodeSeq = many1 node
node = lexeme $
       do semi <?> "node"
          ps <- many prop
          return (sort ps)
prop = lexeme $
       do i  <- ident <?> "property"
          case i of
              "B"  -> do p <- brackets move
                         return (Move Black p)
              "W"  -> do p <- brackets move
                         return (Move White p)
              "AB" -> do ps <- pointList
                         return (Add Black ps)
              "AW" -> do ps <- pointList
                         return (Add White ps)
              "AE" -> do ps <- pointList
                         return (Clear ps)
              "C"  -> do ps <- brackets text
                         return (Comment ps)
              "PL" -> do c <- brackets color
                         return (ToPlay c)
              "VW" -> do ps <- epointList
                         return (View ps)
              "SZ" -> do (a, b) <- brackets boardSize
                         return (BoardSize (a, b))
              "SQ" -> do ps <- pointList
                         return (Mark Sq ps)
              "CR" -> do ps <- pointList
                         return (Mark Cr ps)
              "MA" -> do ps <- pointList
                         return (Mark Sq ps)
              "TR" -> do ps <- pointList
                         return (Mark Tr ps)
              "LB" -> do ps <- labelList
                         return (Labels ps)
              "HA" -> do n <- brackets natural
                         return (if (n >= 2) then (Handicap n)
                                             else (Property "HA" [show n])
                                )
              otherwise -> do ps <- many1 propValue
                              return (Property i ps)
propValue = brackets cValueType
cValueType = text

lineBreaks = try crlf <|> try lfcr <|> cr <|> lf
    where crlf = (char '\n') >> (char '\r') >> return "\n"
          lfcr = (char '\r') >> (char '\n') >> return "\n"
          cr   = (char '\n') >> return "\n"
          lf   = (char '\r') >> return "\n"
convertedWS = do oneOf "\t\v\f\a\b "
                 return " "
convertedLineBreaks = lineBreaks >> return " "
softBreak = (char '\\') >> lineBreaks >> (return "")
anyChar' = do c <- anyChar
              return [c]
notEndText = do c <- noneOf "]"
                return [c]
quotedChar = do char '\\'
                convertedWS <|> anyChar' <?> "quoted character after \\"
quotedSimpleChar = do char '\\'
                      convertedWS <|> convertedLineBreaks <|> anyChar' <?> "quoted character after \\"
textChar = try softBreak <|> try quotedChar <|> convertedWS <|> notEndText <?> "end of property value"
simpleTextChar = try softBreak
             <|> try quotedSimpleChar
             <|> convertedWS
             <|> convertedLineBreaks
             <|> notEndText
             <?> "end of property value"
text = do ss <- many textChar
          return (concat ss)
simpleText = do ss <- many simpleTextChar
                return (concat ss)

parseSGF input = parse collection "(unknown)" input

blah = do c <- getContents
          case parseSGF c of
                Left e -> do putStrLn "Error parsing input:"
                             print e
                Right r -> print r
