{-# OPTIONS
 
 -XTypeSynonymInstances
 -XFlexibleInstances
 -XTypeFamilies
#-}

module GCJParse where

import TypeSyns
import Text.Parsec
import Text.Parsec.String
import Control.Monad

import Utilities

spaces' :: Parser String
spaces' = many1 (char ' ')

genWord :: Parser String
genWord = many1 (noneOf " \n")

int :: Parser I
int = fmap read genWord

integer :: Parser In
integer = fmap read genWord

str :: Parser S
str =  genWord

line :: Parser S
line = do 
  l <- many (noneOf "\n")
  newline
  return l

chr :: Parser C
chr = anyChar

float :: Parser F
float = fmap read genWord

double :: Parser D
double = fmap read genWord

bool:: Parser B
bool = fmap read genWord

type family Extract a :: * where
  Extract (Parser a, Parser b) = (a,b)
  Extract (Parser a, Parser b, Parser c) = (a,b,c)
  Extract (Parser a, Parser b, Parser c, Parser d) = (a,b,c,d)
  Extract (Parser a, Parser b, Parser c, Parser d, Parser e) = (a,b,c,d,e)
  Extract (Parser a, Parser b, Parser c, Parser d, Parser e, Parser f) = (a,b,c,d,e, f)

class IsPTuple t where
  tuple :: Parser x -> t -> Parser (Extract t)

tupleS :: (IsPTuple t) => t -> Parser (Extract t)
tupleS = tuple spaces'

tupleL :: (IsPTuple t) => t -> Parser (Extract t)
tupleL = tuple newline

instance IsPTuple (Parser a,Parser b) where
  tuple p (pa,pb) = do
      a <- pa
      p
      b <- pb
      return (a,b)

instance IsPTuple (Parser a,Parser b, Parser c) where
  tuple p (pa,pb,pc) = do
      a <- pa
      p
      b <- pb
      p
      c <- pc
      return (a,b,c)

instance IsPTuple (Parser a,Parser b, Parser c, Parser d) where
  tuple p (pa,pb,pc,pd) = do
      a <- pa
      p
      b <- pb
      p
      c <- pc
      p
      d <- pd
      return (a,b,c,d)

instance IsPTuple (Parser a,Parser b, Parser c, Parser d, Parser e) where
  tuple p (pa,pb,pc,pd,pe) = do
      a <- pa
      p
      b <- pb
      p
      c <- pc
      p
      d <- pd
      p
      e <- pe
      return (a,b,c,d,e)

instance IsPTuple (Parser a,Parser b, Parser c, Parser d, Parser e, Parser f) where
  tuple p (pa,pb,pc,pd,pe, pf) = do
      a <- pa
      p
      b <- pb
      p
      c <- pc
      p
      d <- pd
      p
      e <- pe
      p
      f <- pf
      return (a,b,c,d,e, f)

list :: Parser x -> Parser a -> Parser [a]
list = flip sepEndBy
--sepBy

listS :: Parser a -> Parser [a]
listS = list spaces'

listL :: Parser a -> Parser [a]
listL = list newline

emptyH = (line >>)

readH :: (Int -> Int) -> Parser a -> Parser (Int, [a])
readH f p = do
  i <- int
  {-  ps <- sepEndBy p newline-}
  if (f i <= 0)
    then return (i, [])
    else do
      newline
      p0 <- p
      ps <- replicateM ((f i) - 1) (newline >> p)
      --replicateM ((f i) - 1) (newline >> p)
      return (i, p0:ps)

readH_ :: (Int -> Int) -> Parser a -> Parser [a]
readH_ = fmap snd `c2` readH

readN = readH id

readN_ = readH_ id

readH' :: (a -> Int) -> Parser a -> Parser b -> Parser (a, [b])
readH' f ph p = do
  i <- ph
  if (f i <= 0) 
    then return (i, [])
    else do
      newline
      p0 <- p
      ps <- count ((f i) - 1) (newline >> p) 
      --replicateM ((f i) - 1) (newline >> p)
      return (i, p0:ps)

readH'_ = fmap snd `c3` readH'

withEnd :: Parser a -> Parser a
withEnd p = do
  i <- p
--  many anyToken
  option ' ' newline
  return i

justParse :: Parser a -> String -> a
justParse p = fromRight . parse (withEnd p) ""

{-
justParse :: (Show a) => Parser a -> String -> a
justParse p x = fromRight $ (debugShow (parse (withEnd p) "" x))
-}
