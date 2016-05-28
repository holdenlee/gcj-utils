{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
 -XLambdaCase
 -XTupleSections
#-}

module Main where
import System.Environment
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Array
-- https://github.com/holdenlee/haskell-utilities

import Utilities
import ParseUtilities
import TypeSyns
import GCJParse

type Input = [Int]
type Output = Int

input :: String -> [[Int]]
input = justParse (emptyH $ listL $ readN_ int)
{-
readH_ :: (Int -> Int) -> Parser a -> Parser [a]
readH' :: (a -> Int) -> Parser a -> Parser b -> Parser (a, [b])

readN = readH id
readN_ = readH_ id
-}

calc :: Input -> Output
calc = sum

output :: [Output] -> String
output li = 
  unlines (emap (\(x,y) -> "Case #"++(show x)++": "++(show y)) li)

main:: IO ()
main = do
  args <- getArgs
  let inputF = if (length args >= 1) then args !! 0 else "a.in"
  let outputF = if (length args >= 2) then args !! 1 else (take (length inputF - 2) inputF ++ "out")
  ioFile inputF outputF (\s -> output $ map calc $ input s)
