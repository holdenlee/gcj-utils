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
import Control.Lens hiding ((|>))
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Array
--https://github.com/holdenlee/gcj-utils

import Utilities
import IOUtilities
import TypeSyns
import GCJParse

type Input = Int
type Output = Int

input :: String -> [Input]
input = justParse (emptyH $ listL int)

calc :: Input -> Output
calc = (+1)

output :: [Output] -> String
output li = 
  unlines (emap (\(x,y) -> "Case #"++(show x)++": "++(show y)) li)

main :: IO ()
main = mainF (\s -> output $ map calc (input s))
