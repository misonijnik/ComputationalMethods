module ReverseInterpolation where

import FuncToTableHelper

getListOfSeg :: [Node] -> Value -> [Segment]
getListOfSeg [] _ = []
getListOfSeg [_] _ = []
getListOfSeg (a : nodes) y
    | (snd a <= y && snd b >= y) || (snd a >= y && snd b <= y) = res : getListOfSeg (tail nodes) y
    | otherwise                                                = getListOfSeg nodes y
    where b = head nodes
          res = (fst a,  fst b)