module Sword.ViewPorter where

import Sword.Utils
import Sword.Hero

type ViewPort = (Coord, Coord)

insideViewPort :: ViewPort -> Coord -> Bool
insideViewPort ((x1, y1), (x2, y2)) (x, y) = xinside && yinside
  where xinside = (x1 <= x) && (x < x2)
        yinside = (y1 <= y) && (y < y2)

updateViewPort :: ViewPort -> Maybe Hero -> Coord -> ViewPort
updateViewPort v Nothing _ = v
updateViewPort ((x1, y1), (x2, y2)) (Just Hero{position = (c1, c2)}) (maxX, maxY) = ((x1', y1'), (x2', y2'))
  where viewPortX = (x1, x2)
        viewPortY = (y1, y2)
        (x1', x2') = updatePort c1 viewPortX (30, maxX)
        (y1', y2') = updatePort c2 viewPortY (2, maxY)

updatePort :: Int -> Coord -> Coord -> Coord
updatePort x (a, b) (pSize, pMax)
  | (x - a) <= pSize && a > 0 = (a - 1, b - 1)
  | (b - x) <= pSize && b <= pMax = (a + 1, b + 1)
  | otherwise = (a, b)
