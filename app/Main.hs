module Main where

import Graphics.Gloss.Interface.Pure.Game
import Data.List (nubBy)

main :: IO ()
main = play
  (InWindow "midpoint clicker" (800, 600) (0, 0))
  white
  30
  []
  render
  handleInput
  (const id)

type IPoint = (Int, Int)

squaredDistance :: IPoint -> IPoint -> Int
squaredDistance (x1, y1) (x2, y2) =
  let
    dx = x1 - x2
    dy = y1 - y2
  in dx * dx + dy * dy

type Points = [IPoint]

render :: Points -> Picture
render = Pictures . map \(x, y) -> color black $ translate (fromIntegral x) (fromIntegral y) $ circle 2

handleInput :: Event -> Points -> Points
handleInput event points = case event of
  EventKey (MouseButton LeftButton) Down _ (x, y) -> mergeClosePoints $ (floor x, floor y) : points
  EventKey (SpecialKey KeySpace) Down _ _ -> mergeClosePoints $ points ++ midpoints points
  _ -> points

mergeClosePoints :: Points -> Points
mergeClosePoints = nubBy (\x y -> squaredDistance x y < 25)

midpoints :: Points -> Points
midpoints points = do
  (p1, i1) <- points `zip` [1..]
  (p2, i2) <- points `zip` [1..]
  if i1 < i2
  then return $ midpoint p1 p2
  else []

midpoint :: IPoint -> IPoint -> IPoint
midpoint (x1, y1) (x2, y2) = ((x1 + x2) `div` 2, (y1 + y2) `div` 2)
