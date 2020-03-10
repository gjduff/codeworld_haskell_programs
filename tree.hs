{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


{-
     draw a tree n levels high. at the ends of the branches, 
     draw circular flowers.
     
     Lena wants purple AND pink flowers. figure out how.
     may redo this project with an actual data structure holding color info
-}
tree :: Integer -> Double -> Picture
tree 0 inc = (colored pink (solidCircle inc))
tree 1 inc = (colored purple (solidCircle inc)) & polyline [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/5) (tree 0 inc) & rotated (- pi/5) (tree 0 inc))
tree n inc = polyline [(0,0),(0,1)] & translated 0 1 (
    rotated (pi/5) (tree (n-1) inc) & rotated (- pi/5) (tree (n-1) inc))


{-
     the animation controller. for first 10 seconds,
     make flower get gradually bigger. after 10 seconds, stays the same
-}
controller :: Double -> Picture
controller t | t < 10    =  tree 6 (increments !! (round t))
             | otherwise =  tree 6 0.36
  where
    increments :: [Double]
    increments = map (*0.035) [0 .. 10]


{-
     main program. start the animation
-}
main = do
  animationOf controller
-- drawingOf (tree 5 0.36)

