{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


{-
     Following three functions draw a colored light or black (turned off)
     depending if parameter is True or False
-}
redLight :: Bool -> Picture
redLight True = translated 0 (2.5) (colored red (solidCircle 1))
redLight False = translated 0 (2.5) (colored black (solidCircle 1))

yellowLight :: Bool -> Picture
yellowLight True = colored yellow (solidCircle 1)
yellowLight False = colored black (solidCircle 1)

greenLight :: Bool -> Picture
greenLight True = translated 0 (-2.5) (colored green (solidCircle 1))
greenLight False = translated 0 (-2.5) (colored black (solidCircle 1))


{-
     A Rectanguler frame to draw around the traffic light
-}
frame :: Picture
frame = rectangle 2.5 7.5


{-
     draw a set of traffic lights n times with a given color
     as the active light.
     This version used foldl and map instead of recursion
-}
drawLights :: Double -> Color -> Picture
drawLights 0 _ = blank
drawLights n color 
  | color == red    = foldl (&) rbb (map (trans rbb) [1..(n-1)])
  | color == yellow = foldl (&) byb (map (trans byb) [1..(n-1)])
  | color == green  = foldl (&) bbg (map (trans bbg) [1..(n-1)])
    where
      bbg = foldl (&) (redLight False)  [(yellowLight False), (greenLight True), frame]
      byb = foldl (&) (redLight False)  [(yellowLight True), (greenLight False), frame]
      rbb = foldl (&) (redLight True)  [(yellowLight False), (greenLight False), frame]
      trans :: Picture -> Double -> Picture
      trans pic nn = translated (3*nn) 0 pic


{-
     animation sequence.
     combine the frame and three lights. the light that turns on
     depends on the value of the timer. Sequence is
     green light 4s,  yellow 1s,   red 4s
-}
trafficController ::  Double -> Picture
trafficController time 
                | ((round time) `mod` 9) `elem` [0,1,2,3] = translated (-3) 0 $ drawLights 1 green
                | ((round time) `mod` 9) `elem` [4] =       translated (-3) 0 $ drawLights 1 yellow
                | ((round time) `mod` 9) `elem` [5,6,7,8] = translated (-3) 0 $ drawLights 1 red
                 

{-
     Main program. just start the animation
-}
main :: IO ()
main = animationOf trafficController


