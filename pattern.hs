import CodeWorld


{-
     for 72 seconds, add zig zags to pattern. after that, just 
     keep the pattern constant
-}
controller :: Double -> Picture
controller t = if t < 72 
               then foldr (&) blank (map (rotatePattern staticPattern) [0 .. t])
               else foldr (&) blank (map (rotatePattern staticPattern) [0 .. 72])
               

{-
     given a picture and number n, rotate the picture by n x 18 degrees
     and assign it one of 5 colors.
-}
rotatePattern :: Picture -> Double -> Picture
rotatePattern picture n = colored color $ rotated angle picture
  where
    angle = n * 18
    color = case ((round n) `mod` 6) of
            0 -> blue
            1 -> red
            2 -> green
            3 -> orange
            4 -> purple
            5 -> yellow


{-
     draw a squiggly line. many of these will make up the pattern
-}
staticPattern :: Picture
staticPattern = polyline [(0,0), (0.5,1), (0,2), (0.5,3), (0,4), (0.5,5), (0,6), (1,7)]
              & polyline [(-1,6.75), (2.5,6.25)]
              & polyline [(-1,4.0), (2.5,5.75)]
              & translated (-1) 3.5 $ solidCircle 0.25
              

{-
     start the pattern animation
-}
main = animationOf controller

