import CodeWorld



controller :: Double -> Picture
controller t = if t < 72 
               then foldr (&) blank (map (rotatePattern staticPattern) [0 .. t])
               else foldr (&) blank (map (rotatePattern staticPattern) [0 .. 72])
               

rotatePattern :: Picture -> Double -> Picture
rotatePattern picture n = colored color $ rotated angle picture
  where
    angle = n * 18
    color = case ((round n) `mod` 5) of
            0 -> blue
            1 -> red
            2 -> green
            3 -> orange
            4 -> purple


staticPattern :: Picture
staticPattern = polyline [(0,0), (0.5,1), (0,2), (0.5,3), (0,4), (0.5,5), (0,6), (1,7)]


main = animationOf controller

