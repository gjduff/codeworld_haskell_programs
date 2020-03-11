{-# LANGUAGE OverloadedStrings #-}
import CodeWorld


{-
     draw a dot at the coordinates passed in, but x coordinate
     scaled down so many points can fit
-}
trans :: (Double, Double) -> Picture
trans (x, y) = translated (0.15*x) y $ solidCircle 0.15


{-
     return a list of coordinates given an offset and a mod3 result.
     the phaseShift is so when they are given to a sine function, it can
     produce different phases. the mod3 result is so that only 1 of 3
     appears at a time for animation
-}
xAxisValues :: Int -> Int -> [Double]
xAxisValues phaseShift mod3res = map fromIntegral $ filter (\x -> x `mod` 3 == mod3res) $ map (+phaseShift) [1..128]


{-
     custom sine function with argment converted to radians
     and the sine function amplitude scaled
-}
mysin :: Double -> Double
mysin x = 3 * sin (pi*(x/32))


{-
     create a list of tuples representing x,y coords
     the y coord is the result of a sine operation on x
-}
setupZippedCoords :: Int -> Int -> [(Double,Double)]
setupZippedCoords mod3res shift = case mod3res of
                   0 -> zip (xAxisValues 0 0) (map mysin (xAxisValues shift 0))
                   1 -> zip (xAxisValues 0 1) (map mysin (xAxisValues shift 1))
                   2 -> zip (xAxisValues 0 2) (map mysin (xAxisValues shift 2))


{-
     given a time in seconds (and using one decimal place)
     draw three animated sine waves
-}
controller :: Double -> Picture
controller time = (colored blue  $ translated (-10) 0 track1)  &
                  (colored green $ translated (-10) 0 track2)  &
                  (colored red   $ translated (-10) 0 track3)
  where
    track1 = foldr (&) blank (map trans zippedCoords1)
    track2 = foldr (&) blank (map trans zippedCoords2)
    track3 = foldr (&) blank (map trans zippedCoords3)
    mod3 = ((round (time*10)) `mod` 3) 
    zippedCoords1 = setupZippedCoords mod3 0
    zippedCoords2 = setupZippedCoords mod3 21
    zippedCoords3 = setupZippedCoords mod3 42


{-
     start the animation
-}
main :: IO ()
main = animationOf controller



