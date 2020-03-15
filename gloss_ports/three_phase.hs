 {-# LANGUAGE OverloadedStrings #-}
-- import CodeWorld
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import qualified Data.Text as T
import qualified Data.List as D
import qualified System.Random as R
import qualified Control.Monad as C


{----------------------------------------------------}
--         Code world emulation layer               --
{----------------------------------------------------}

import Prelude hiding (Double)
type Double = Float       -- it's hard to believe I can do this

type Picture = PG.Picture
data Event = KeyPress String | PointerPress (Float,Float) | TimePassing Float
            -- might have to OR this with some other stuff, like for time, mouse click


scaled l w = PG.Scale (realToFrac l) (realToFrac w)
translated x y = PG.Translate (realToFrac x) (realToFrac y)
rotated n = PG.Rotate (realToFrac (n * (-1) * 180 / 3.14))
colored = PG.Color

circle n = PG.Circle (realToFrac n)  -- because gloss uses Float, not Double
solidCircle n = PG.circleSolid (realToFrac n)
solidRectangle l w = PG.rectangleSolid (realToFrac l) (realToFrac w)
rectangle = PG.rectangleWire

data Monospace = Monospace
data Bold = Bold
styledLettering style font t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)
lettering t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)

polygon = PG.lineLoop
solidPolygon p = PG.polygon $ (tail p) ++ [(head p)]
polyline = PG.Line

type Color = PG.Color

green = PG.green
red = PG.red
blue = PG.blue
purple = PG.violet
blank = PG.blank
orange = PG.orange
black = PG.black
yellow = PG.yellow
grey = PG.greyN 0.5
brown = PG.makeColorI 185 122 87 255
pink = PG.makeColorI 255 174 201 255

(&) f s = mappend s f

windowDisplay :: PG.Display
windowDisplay = PG.InWindow "Window" (640, 480) (50, 10)

updateFunc f dt state =  
    f (TimePassing dt) state

eventFunc f event state = 
  case event of 
    PG.EventKey (PG.Char chr) PG.Down _ (x,y)   -> f (KeyPress [chr]) state
    PG.EventKey (PG.SpecialKey PG.KeyUp) PG.Down _ (x,y)   -> f (KeyPress "Up") state
    PG.EventKey (PG.SpecialKey PG.KeyDown) PG.Down _ (x,y) -> f (KeyPress "Down") state
    PG.EventKey (PG.SpecialKey PG.KeyLeft) PG.Down _ (x,y) -> f (KeyPress "Left") state
    PG.EventKey (PG.SpecialKey PG.KeyRight) PG.Down _ (x,y) -> f (KeyPress "Right") state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Down _ (x,y)  -> f (PointerPress (x,y)) state
    _                                                      -> f (KeyPress "OTHER") state


renderFunc f state = PG.Scale 20 20 $ f state

activityOf initStates processEvents render = PG.play 
   windowDisplay
   PG.white
   30
   initStates
   (renderFunc render)
   (eventFunc processEvents)
   (updateFunc processEvents)



-- animationOf requires some finessing
-- param is a:    controller :: Double -> Picture
-- aka            controller :: Float -> Picture
-- seems like I have to write the "f" that updateFunc2 would get
-- and make that it's param in animationOf

renderFunc2 f state = PG.Scale 20 20 $ f state

updateFunc2 dt state = state + dt

animationOf control = PG.play 
   windowDisplay
   PG.white
   30
   0
   (renderFunc2 control)
   (eventFunc (\e s -> s) )
   (updateFunc2 )

{----------------------------------------------------}
--     Paste Code world code below!!                --
{----------------------------------------------------}




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
