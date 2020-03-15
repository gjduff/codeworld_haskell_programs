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
