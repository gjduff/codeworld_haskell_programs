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
     draw a tree n levels high. at the ends of the branches, 
     draw circular flowers.
     
     Lena wants purple AND pink flowers. figure out how.
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
             | otherwise =  tree 6 0.35
  where
    increments :: [Double]
    increments = map (*0.035) [0 .. 10]


{-
     main program. start the animation
-}
main = do
  animationOf controller
-- drawingOf (tree 5 0.35)
