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
data Event = KeyPress T.Text | PointerPress (Float,Float) | TimePassing Float | PlaceHolder
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
    PG.EventKey (PG.Char chr) PG.Down _ (x,y)   -> f (KeyPress (T.pack [chr])) state
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

updateFunc2 dt state = dt

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




colors :: [Color]
colors = [red, green, blue]

data States = States {elapsedTime :: Double, getX :: Double, getY :: Double, getKeyS :: String}


{-
     The initial state is zero elapsed time and no mouse presses
-}
initialStates :: States
initialStates = States 0 0 0 ""


{-
     every elapsed time event, increment the time and pass to
     the new state.
     every mouse click, update the coordinates
-}
processEventToUpdateState:: Event -> States -> States
processEventToUpdateState event states = 
  let etime = elapsedTime states
      xx    = getX states
      yy    = getY states
      ss    = getKeyS states
  in
  case event of
    TimePassing x      ->  States (etime+1) xx yy ss
    PointerPress (x,y) ->  States (etime+1) x y ss
    KeyPress str       ->  States etime xx yy (T.unpack str) 
    _                  ->  states
    
        
          
{-
     draw the scene. make circle change color every 5 seconds.
     to do this, get the elapsed time from the current state,
     divide by 100 to get seconds,
     mod by 15 to make them all between 0 and 14
     integer divide by 5 so   0 to 4 is 0
                              5 to 9 is 1
                            10 to 14 is 2
     also, render the coordinates of the last mouse click
     
-}
wholeScene :: States -> Picture
wholeScene states = ( translated (-3) 3 $ lettering $ T.pack $ (getKeyS states)) & 
                    ( translated 3 3 $ lettering $ T.pack $ xy) & 
                    ( translated 5 5 $ lettering $ T.pack $ y20happened) & 
                    ( lettering $ T.pack $ show etime) & 
                    ( colored (colors !! index) $ solidCircle 2 )
  where index =  round etime `div` 100 `mod` 15 `div` 5
        etime = elapsedTime states
        xy    = show (roundToPlaces (getX states) 2) ++ "," ++ show (roundToPlaces (getY states) 2)
        y20happened = show $ if (round etime `mod` 20) == 19 then 1 else 0


{-
     round a double to a specified number of decimal places
-}
roundToPlaces :: Double -> Int -> Double
roundToPlaces n s = fromIntegral (round (n * factor)) / factor
    where factor = fromIntegral (10^s) 


{-
     Main program. just start the animation
-}
main :: IO ()
main = activityOf initialStates processEventToUpdateState wholeScene


