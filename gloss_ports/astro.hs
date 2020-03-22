 {-# LANGUAGE OverloadedStrings #-}
-- import CodeWorld
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import qualified Data.Text as T
import qualified Data.List as D
import qualified System.Random as R
import qualified Control.Monad as C


{----------------------------------------------------}
--         CodeWorld emulation layer               --
{----------------------------------------------------}


{-
     CodeWorld uses doubles, and by and large gloss uses float
     so to make pasted CodeWorld code work ...
-}
import Prelude hiding (Double)
type Double = Float       


{-
     Types from CodeWorld that we need to make Events work
-}
data Event = KeyPress String | 
             PointerPress (Float,Float) | 
             TimePassing Float | 
             PointerRelease (Float,Float) |
             PointerMovement (Float,Float)
            -- might have to OR this with some other stuff, like for time, mouse click


{-
     Types from CodeWorld translated to Gloss
-}
type Picture = PG.Picture
type Point = PG.Point
type Color = PG.Color


{-
     location and shape functions from CodeWorld translated to Gloss
-}
scaled l w = PG.Scale (realToFrac l) (realToFrac w)
translated x y = PG.Translate (realToFrac x) (realToFrac y)
rotated n = PG.Rotate (realToFrac (n * (-1) * 180 / 3.14))
colored = PG.Color
circle n = PG.Circle (realToFrac n)  -- because gloss uses Float, not Double
solidCircle n = PG.circleSolid (realToFrac n)
solidRectangle l w = PG.rectangleSolid (realToFrac l) (realToFrac w)
rectangle = PG.rectangleWire
polygon = PG.lineLoop
polyline = PG.Line

solidPolygon p = PG.polygon $ (tail p) ++ [(head p)]


{-
     CodeWorld text functions translated to gloss.
     font types made just to make pasted code compile.
     fonts not actually used yet.
-}
data Monospace = Monospace
data Bold = Bold
styledLettering style font t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)
lettering t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)


{-
     CodeWorld colors translated to gloss.
     NOTE:
        RGBA in CodeWorld is a data constructor
        gloss "makeColor" is a function, so no direct translation
        possible, so called it rgba
        so need to search and replace RGBA with rgba :(
-}
green = PG.green
red = PG.red
blue = PG.blue
purple = PG.violet
blank = PG.blank
orange = PG.orange
black = PG.black
white = PG.white
yellow = PG.yellow
grey = PG.greyN 0.5
brown = PG.makeColorI 185 122 87 255
pink = PG.makeColorI 255 174 201 255
light = PG.light
rgba = PG.makeColor  


{-
     CodeWorld puts together pictures with & operator
     so we need to define it to work with Gloss
-}
(&) f s = mappend s f


{-
     define the window that gloss application runs in
-}
windowDisplay :: PG.Display
windowDisplay = PG.InWindow "Window" (640, 480) (50, 10)


{-
     CodeWorld doesn't have seperate functions for the time
     passing event and the key/mouse events so we define the
     two that Gloss needs and pass them the CodeWorld event 
     function to call.
     Mouse Pointer events get scaled down by 20, since the whole
     scene gets rendered scaled up by 20
-}
updateFunc f dt state =  
    f (TimePassing dt) state

eventFunc f event state = 
  case event of 
    PG.EventKey (PG.Char chr) PG.Down _ (x,y)                  -> f (KeyPress [chr]) state
    PG.EventKey (PG.SpecialKey PG.KeyUp) PG.Down _ (x,y)       -> f (KeyPress "Up") state
    PG.EventKey (PG.SpecialKey PG.KeyDown) PG.Down _ (x,y)     -> f (KeyPress "Down") state
    PG.EventKey (PG.SpecialKey PG.KeyLeft) PG.Down _ (x,y)     -> f (KeyPress "Left") state
    PG.EventKey (PG.SpecialKey PG.KeyRight) PG.Down _ (x,y)    -> f (KeyPress "Right") state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Down _ (x,y) -> f (PointerPress (x/20,y/20)) state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Up _ (x,y)   -> f (PointerRelease (x/20,y/20)) state
    PG.EventMotion (x,y)                                       -> f (PointerMovement (x/20,y/20)) state
    _                                                          -> f (KeyPress "OTHER") state


{-
     We render the with the CodeWorld render function.
     scaling by 20 otherwise looks way too small in Gloss
-}
renderFunc f state = PG.Scale 20 20 $ f state

{-
     CodeWorld activityOf implementartion using
     Gloss's "play" function
-}
activityOf initStates processEvents render = PG.play 
   windowDisplay
   PG.white
   30
   initStates
   (renderFunc render)
   (eventFunc processEvents)
   (updateFunc processEvents)





-- renderFunc2 f state = PG.Scale 20 20 $ f state

{-
     the time passing event to make CodeWorld
     "animationOf" controller function happen with
     the ellapsed time passed to it
-}
updateFunc2 dt state = state + dt

{-
     CodeWorld animationOf implementartion using
     Gloss's "play" function. animationOf uses no
     mouse/key events so we just pass a do-nothing lambda
     to that event function
-}
animationOf control = PG.play 
   windowDisplay
   PG.white
   30
   0
   (renderFunc control)
   (eventFunc (\e s -> s) )
   (updateFunc2 )




{----------------------------------------------------}
--     Paste Code world code below!!                --
--                                                  --
-- caveats:                                         --
--     RGBA has to become rgba                      --
{----------------------------------------------------}





type Coords = (Double, Double)

data State = State {
                     getAstroCoords :: Coords
                    ,getElapsedTime :: Double
                    ,getIsJumping :: Bool
                    ,getGoingLeft :: Bool
                   }

{-
     Set the initial state of the astronaut: ie - position
-}
initialState :: State
initialState = State (9, 0) 0 False True


jumpTime :: Double
jumpTime = 20

stepSize :: Double
stepSize = jumpTime / 100

leftSide :: Double
leftSide = -10

rightSide :: Double
rightSide = 10


{-
     the astronaut is jumping so: 
         update it's coordinates
         stop jumping after a certain amount of time
         turn right after it hits left part of screen and vuce versa
-}
updateJump :: State -> State
updateJump state = State newCoords (etime+1) newIsJumping newGoingLeft
  where
    dx = if goingLeft == True then (-stepSize) else stepSize
    etime = getElapsedTime state
    acoords = getAstroCoords state
    isJumping = getIsJumping state
    goingLeft = getGoingLeft state
    newCoords = ((fst acoords + dx), abs (5*sin (pi * etime/jumpTime)) )
    newIsJumping = if etime < (jumpTime+1) then True else False
    newGoingLeft = if goingLeft == True && (fst acoords) < leftSide 
                   then False
                   else if goingLeft == False && (fst acoords) > rightSide
                        then True
                        else goingLeft


{-
     given an event and current state, compute the new state
-}  
processEvent :: Event -> State -> State
processEvent event state = 
   case event of
     PointerPress (x,y) -> State acoords etime True goingLeft
     TimePassing x      -> if isJumping == True 
                           then updateJump state
                           else State acoords 0 isJumping goingLeft
     -- TimePassing x      -> if isJumping == True  
     --                      then State (setCoordsIfJumping state) (etime+1) 
     --                                 (setIsJumpingByTime state) (setIsGoingLeft state)
     --                      else State acoords 0 isJumping goingLeft
     _                  -> state
   where
     etime = getElapsedTime state
     acoords = getAstroCoords state
     isJumping = getIsJumping state
     goingLeft = getGoingLeft state


{-
     given the current state, draw the scene, including
     an astronaut at the coordinates held in the state
-}
renderScene :: State -> Picture
renderScene state = (lettering $ T.pack xText) &
                    (translated x y $ drawAstronaut state)                                  
  where
    (x,y) = getAstroCoords state
    xText = show x
    


{-
     draw an astronaut
-}
drawAstronaut :: State -> Picture
drawAstronaut state = 
  (translated (facing * 0.56) 2.4 $ colored white $ solidCircle 0.10) &
  (translated (facing * 0.34) 2.2 $ colored black $ solidCircle 0.66) &
  (translated (facing * 0.4) 0.25 $ colored (grey) $ solidRectangle 0.5 0.6) &  
  (translated 0 2.2 $ colored (light grey) $ solidCircle 1) &
  (translated 0 0.5 $ colored (light grey) $ solidCircle 1) &
  (translated 0 0 $ colored (light grey) $ solidRectangle 2 1.2) &
  (translated 0.5 (-0.5) $ colored ( grey) $ solidCircle 0.5) &
  (translated (-0.5) (-0.5) $ colored ( grey) $ solidCircle 0.5) 
  where
    facing = if (getGoingLeft state) == True then (-1) else 1

  

{-
     start the activity
-}
main :: IO ()
main = activityOf initialState processEvent renderScene

