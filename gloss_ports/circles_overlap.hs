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
yellow = PG.yellow
grey = PG.greyN 0.5
brown = PG.makeColorI 185 122 87 255
pink = PG.makeColorI 255 174 201 255
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
                     getCircleCoords :: [Coords]
                    ,getCircleColors :: [Color]
                    ,getCircleDragged :: [Bool]
                    ,getMouseCoords :: Point 
                    ,debug :: String
                   }
                
circleRadius :: Double  
circleRadius = 2
-- 34  177   76
-- 
redX    = (rgba 1.0  0.0  0.0  0.6)
blueX   = (rgba (-0.4)  (-0.2)  0.6  0.4)
yellowX = (rgba 0.9  0.9  0.0  0.5)


{-
     Set the initial state of the astronaut: ie - position
-}
initialState :: State
initialState = State [(0,0),(2,0),(0,3)] [redX, blueX, yellowX]
                     [False, False, False] (0,0) ""


{-
     checks if mouse pointer within a circle
-}
checkMouseOverlaps :: (Point, Point) -> Bool
checkMouseOverlaps ((cx,cy), (mx,my)) =
  if hyp < circleRadius then True else False
  where 
    hyp = sqrt( (cx-mx)^2 + (cy-my)^2 )




{-
     if dragged by mouse is true, make circle coords same as mouse's
     otherwise keep circle coords the same 
-}
moveWithMouse :: (Point, Point, Bool) -> Point
moveWithMouse ((mx, my), (cx, cy), dragged) = if dragged 
                                          then (mx, my) 
                                          else (cx, cy)


{-
     given an event and current state, compute the new state
-}
processEvent :: Event -> State -> State
processEvent event state =
  case event of 
    PointerRelease (x,y) -> State circleCoords circleColors allFalse (x,y) "release"
         where
           allFalse = (replicate numCircles False)
    PointerPress (x,y) -> State circleCoords circleColors circleDraggeds (x,y) ("press " ++ msg)
         where
           r1 = checkMouseOverlaps ((circleCoords !! 0), (x,y))
           b1 = not r1 && checkMouseOverlaps ((circleCoords !! 1), (x,y))
           y1 = not r1 && not b1 && checkMouseOverlaps ((circleCoords !! 2), (x,y))
           circleDraggeds = [r1, b1, y1]
           msg = "mouse:" ++ (show (x,y)) ++ "  circles:" ++ (show circleCoords)
    PointerMovement (x,y) -> State newCircleCoords circleColors circleDraggedsWM (x,y) "move"
         where
           newCircleCoords = map moveWithMouse (zip3a mouseRepl circleCoords circleDraggedsWM)
           mouseRepl = replicate numCircles (x,y)
    _   -> state
  where
    circleCoords = getCircleCoords state
    circleColors = getCircleColors state
    circleDraggedsWM = getCircleDragged state
    numCircles = length circleCoords
    
{-
     This function zips up three lists into a list of 3-tuples
-}
zip3a :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3a list1 list2 list3 = t2
  where
    t1 = zip list1 list2
    t2 = zipWith (\(x,y) z -> (x,y,z)) t1 list3
    

{-
     draw circles according to the color and coordinates
     as obtained from the program state
-}
renderScene :: State -> Picture
renderScene state = (translated (-10) 0 $ lettering $ T.pack $ debug state) & circles
  where
    circleCoords = getCircleCoords state
    circleColors = getCircleColors state
    circles = foldr (&) blank (map drawCircle (zip circleCoords circleColors) )


{-
     draw a circle at a given point with a given color
-}
drawCircle :: (Point, Color) -> Picture
drawCircle ((x,y), c) = translated x y $ colored ( c) $ 
                     solidCircle circleRadius




{-
     start the activity
-}
main :: IO ()
main = activityOf initialState processEvent renderScene

