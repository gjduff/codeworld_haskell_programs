 {-# LANGUAGE OverloadedStrings #-}
-- import CodeWorld
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import qualified Data.Text as T

{----------------------------------------------------}
--         Code world emulation layer               --
{----------------------------------------------------}

type Picture = PG.Picture
data Event = KeyPress String | PointerPress (Float,Float)
            -- might have to OR this with some other stuff, like for time, mouse click


translated x y = PG.Translate (realToFrac x) (realToFrac y)
rotated n = PG.Rotate (realToFrac (n * (-1) * 180 / 3.14))
colored = PG.Color
circle n = PG.Circle (realToFrac n)  -- because gloss uses Float, not Double
solidCircle n = PG.circleSolid (realToFrac n)
solidRectangle l w = PG.rectangleSolid (realToFrac l) (realToFrac w)

lettering t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)

green = PG.green
red = PG.red
purple = PG.violet
blank = PG.blank



windowDisplay :: PG.Display
windowDisplay = PG.InWindow "Window" (640, 480) (50, 10)

updateFunc f dt state = state -- for now

eventFunc f event state = 
  case event of 
    PG.EventKey (PG.SpecialKey PG.KeyUp) PG.Down _ (x,y)   -> f (KeyPress "Up") state
    PG.EventKey (PG.SpecialKey PG.KeyDown) PG.Down _ (x,y) -> f (KeyPress "Down") state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Down _ (x,y)  -> f (PointerPress (x,y)) state
    _                                                      -> f (KeyPress "OTHER") state


renderFunc f state = PG.Scale 20 20 $ f state

activityOf initStates processEvents render = PG.play 
   windowDisplay
   PG.white
   1
   initStates
   (renderFunc render)
   (eventFunc processEvents)
   (updateFunc processEvents)

(&) f s = mappend s f



{----------------------------------------------------}
--     Paste Code world code below!!                --
{----------------------------------------------------}




{-
     All the states needed for this interactive program.
     In this case, just radii for two circles
-}
data States = States { radius1 :: Double 
                     , radius2 :: Double }


{-
     Takes a States data type which delivers two radii and creates
     a picture of two circles with those radii
-}
twoCircles :: States -> Picture
twoCircles (States r1 r2) = (translated 2 0 $ colored green $ circle r1) &
                            (translated (-2) 0 $ colored red $ circle r2)


{-
     This function will be passed to activityOf as a callback.
     when an event occurs, it will be called and it will
     "return" the updated state (the two processed radii)
-}
processEventToUpdateState :: Event -> States -> States
processEventToUpdateState event (States r1 r2) =
   case event of
     KeyPress "Up"   -> (States (r1+1) (r2-1))
     KeyPress "Down" -> (States (r1-1) (r2+1))
     _               -> States r1 r2


{-
     The initial state of the program. in this case, starting
     radii of the two circles
-}
initialStates :: States
initialStates = States 4 4


{-
     main program. activityOf is our IO action. it requires
     one param that's the initial state, then next a function that will be an event callback
     that takes event and state as a parameter and produces the new state,
     finally the picture producing function that will accept that new state and draw accordingly
-}
main :: IO ()
main = activityOf initialStates processEventToUpdateState twoCircles


         
