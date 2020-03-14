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
     Initial state of the dice
-}
initStates :: Int
initStates = 1


{-
     handle key press event given the event and the current state.
     result will be the next state
-}
eventHandler :: Event -> Int -> Int
eventHandler event diceState = 
   case event of
     KeyPress "Up"   -> if diceState == 5 then 0 else diceState + 1
     KeyPress "Down" -> if diceState == 0 then 5 else diceState - 1
     _               -> diceState


{-
     The coordinates of the little dots on the dice for each side
-}
-- (    0),(    0)  mid mid
-- (-1.25),(    0)  mid left           ( 1.25),(    0)  mid right
-- (-1.25),( 1.25)  top left           ( 1.25),( 1.25)  top right 
-- (-1.25),(-1.25)  bottom left        ( 1.25),(-1.25)  bottom right
dice1 = [(0,0)]
dice2 = [((-1.25), (1.25)), ((1.25), (-1.25))]
dice3 = [(0,0), ((-1.25), (1.25)), ((1.25), (-1.25))]
dice4 = [((-1.25), (1.25)), ((1.25), (-1.25)), ((1.25), (1.25)), ((-1.25), (-1.25))]
dice5 = [(0,0), ((-1.25), (1.25)), ((1.25), (-1.25)), ((1.25), (1.25)), ((-1.25), (-1.25))]
dice6 = [((-1.25), (1.25)), ((1.25), (-1.25)), ((1.25), (1.25)), ((-1.25), (-1.25)), ((-1.25), (0)), ((1.25), (0))]

allDice = [dice1,dice2,dice3,dice4,dice5,dice6]


{-
     Translate a a picture by coordinates found in a tuple
-}
tupTrans :: Picture -> (Double,Double) -> Picture
tupTrans pic coords = translated (fst coords) (snd coords) pic


{-
     given the current state, draw the corresponding dice
-}
newStatePicture :: Int -> Picture
newStatePicture diceState = 
  dots & (colored green $ solidRectangle 4 4)
  where
    dots = foldl (&) blank (map (tupTrans (solidCircle 0.25) ) (allDice !! diceState) )


{-
     Main program. just start the activity
-}
main :: IO ()
main = activityOf initStates eventHandler newStatePicture





