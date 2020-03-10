 {-# LANGUAGE OverloadedStrings #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
     EventKey (SpecialKey KeyUp) Down _ (x,y)   -> if diceState == 5 then 0 else diceState + 1
     EventKey (SpecialKey KeyDown) Down _ (x,y) -> if diceState == 0 then 5 else diceState - 1
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
tupTrans :: Picture -> (Float,Float) -> Picture
tupTrans pic coords = Translate (fst coords) (snd coords) pic


{-
     given the current state, draw the corresponding dice
-}
newStatePicture :: Int -> Picture
newStatePicture diceState = Scale 20 20 $
  mappend (Color green $ rectangleSolid 4 4) dots
  where
    dots = foldl (mappend) blank (map (tupTrans (ThickCircle 0.25 0.25) ) (allDice !! diceState) )


{-
     Main program. just start the activity
-}
--main :: IO ()
--main = activityOf initStates eventHandler newStatePicture



--updateFunc :: Float -> State -> State
updateFunc :: Float -> Int -> Int
updateFunc dt state = state 


main :: IO ()
main = activityOf
  newStatePicture
  eventHandler
  updateFunc


activityOf :: (Int -> Picture) -> (Event -> Int -> Int) -> 
              (Float -> Int -> Int) -> IO ()
activityOf = play 
   windowDisplay
   white
   1
   initStates


windowDisplay :: Display
windowDisplay = InWindow "Window" (640, 480) (50, 10)
