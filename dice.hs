 {-# LANGUAGE OverloadedStrings #-}
import CodeWorld

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
