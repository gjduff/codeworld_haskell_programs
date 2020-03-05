{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified System.Random as R
import qualified Control.Monad as C


{-
     Initial state of the dice
-}
initStates :: Int
initStates = 0


{-
     handle key press event given the event and the current state.
     result will be the next state
     first parameter is the length of the random rolls list which
     gets passed using partial application, because activityOf
     expects an Event as the first param
-}
eventHandler :: Int -> Event -> Int -> Int
eventHandler rollsLen event diceState = 
   case event of
     PointerPress (x,y) -> if diceState == rollsLen then 0 else diceState + 1
     KeyPress "Up"   -> if diceState == rollsLen then 0 else diceState + 1
     _               -> diceState
     -- KeyPress "Down" -> if diceState == 0 then rollsLen else diceState - 1


{-
     The coordinates of the little dots on the dice for each side
     
     (    0),(    0)  mid mid
     (-1.25),(    0)  mid left           ( 1.25),(    0)  mid right
     (-1.25),( 1.25)  top left           ( 1.25),( 1.25)  top right 
     (-1.25),(-1.25)  bottom left        ( 1.25),(-1.25)  bottom right
-}
dice1 = [(0,0)]
dice2 = [((-1.25), (1.25)), ((1.25), (-1.25))]
dice3 = [(0,0), ((-1.25), (1.25)), ((1.25), (-1.25))]
dice4 = [((-1.25), (1.25)), ((1.25), (-1.25)), ((1.25), (1.25)), ((-1.25), (-1.25))]
dice5 = [(0,0), ((-1.25), (1.25)), ((1.25), (-1.25)), ((1.25), (1.25)), ((-1.25), (-1.25))]
dice6 = [((-1.25), (1.25)), ((1.25), (-1.25)), ((1.25), (1.25)), ((-1.25), (-1.25)), ((-1.25), (0)), ((1.25), (0))]

allDice = [dice1,dice2,dice3,dice4,dice5,dice6]
diceColors = [green,red,blue,yellow,orange,purple]


{-
     Translate a a picture by coordinates found in a tuple
-}
tupTrans :: Picture -> (Double,Double) -> Picture
tupTrans pic coords = translated (fst coords) (snd coords) pic


{-
     given the current state, draw the corresponding dice.
     first parameter is the random rolls list which
     gets passed using partial application, because activityOf
     expects a function with just one parameter (the state)
-}
newStatePicture :: [Int] -> [Int] -> Int -> Picture
newStatePicture rolls rolls2 diceState = 
  translated 4.5 0 $ ( dots1 & (colored currentColor1 $ solidRectangle 4 4) ) &
  translated (-4.5) 0 $ ( dots2 & (colored currentColor2 $ solidRectangle 4 4) )
  where
    dots1 = foldl (&) blank (map (tupTrans (solidCircle 0.25) ) (allDice !! (rolls !! diceState)) )
    currentColor1 = diceColors !! (rolls !! diceState)
    dots2 = foldl (&) blank (map (tupTrans (solidCircle 0.25) ) (allDice !! (rolls2 !! diceState)) )
    currentColor2 = diceColors !! (rolls2 !! diceState)


{-
     generate a list of random numbers between zero and five
-}
rollDice :: IO [Int]
rollDice = C.replicateM 10000 $ R.randomRIO (0,5)


{-
     capture the list of random numbers and its length
     to pass as partial application parameters to arguments of 
     eventHandler and newStatePicture
     since activityOf expects certain type signature functions as
     its arguments, but we need to shoehorn the random numbers in 
     there.
     
     using those, start the activity
-}
main :: IO ()
main = do 
  rolls <- rollDice
  rolls2 <- rollDice
  let numRolls = (length rolls) - 1
  print $ map (+1) $ take 20 rolls
  activityOf initStates (eventHandler numRolls) (newStatePicture rolls rolls2)


