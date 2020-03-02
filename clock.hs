 {-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as T


{-
     state is the angle of the minute hand, the hour hand, and the
     minute hand round trip count
-}
data States = States {getAngle :: Double, getAngle2 :: Double, getMinHandCount :: Double}


{-
     draw the clock tick 5 minute marks in a circle around the clock
-}
tick :: Picture
tick = (translated 7 0 $ solidCircle 0.2)

rotate12 :: Picture -> Double -> Picture
rotate12 pic inc = rotated ((6.28/12)*inc) pic

ticks :: Picture
ticks = foldr (&) blank (map (rotate12 tick) [0,1,2,3,4,5,6,7,8,9,10,11])


{-
     draw the clock numbers 1 to 12 in a circle around the clock
-}
clockNum :: T.Text -> Picture
clockNum t = lettering $ t

rotateT :: (Double, Picture) -> Picture
rotateT (inc, pic) = rotated ((6.28/12)*inc) $ translated 0 8 pic

clockNums :: Picture
clockNums = foldr (&) blank (map rotateT theTuples)
  where
    theNums = map clockNum [ "12", "11", "10",  "9",  "8",  "7", 
                             "6", "5", "4", "3", "2", "1"  ]
    theTuples = zip [0,1,2,3,4,5,6,7,8,9,10,11] theNums


{-
     initial state of the clock. no angles for the hands or minute hand
     round trip counts
-}
initialState :: States
initialState = States 0 0 0


{-
     every time user clicks the mouse button, the state must be updated 
     such that the minute hand moves to next clock number. 
     any time the minute hand does a full round trip, the hour hand will 
     move to its next clock number. 
-}
eventProc :: Event -> States -> States
eventProc event states =
  let 
    ang = getAngle states
    ang2 = if (getMinHandCount states) == 11
           then getAngle2 states + (-6.28/12)
           else getAngle2 states
    minHandCount = if (getMinHandCount states) == 12
                   then 0 
                   else getMinHandCount states
  in
  case event of
    PointerPress (x,y) -> if ang <= -6 
                            then States (-6.28/12) ang2 (minHandCount+1)
                            else States (ang + (-6.28/12)) ang2 (minHandCount+1)
    _                  -> states
    

{-
     draw the minute hand, rotated by the passed in angle
-}
minuteHand :: Double -> Picture
minuteHand ang =  rotated ang $
                  colored green  $ (
                                     (translated 0 2 $ solidRectangle 0.5 5) &
                                     (translated 0 7 $ solidCircle 0.2)
                                   )
         
         
{-
     draw the hour hand, rotated by the passed in angle
-}
hourHand :: Double -> Picture
hourHand ang =  rotated ang $
                  colored purple $ (
                                     (translated 0 0.75 $ solidRectangle 0.5 2.5) &
                                     (translated 0 7 $ solidCircle 0.2)
                                   )
    

{-
     draw the whole clock based on the current state
-}
renderState :: States -> Picture
renderState states =  clockNums &
                      (circle 8.5) &
                      (circle 7.5) &
                      (hourHand ang2) &
                      (minuteHand ang) &
                      ticks
  where
    ang = getAngle states
    ang2 = getAngle2 states


{-
     start the clock activity
-}
main :: IO ()
main = activityOf initialState eventProc renderState



