 {-# LANGUAGE OverloadedStrings #-}
import CodeWorld


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


         

