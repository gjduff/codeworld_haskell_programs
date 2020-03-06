import CodeWorld

type Coords = (Double, Double)

data State = State {
                    getAstroCoords :: Coords
                   }

{-
     Set the initial state of the astronaut: ie - position
-}
initialState :: State
initialState = State (9, 0)


{-
     given an event and current state, compute the new state
-}
processEvent :: Event -> State -> State
processEvent event state = state


{-
     given the current state, draw the scene, including
     an astronaut at the coordinates held in the state
-}
renderScene :: State -> Picture
renderScene state = translated x y $ drawAstronaut
  where
    (x,y) = getAstroCoords state


{-
     draw an astronaut
-}
drawAstronaut :: Picture
drawAstronaut = 
  (translated (-0.56) 2.4 $ colored white $ solidCircle 0.10) &
  (translated (-0.34) 2.2 $ colored black $ solidCircle 0.66) &
  (translated 0 2.2 $ colored (light grey) $ solidCircle 1) &
  (translated 0 0.5 $ colored (light grey) $ solidCircle 1) &
  (translated 0 0 $ colored (light grey) $ solidRectangle 2 1.2)
  

{-
     start the activity
-}
main :: IO ()
main = activityOf initialState processEvent renderScene



