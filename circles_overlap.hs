import CodeWorld

type Coords = (Double, Double)


data State = State {
                     getCircleCoords :: [Coords]
                    ,getCircleColors :: [Color]
                    ,getCircleDragged :: [Bool]
                    ,getIsMousePressed :: Bool 
                   }
                   
numCircles :: Int
numCircles = 3
                   

{-
     Set the initial state of the astronaut: ie - position
-}
initialState :: State
initialState = State [(-3,0),(3,0),(0,3)] [red, blue, yellow]
                     [False, False, False] False


{-
     given an event and current state, compute the new state
-}
processEvent :: Event -> State -> State
processEvent event state =
  case event of 
    PointerRelease (x,y) -> state
    PointerPress (x,y) -> state
    _   -> state


{-
     given the current state, draw the scene, including
     an astronaut at the coordinates held in the state
-}
renderScene :: State -> Picture
renderScene state = circles
  where
    coords = getCircleCoords state
    colors = getCircleColors state
    circles = foldr (&) blank (map drawCircle (zip coords colors) )


{-
     draw a circle
-}
drawCircle :: (Point, Color) -> Picture
drawCircle ((x,y), c) = translated x y $ colored c $ 
                     solidCircle 1





{-
     start the activity
-}
main :: IO ()
main = activityOf initialState processEvent renderScene



