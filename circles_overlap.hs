import CodeWorld

type Coords = (Double, Double)


data State = State {
                     getCircleCoords :: [Coords]
                    ,getCircleColors :: [Color]
                    ,getCircleDragged :: [Bool]
                    ,getMouseCoords :: Point 
                   }
                
circleRadius :: Double  
circleRadius = 2


{-
     Set the initial state of the astronaut: ie - position
-}
initialState :: State
initialState = State [(0,0),(2,0),(0,3)] [(RGBA 1 0 0 0.6), (RGBA 1 1 0 0.6), (RGBA 0 0 0.8 0.6)]
                     [False, False, False] (0,0)


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
    PointerRelease (x,y) -> State circleCoords circleColors allFalse (x,y)
         where
           allFalse = (replicate numCircles False)
    PointerPress (x,y) -> State circleCoords circleColors circleDraggeds (x,y)
         where
           circleDraggeds = map checkMouseOverlaps 
                           (zip circleCoords mouseRepl)
           mouseRepl = replicate numCircles (x,y)
    PointerMovement (x,y) -> State newCircleCoords circleColors circleDraggedsWM (x,y)
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
renderScene state = circles
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



