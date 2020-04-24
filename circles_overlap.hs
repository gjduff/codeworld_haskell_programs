import CodeWorld
import qualified Data.List as L

type Coords = (Double, Double)


data State = State {
                     getCircleCoords :: [Coords]
                    ,getCircleColors :: [Color]
                    ,getCircleDragged :: Maybe Int
                    ,getMouseCoords :: Point 
                   }
                
circleRadius :: Double  
circleRadius = 2
-- 34  177   76
-- 
redX    = (RGBA 1.0  0.0  0.0  0.6)
blueX   = (RGBA (-0.4)  (-0.2)  0.6  0.4)
yellowX = (RGBA 0.9  0.9  0.0  0.5)


{-
     Set the initial state of the circles
-}
initialState :: State
initialState = State (take 12 $ cycle [(0,0),(2,0),(0,3)]) (take 12 $ cycle [redX, blueX, yellowX])
                     Nothing (0,0)


{-
     checks if mouse pointer within a circle
-}
checkMouseOverlaps :: Point -> Point -> Bool
checkMouseOverlaps (mx,my) (cx,cy) =
  if hyp < circleRadius then True else False
  where 
    hyp = sqrt( (cx-mx)^2 + (cy-my)^2 )


{-
     if dragged by mouse is true, make circle coords same as mouse's
     otherwise keep circle coords the same 
-}
moveWithMouse :: Maybe Int -> (Point, Point, Int) -> Point
moveWithMouse draggedIndex ((mx, my), (cx, cy), index) = 
                            if draggedIndex == (Just index) 
                            then (mx, my)
                            else (cx,cy)



{-
     given an event and current state, compute the new state
-}
processEvent :: Event -> State -> State
processEvent event state =
  case event of 
    PointerRelease (x,y) -> State circleCoords circleColors Nothing (x,y)
         where
           allFalse = (replicate numCircles False)
    PointerPress (x,y) -> State circleCoords circleColors draggedIndex (x,y)
         where
           lst = map (checkMouseOverlaps (x,y)) circleCoords
           draggedIndex = L.elemIndex True lst
    PointerMovement (x,y) -> State newCircleCoords circleColors circleDraggedInd (x,y)
         where
           newCircleCoords = map (moveWithMouse circleDraggedInd) (L.zip3 mouseRepl circleCoords indices)
           mouseRepl = replicate numCircles (x,y)
    _   -> state
  where
    circleCoords = getCircleCoords state
    circleColors = getCircleColors state
    circleDraggedInd = getCircleDragged state
    numCircles = length circleCoords
    indices = [0..(numCircles-1)]

    
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


