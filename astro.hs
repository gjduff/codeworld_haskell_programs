import CodeWorld
import qualified Data.Text as T

type Coords = (Double, Double)

data State = State {
                     getAstroCoords :: Coords
                    ,getElapsedTime :: Double
                    ,getIsJumping :: Bool
                    ,getGoingLeft :: Bool
                   }

{-
     Set the initial state of the astronaut: ie - position
-}
initialState :: State
initialState = State (9, 0) 0 False True


jumpTime :: Double
jumpTime = 20

stepSize :: Double
stepSize = jumpTime / 100

leftSide :: Double
leftSide = -10

rightSide :: Double
rightSide = 10


{-
     the astronaut is jumping so: 
         update it's coordinates
         stop jumping after a certain amount of time
         turn right after it hits left part of screen and vuce versa
-}
updateJump :: State -> State
updateJump state = State newCoords (etime+1) newIsJumping newGoingLeft
  where
    dx = if goingLeft == True then (-stepSize) else stepSize
    etime = getElapsedTime state
    acoords = getAstroCoords state
    isJumping = getIsJumping state
    goingLeft = getGoingLeft state
    newCoords = ((fst acoords + dx), abs (5*sin (pi * etime/jumpTime)) )
    newIsJumping = if etime < (jumpTime+1) then True else False
    newGoingLeft = if goingLeft == True && (fst acoords) < leftSide 
                   then False
                   else if goingLeft == False && (fst acoords) > rightSide
                        then True
                        else goingLeft


{-
     given an event and current state, compute the new state
-}  
processEvent :: Event -> State -> State
processEvent event state = 
   case event of
     PointerPress (x,y) -> State acoords etime True goingLeft
     TimePassing x      -> if isJumping == True 
                           then updateJump state
                           else State acoords 0 isJumping goingLeft
     -- TimePassing x      -> if isJumping == True  
     --                      then State (setCoordsIfJumping state) (etime+1) 
     --                                 (setIsJumpingByTime state) (setIsGoingLeft state)
     --                      else State acoords 0 isJumping goingLeft
     _                  -> state
   where
     etime = getElapsedTime state
     acoords = getAstroCoords state
     isJumping = getIsJumping state
     goingLeft = getGoingLeft state


{-
     given the current state, draw the scene, including
     an astronaut at the coordinates held in the state
-}
renderScene :: State -> Picture
renderScene state = (lettering $ T.pack xText) &
                    (translated x y $ drawAstronaut state)                                  
  where
    (x,y) = getAstroCoords state
    xText = show x
    


{-
     draw an astronaut
-}
drawAstronaut :: State -> Picture
drawAstronaut state = 
  (translated (facing * 0.56) 2.4 $ colored white $ solidCircle 0.10) &
  (translated (facing * 0.34) 2.2 $ colored black $ solidCircle 0.66) &
  (translated (facing * 0.4) 0.25 $ colored (grey) $ solidRectangle 0.5 0.6) &  
  (translated 0 2.2 $ colored (light grey) $ solidCircle 1) &
  (translated 0 0.5 $ colored (light grey) $ solidCircle 1) &
  (translated 0 0 $ colored (light grey) $ solidRectangle 2 1.2) &
  (translated 0.5 (-0.5) $ colored ( grey) $ solidCircle 0.5) &
  (translated (-0.5) (-0.5) $ colored ( grey) $ solidCircle 0.5) 
  where
    facing = if (getGoingLeft state) == True then (-1) else 1

  

{-
     start the activity
-}
main :: IO ()
main = activityOf initialState processEvent renderScene


