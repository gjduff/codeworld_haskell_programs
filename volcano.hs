{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified System.Random as R
import qualified Control.Monad as C

{-
     the state includes elapsed time, start times for all dots
     and initial and current coords and velocities for all dots
-}
data States = States {getElapsedTime :: Double, 
                      getStartTimes :: [Double],
                      getInitCoords :: [(Double,Double)],
                      getInitVelocities :: [(Double,Double)],
                      getCurCoords :: [(Double,Double)],
                      getCurVelocities :: [(Double,Double)]}


{-
     function to initialize the state
-}
initStates :: [Double] -> [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)] -> [(Double,Double)] -> States
initStates startTicks initCoords initVelocities curCoords curVelocities 
           = States 0 startTicks initCoords initVelocities curCoords curVelocities


{-
     update one dot's coordinates given current time, start time,
     current position and velocity,
     and have it go back to original coordinates after it "hits the ground"
-}
updateSingleCoord :: Double -> (Double, (Double,Double), (Double,Double), (Double,Double)) -> (Double,Double)
updateSingleCoord eTime (startTime, initCoord, curCoord, curVelocity) = (x+dx, y+dy)                      
          where
            x  = if (snd curCoord) > 0 then fst curCoord else fst initCoord
            y  = if (snd curCoord) > 0 then snd curCoord else snd initCoord
            dx = if eTime > startTime then fst curVelocity else 0
            dy = if eTime > startTime then snd curVelocity else 0
                                                       

{-
     update one dot's velocity given current time, start time,
     current position and velocity,
     have the velocity changed over time by gravity
     and have it go back to original velocity after it "hits the ground"
-}
updateSinlgeVelocity :: Double -> (Double, (Double,Double), (Double,Double), (Double,Double)) -> (Double,Double)
updateSinlgeVelocity eTime (startTick, curVelocity, curCoord, initVelocity) = (newDX, newDY)
  where
    newDX   = fst curVelocity
    newDY   = if eTime > startTick 
              then if (snd curCoord) < 0 then (snd initVelocity) else (snd curVelocity) + gravity 
              else snd curVelocity
    gravity = -0.0035




{-
     process event, in this case, just the passage of time,
     updating the state when that happens
-}
processEventToUpdateState:: Event -> States -> States
processEventToUpdateState event states = 
  case event of
    TimePassing x      ->  States (etime+1) startTimes iCoords iVelos updatedCoords updatedVelos
    _                  ->  States etime startTimes iCoords iVelos cCoords cVelos
    where
      etime  = getElapsedTime states
      tick   = round etime `mod` 20 == 19
      
      iCoords = getInitCoords states
      iVelos  = getInitVelocities states
      cCoords = getCurCoords states
      cVelos  = getCurVelocities states
      startTimes = getStartTimes states
      
      updatedCoords = map (updateSingleCoord etime)  ( zip4a startTimes iCoords cCoords cVelos ) 
      updatedVelos  = map (updateSinlgeVelocity etime) ( zip4a startTimes cVelos cCoords iVelos )
      

{-
     This function zips up four lists into a list of 4-tuples
-}
zip4a :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4a list1 list2 list3 list4 = t3
  where
    t1 = zip list1 list2
    t2 = zipWith (\(x,y) z -> (x,y,z)) t1 list3
    t3 = zipWith (\(w,x,y) z -> (w,x,y,z)) t2 list4


{-
     draw the volcano and all the lava for the lava/dot
     coordinates, which are obtained from the current state
-}
drawState :: [Int] -> States -> Picture
drawState dColors states = translated (-7) (-7) $ 
                   (
                    blank &
                    drawVolcano &
                    foldr (&) blank (map drawDot (zip dColors (getCurCoords states)) ) &
                    
                    blank
                    )


{-
     draw a yellow, orange or red dot at the coordinate passed in
-}
drawDot :: (Int, (Double,Double)) -> Picture
drawDot (c,(x,y)) = translated x y $ colored calcColor $ solidCircle (0.25+dotGrow)
  where
    dotGrow   = if y<11 then 0 else (y/8) * (0.15)
    calcColor = case c of
                0 -> orange
                1 -> yellow
                2 -> red
                _ -> green


{-
     draw the shape of a volcano
-}
drawVolcano :: Picture
drawVolcano = (colored brown $ solidPolygon [(-1,-1),(17,-1),(12,9),(4,9)])


{-
     number of dots representing the lava in the animation
-}
numDots :: Int
numDots = 150

{-
     start location on a axis. these are at y=0
-}
startXs :: IO [Double]
startXs = C.replicateM numDots $ R.randomRIO (0,32)

{-
     "destination" location on x axis. these are at y=10.
     this is just so we can get a proper velocity vector to
     shoot out of the volcano
-}
destXs :: IO [Double]
destXs = C.replicateM numDots $ R.randomRIO (5,11)


{-
     dots/lava need various start times. if they all go at the
     same time, it looks like a row of dots, not a natural/chaotic
     way
-}
startTimes :: IO [Double]
startTimes = C.replicateM numDots $ R.randomRIO (0,200)


{-
     create a velocity vector (dx, dy) using a start x
     and a destination x. The start y is always 0
     and the dest y is always 10, so can just use a
     constant for dy
-}
calcInitVelocity :: (Double, Double) -> (Double, Double)
calcInitVelocity (startx, destx) = ( (destx-startx)/30 , 0.33 )


{-
     numbers to represent a random assortment of three colors
-}
dotColors :: IO [Int] 
dotColors = C.replicateM numDots $ R.randomRIO (0,2)


{-
     main program. gather up the random data to initialize the
     state with, then start the activity
-}
main = do 
  sx <- startXs
  dx <- destXs
  st <- startTimes
  dc <- dotColors
  let scaledSX = (map (*0.5) sx)
  let ic = (zip scaledSX $ replicate numDots 0)
  let iv = (map calcInitVelocity (zip scaledSX dx))
  
  activityOf (initStates st ic iv ic iv)
             processEventToUpdateState
             (drawState dc)
             




