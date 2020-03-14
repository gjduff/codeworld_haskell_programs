 {-# LANGUAGE OverloadedStrings #-}
-- import CodeWorld
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import qualified Data.Text as T
import qualified Data.List as D
import qualified System.Random as R
import qualified Control.Monad as C


{----------------------------------------------------}
--         Code world emulation layer               --
{----------------------------------------------------}

import Prelude hiding (Double)
type Double = Float       -- it's hard to believe I can do this

type Picture = PG.Picture
data Event = KeyPress String | PointerPress (Float,Float) | TimePassing Float
            -- might have to OR this with some other stuff, like for time, mouse click


scaled l w = PG.Scale (realToFrac l) (realToFrac w)
translated x y = PG.Translate (realToFrac x) (realToFrac y)
rotated n = PG.Rotate (realToFrac (n * (-1) * 180 / 3.14))
colored = PG.Color

circle n = PG.Circle (realToFrac n)  -- because gloss uses Float, not Double
solidCircle n = PG.circleSolid (realToFrac n)
solidRectangle l w = PG.rectangleSolid (realToFrac l) (realToFrac w)

data Monospace = Monospace
data Bold = Bold
styledLettering style font t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)
lettering t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)

polygon = PG.lineLoop
solidPolygon p = PG.polygon $ (tail p) ++ [(head p)]

type Color = PG.Color

green = PG.green
red = PG.red
blue = PG.blue
purple = PG.violet
blank = PG.blank
orange = PG.orange
black = PG.black
yellow = PG.yellow
grey = PG.greyN 0.5
brown = PG.makeColorI 185 122 87 255


windowDisplay :: PG.Display
windowDisplay = PG.InWindow "Window" (640, 480) (50, 10)

updateFunc f dt state =  
    f (TimePassing dt) state

eventFunc f event state = 
  case event of 
    PG.EventKey (PG.Char chr) PG.Down _ (x,y)   -> f (KeyPress [chr]) state
    PG.EventKey (PG.SpecialKey PG.KeyUp) PG.Down _ (x,y)   -> f (KeyPress "Up") state
    PG.EventKey (PG.SpecialKey PG.KeyDown) PG.Down _ (x,y) -> f (KeyPress "Down") state
    PG.EventKey (PG.SpecialKey PG.KeyLeft) PG.Down _ (x,y) -> f (KeyPress "Left") state
    PG.EventKey (PG.SpecialKey PG.KeyRight) PG.Down _ (x,y) -> f (KeyPress "Right") state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Down _ (x,y)  -> f (PointerPress (x,y)) state
    _                                                      -> f (KeyPress "OTHER") state


renderFunc f state = PG.Scale 20 20 $ f state

activityOf initStates processEvents render = PG.play 
   windowDisplay
   PG.white
   30
   initStates
   (renderFunc render)
   (eventFunc processEvents)
   (updateFunc processEvents)

(&) f s = mappend s f



{----------------------------------------------------}
--     Paste Code world code below!!                --
{----------------------------------------------------}




data Mode = START | BLASTOFF
data States = States { getYOffset :: Double, 
                       getMode :: Mode,
                       getCurCoords :: [(Double,Double)],
                       getVelocity :: [(Double,Double)],
                       getStartTimes :: [Double],
                       getElapsedTime :: Double,
                       getColorIndexes :: [Int]}

initialStates :: [(Double,Double)] -> [Double] -> [Int] -> States
initialStates velos starts colorIndexes = States 0 START (replicate numDots (3,2.5)) velos starts 0 colorIndexes


{-
     event process functions.
     each mode has it's own function doing something
     based on a timed event or a user event, like a
     mouse click/tap
-}
processEvent :: Event -> States -> States
processEvent event states = 
    case getMode states of
      START    -> processEventStart event states
      BLASTOFF -> processEventBlastoff event states

processEventStart :: Event -> States -> States
processEventStart event states =
    case event of
      PointerPress (x,y) -> States yOffset BLASTOFF curCoords velocity startTimes etime cInds
      _                  -> states
    where
      yOffset = getYOffset states
      curCoords = getCurCoords states
      velocity = getVelocity states
      startTimes = getStartTimes states
      etime = getElapsedTime states
      cInds = getColorIndexes states

-- function to map cur coord to new coord given start time and velocity
-- if start time has elapsed
-- so need as params:      cur_coord, velocity, time, start_time
-- will be mapped over
calcNewCoord :: ((Double, Double), (Double, Double), Double, Double) -> (Double, Double)
calcNewCoord (curCoord, velocity, time, startTime) = 
  -- (nx,ny)
  if time > startTime then (nx,ny) else (x,y)
  where
    x = fst curCoord
    y = snd curCoord
    dx = fst velocity
    dy = snd velocity
    nx = x + dx
    ny = y + dy
    

{-
     This function zips up four lists into a list of 4-tuples
-}
zip4a :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4a list1 list2 list3 list4 = t3
  where
    t1 = zip list1 list2
    t2 = zipWith (\(x,y) z -> (x,y,z)) t1 list3
    t3 = zipWith (\(w,x,y) z -> (w,x,y,z)) t2 list4
    

processEventBlastoff :: Event -> States -> States
processEventBlastoff event states =
    case event of
      TimePassing x -> States (yOffset+0.17) BLASTOFF newCoords velocity startTimes (etime+1) cInds
                       where
                          newCoords = map calcNewCoord (zip4a curCoords velocity curTimes startTimes) 
                          yOffset = getYOffset states
                          curCoords = getCurCoords states
                          velocity = getVelocity states
                          startTimes = getStartTimes states
                          etime = getElapsedTime states
                          curTimes = (replicate numDots etime)
                          cInds = getColorIndexes states
      _             -> states


      


{-
     The render functions draw pictures on the scene.
     each mode has it's own function for doing so
-}
renderScene :: States -> Picture
renderScene states = 
    case getMode states of
      START    -> renderSceneStart states
      BLASTOFF -> renderSceneBlastoff states
    
renderSceneStart :: States -> Picture
renderSceneStart states =   scaled 0.6 0.6 $ translated (-3) (-14) $ 
     drawRocket


colorLookup :: [Color]
colorLookup = [orange, brown, red]

-- function to draw exhaust (circles) if the time is right
-- so need as params:      cur_coord, time, start time
-- will be mapped over
drawBall :: ((Double, Double), Int) -> Picture
drawBall (curCoord, colorInd) = translated (fst curCoord) (snd curCoord) $ colored ballColor $ solidCircle 0.5
  where
    ballColor = colorLookup !! colorInd

{-
     This function zips up three lists into a list of 3-tuples
-}
zip3a :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3a list1 list2 list3 = t2
  where
    t1 = zip list1 list2
    t2 = zipWith (\(x,y) z -> (x,y,z)) t1 list3


renderSceneBlastoff :: States -> Picture
renderSceneBlastoff states =   

     scaled 0.6 0.6 $ translated (-3) (-14) $ 
     (colored green $ lettering "BLASTOFF!") &
     (
     translated 0 yOffset $ 
     
     drawRocket &
     foldr (&) blank ( map drawBall (zip curCoords cInds) ) &
     drawBallX
     
     )
     where
       yOffset = getYOffset states
       curCoords = getCurCoords states
       cInds = getColorIndexes states
       

{-
   functions for drawing individual items
-}
drawRocket :: Picture 
drawRocket = colored grey $ solidPolygon [(1,1), (2,3), (2,8), (3,10), (4,8), (4,3), (5,1)]

drawBallX :: Picture
drawBallX = translated 3 1.5 $ colored orange $ solidCircle 0.5


{-
     number of dots representing the rocket exhaust in the animation
-}
numDots :: Int
numDots = 250

{-
     "destination" location on x axis. these are at y=?.
     this is just so we can get a proper velocity vector to
     shoot out of the volcano
-}
genDestXs :: IO [Double]
genDestXs = C.replicateM numDots $ R.randomRIO (1.0,4.5)

calcVelocity :: Double -> (Double,Double)
calcVelocity destx = (dx,dy)
  where
    dx = (destx - 3)/10
    dy = (-4 - 1.5 )/10

genStartTimes :: IO [Double]
genStartTimes = C.replicateM numDots $ R.randomRIO (1,300)

genColorIndexes :: IO [Int]
genColorIndexes = C.replicateM numDots $ R.randomRIO (0,2)


{-
     main program starts the activity
-}
main = do
  startTimes <- genStartTimes
  destXs  <- genDestXs
  colorIndexes <- genColorIndexes
  let velos = map calcVelocity destXs
  activityOf (initialStates velos startTimes colorIndexes) processEvent renderScene