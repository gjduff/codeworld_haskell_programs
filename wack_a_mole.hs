import CodeWorld
import qualified Data.Text as T

data MType = Fast | Medium | Slow deriving (Eq, Ord)

mColors :: [Color]
mColors = [orange, purple, green]

mTimeUp :: [Int]
mTimeUp = [1, 2, 3]


-- type of mole, position (0 to 8), time it pops up, is currently popped up
type Mole = (MType, Int, Double, Bool)

-- type of mole, position (0 to 8), time it pops up
levels :: [[Mole]] 
levels = [
           [ (Slow, 0, 15, False), (Slow, 1, 5, False), (Slow, 3, 20, False), (Slow, 4, 25, False), (Slow, 4, 30, False), (Slow, 5, 25, False), (Slow, 7, 10, False) ],
           [ (Slow, 0, 25, False), (Slow, 2, 10, False), (Slow, 5, 5, False), (Medium, 5, 15, False), (Slow, 8, 20, False), (Medium, 8, 30, False) ],
           [ (Slow, 0, 20, False), (Slow, 1, 5, False), (Slow, 2, 25, False), (Medium, 3, 15, False), (Slow, 4, 30, False), (Medium, 5, 10, False), (Medium, 5, 25, False), (Slow, 7, 10, False), (Slow, 8, 20, False) ]
         ]
         

--
-- update a single mole. this will be a mappable function with current time and mouse pos as the partially
-- applied args of the mapped function
--
updateMole :: Point -> Double -> Mole -> Mole
updateMole mousePos time mole@(mtype, pos, uptime, up) = 
  (mtype, pos, uptime, isUp)
  where
    isUp = if (time > uptime) && (time<(uptime+3)) 
           then True
           else False
    gotHit = False -- this will be based on mouse pos, w/ function to check mousePos with a square related to pos


data World = World { clock :: Double, tick :: Bool, currentLVL :: Int, lvlMoles :: [Mole] }

initState :: World
initState = World 0 False  0 (levels !! 0)

offsetX :: Double
offsetX = 7
offsetY :: Double
offsetY = 6

holeLocations :: [Point]
holeLocations = [ (-offsetX,offsetY*2),  (0,offsetY*2),  (offsetX,offsetY*2),
                  (-offsetX,offsetY),    (0,offsetY),    (offsetX,offsetY),
                  (-offsetX,0),          (0,0),          (offsetX,0) ]





drawAllMoles :: Picture
drawAllMoles = blank




drawHole :: Point -> Picture
drawHole point@(x,y) = translated x y $
                       (
                       translated 0 (-2.8) $ solidClosedCurve [(-3,0), (0,1), (3,0), (0,-1)]
                       )

drawMole :: Mole -> Picture
drawMole mole@(mtype, pos, uptime, up) = translated x y $
           if up==True then pic else blank
  where
    bodyColor = purple
    x = fst $ holeLocations !! pos
    y = snd $ holeLocations !! pos
    pic =  (translated 0.7 0 $ colored black $ solidCircle 0.2)       &
           (translated (-0.7) 0 $ colored black $ solidCircle 0.2)    &
           (colored bodyColor $ solidCircle 2)                        &
           (translated 0 (-1.5) $ colored bodyColor $ solidRectangle 4 3)


drawWorld :: World -> Picture
drawWorld world@(World clock _ _ moles) = scaled 0.7 0.7 $
                  (
                  blank & 
                  molesP &
                  holes & 
                  clockP &
                  blank
                  )
  where
    holes = foldr (&) blank (map drawHole holeLocations)
    molesP = foldr (&) blank (map drawMole moles)
    clockP = translated (-8) 0 $ lettering $ T.pack $ show clock
                  
                  
eventProc :: Event -> World -> World
eventProc event world@(World clock tick lvl moles) =
  case event of
     TimePassing x      -> World (clock+1) tick lvl newMoles
       where
         newMoles = map (updateMole (0,0) (clock/100)) moles 
     _                  -> world


--updateMole :: Point -> Double -> Mole -> Mole
--updateMole mousePos time mole@(mtype, pos, uptime, up) = 


main = activityOf initState eventProc drawWorld




