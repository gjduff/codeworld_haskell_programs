import CodeWorld
import qualified Data.Text as T

data MType  = Fast | Medium | Slow deriving (Eq, Ord, Enum)
data GState = PLAYING | BETWEEN


mColors :: [Color]
mColors = [orange, purple, green]

mDurationUp :: [Double]
mDurationUp = [0.5, 1.0, 1.5]


-- define a mole as a position (0 to 8), time it pops up, 
-- duration to stay up, and whether it *is* currently popped up
type Mole = (MType, Int, Double, Bool, Bool, String)

-- type of mole, position (0 to 8), time it pops up
levels :: [[Mole]] 
levels = [
           [ (Slow, 0, 25, False, False, ""), (Slow, 1, 17, False, False, ""), (Slow, 2, 3, False, False, ""), (Slow, 2, 10, False, False, ""), (Slow, 3, 1, False, False, ""), (Slow, 3, 17, False, False, ""), (Slow, 3, 29, False, False, ""), (Slow, 4, 7, False, False, ""), (Slow, 4, 14, False, False, ""), (Slow, 4, 23, False, False, ""), (Slow, 5, 5, False, False, ""), (Medium, 5, 15, False, False, ""), (Slow, 5, 21, False, False, ""), (Slow, 5, 29, False, False, ""), (Slow, 6, 27, False, False, ""), (Slow, 7, 12, False, False, ""), (Slow, 8, 12, False, False, ""), (Slow, 8, 20, False, False, ""), (Slow, 8, 25, False, False, ""), (Medium, 8, 30, False, False, "") ],
           [ (Slow, 0, 7, False, False,""), (Medium, 0, 20, False, False,""), (Slow, 1, 3, False, False,""), (Slow, 1, 25, False, False,""), (Slow, 1, 29, False, False,""), (Medium, 2, 5, False, False,""), (Slow, 3, 12, False, False,""), (Slow, 3, 14, False, False,""), (Slow, 3, 17, False, False,""), (Slow, 3, 23, False, False,""), (Slow, 3, 29, False, False,""), (Slow, 4, 14, False, False,""), (Medium, 4, 25, False, False,""), (Slow, 6, 1, False, False,""), (Slow, 6, 17, False, False,""), (Medium, 6, 30, False, False,""), (Medium, 7, 15, False, False,""), (Slow, 7, 21, False, False,""), (Slow, 7, 27, False, False,""), (Medium, 8, 10, False, False,"") ],
           [ (Medium, 0, 5, False, False,""), (Medium, 0, 14, False, False,""), (Slow, 0, 23, False, False,""), (Slow, 1, 17, False, False,""), (Medium, 1, 20, False, False,""), (Slow, 2, 12, False, False,""), (Slow, 2, 17, False, False,""), (Medium, 4, 3, False, False,""), (Medium, 4, 15, False, False,""), (Slow, 4, 21, False, False,""), (Slow, 4, 29, False, False,""), (Slow, 5, 25, False, False,""), (Medium, 6, 25, False, False,""), (Slow, 7, 7, False, False,""), (Medium, 7, 30, False, False,""), (Slow, 8, 1, False, False,""), (Medium, 8, 10, False, False,""), (Slow, 8, 14, False, False,""), (Medium, 8, 27, False, False,""), (Slow, 8, 29, False, False,"") ],
           [ (Slow, 0, 1, False, False,""), (Slow, 0, 21, False, False,""), (Slow, 1, 23, False, False,""), (Slow, 2, 25, False, False,""), (Slow, 2, 29, False, False,""), (Slow, 3, 12, False, False,""), (Fast, 4, 5, False, False,""), (Slow, 4, 14, False, False,""), (Medium, 4, 30, False, False,""), (Medium, 5, 3, False, False,""), (Fast, 5, 15, False, False,""), (Slow, 5, 17, False, False,""), (Fast, 5, 27, False, False,""), (Slow, 5, 29, False, False,""), (Slow, 6, 17, False, False,""), (Medium, 7, 10, False, False,""), (Medium, 7, 14, False, False,""), (Slow, 8, 7, False, False,""), (Medium, 8, 20, False, False,""), (Medium, 8, 25, False, False,"") ],
           [ (Fast, 0, 5, False, False,""), (Slow, 0, 21, False, False,""), (Slow, 1, 1, False, False,""), (Slow, 1, 12, False, False,""), (Medium, 1, 20, False, False,""), (Slow, 1, 29, False, False,""), (Medium, 2, 14, False, False,""), (Fast, 2, 27, False, False,""), (Slow, 3, 29, False, False,""), (Medium, 4, 3, False, False,""), (Medium, 4, 10, False, False,""), (Slow, 4, 23, False, False,""), (Slow, 5, 14, False, False,""), (Fast, 6, 15, False, False,""), (Slow, 6, 17, False, False,""), (Slow, 6, 25, False, False,""), (Slow, 7, 7, False, False,""), (Slow, 7, 17, False, False,""), (Medium, 7, 30, False, False,""), (Medium, 8, 25, False, False,"") ],
           [ (Slow, 0, 20, False, False, ""), (Slow, 1, 5, False, False, ""), (Slow, 2, 25, False, False, ""), (Medium, 3, 15, False, False, ""), (Slow, 4, 30, False, False, ""), (Medium, 5, 10, False, False, ""), (Medium, 5, 25, False, False, ""), (Slow, 7, 10, False, False, ""), (Slow, 8, 20, False, False, "") ]
         ]
     
maxLevel :: Int
maxLevel = length levels

maxTime :: Double
maxTime = 35   -- 6   -- for quick test of levels, set to 6

         
-- The data of the game world
data World = World { clock :: Double, 
                     currentLVL :: Int, 
                     lvlMoles :: [Mole], 
                     message :: String,
                     gameScore :: Int,
                     gameState :: GState}


--
-- The initial state of the game world
--
initState :: World
initState = World 0 0 (levels !! 0) "" 0 BETWEEN

--
-- The holes and moles need to be a certain distance from each other
--
offsetX :: Double
offsetX = 7
offsetY :: Double
offsetY = 6

-- Nine holes arranged as a square
holeLocations :: [Point]
holeLocations = [ 
                  (-offsetX,offsetY),    (0,offsetY),    (offsetX,offsetY),
                  (-offsetX,0),          (0,0),          (offsetX,0),
                  (-offsetX,-offsetY),    (0,-offsetY),    (offsetX,-offsetY)
                ]

hdTopLeft :: (Double, Double)
hdTopLeft  = ((-2.0),2.0)

hdBotRight :: (Double, Double)
hdBotRight = (2.0,(-3.0))


--
-- draw one hole at a location
--
drawHole :: Point -> Picture
drawHole point@(x,y) = translated x y $
                       (
                         (translated 0 (-2.8) $ solidClosedCurve [(-3,0), (0,1), (3,0), (0,-1)]  )
                       )



--
-- draw bounding box
--
drawBox :: Point -> Picture
drawBox point@(x,y) = translated x y $
                       (
                         (colored red $ polygon [bb_topLeft, bb_topRight, bb_botRight, bb_botLeft])
                       )
  where
    bb_topLeft  = ((fst hdTopLeft),  (snd hdTopLeft) )
    bb_topRight = ((fst hdBotRight), (snd hdTopLeft) )
    bb_botLeft  = ((fst hdTopLeft),  (snd hdBotRight) )
    bb_botRight = ((fst hdBotRight), (snd hdBotRight) )



--
-- given a mole data structure figure out where it is, what color it should be
-- and whether it should be up out of the hole. If it should be, draw it.
--
drawMole :: Mole -> Picture
drawMole mole@(mtype, pos, uptime, up, wasHit, msg) = translated x y $
           if up==True && wasHit==False then pic else blank
  where
    bodyColor = mColors !! (fromEnum mtype)
    x = fst $ holeLocations !! pos
    y = snd $ holeLocations !! pos
    pic =  (translated 0 (-1.5) $ lettering $ T.pack $ msg) &
           (translated 0.7 0 $ colored black $ solidCircle 0.2)       &
           (translated (-0.7) 0 $ colored black $ solidCircle 0.2)    &
           (colored bodyColor $ solidCircle 2)                        &
           (translated 0 (-1.5) $ colored bodyColor $ solidRectangle 4 3)





--
-- world drawing. passes responsibility to other function depending
-- on current state of the game
--
drawWorld :: World -> Picture
drawWorld world@(World clock _ moles msg score state) = 
  case state of
    BETWEEN -> drawWorldBetween world
    PLAYING -> drawWorldPlay world


drawWorldBetween :: World -> Picture
drawWorldBetween world@(World clock lvl moles msg score state) = 
                  (
                  blank &
                  txt1  &
                  txt2  &
                  blank
                  )
  where
    txt1 = translated (-4)  (2) $ lettering $ T.pack $ if lvl==0 
                                                    then "Wack a Mole" 
                                                    else "Level " ++ show lvl ++ " complete"
    txt2 = translated (-4) (-2) $ lettering $ T.pack $ if lvl==0 
                                                    then "Click to play" 
                                                    else "Click to continue"

--
-- draw the entire game world including all holes and moles
--
drawWorldPlay :: World -> Picture
drawWorldPlay world@(World clock _ moles msg score state) = 
                  (
                  blank  & 
                  molesP &
                  clockS &
                  holes  & 
                  blank
                  )
  where
    holes  = foldr (&) blank (map drawHole holeLocations)
    boxes  = foldr (&) blank (map drawBox holeLocations)
    molesP = foldr (&) blank (map drawMole moles)
    clockC = translated (-8) 0 $ lettering $ T.pack $ show clock
    clockS = colored blue $ translated (-6) 1.5 $ lettering $ T.pack $ show score ++ "  " ++show (clock/100)
    newMsg = translated 0 (-6) $ lettering $ T.pack msg
                  





--
-- update a single mole. this will be a mappable function with current time and mouse pos as the partially
-- applied args of the function mapped over the list of moles
--
-- A very important point is this is being called from two different events - time passing
-- and pointer press
--
updateMole :: Point -> Double -> Mole -> Mole
updateMole mousePos time mole@(mtype, pos, uptime, up, wasHit, msg) = 
  (mtype, pos, uptime, isUp, gotHit, newMsg)
  where
    upDuration = mDurationUp !! (fromEnum mtype)
    isUp = if (time > uptime) && (time<(uptime+upDuration)) 
           then True
           else False  
           
    -- mole is hit if mouse click within bounding box *and* mole is up.
    gotHit = if up==True && wasHit==False
             then
                (  ((fst mousePos) > (fst topCorner)) && ((fst mousePos) < (fst botCorner)) &&
                    ((snd mousePos) < (snd topCorner)) && ((snd mousePos) > (snd botCorner))  )
             else 
                wasHit       
    topCorner = ((fst (holeLocations !! pos)) + (fst hdTopLeft), (snd (holeLocations !! pos)) + (snd hdTopLeft))
    botCorner = ((fst (holeLocations !! pos)) + (fst hdBotRight), (snd (holeLocations !! pos)) + (snd hdBotRight))
    newMsg = "BOO:" ++ show (fst mousePos) ++ ", " ++ show (snd mousePos)



moleWasHit :: Mole -> Bool
moleWasHit mole@(mtype, pos, uptime, up, wasHit, msg) = wasHit


updateMoleTime :: Point -> Double -> Mole -> Mole
updateMoleTime mousePos time mole@(mtype, pos, uptime, up, wasHit, msg) = 
  (mtype, pos, uptime, isUp, wasHit, msg)
  where
    upDuration = mDurationUp !! (fromEnum mtype)
    isUp = if (time > uptime) && (time<(uptime+upDuration)) 
           then True
           else False  


--
-- event function. passes responsibility to other function depending
-- on current state of the game
--
eventProc :: Event -> World -> World
eventProc event world@(World clock lvl moles msg score state) =
  case state of
    BETWEEN -> eventProcBetween event world
    PLAYING -> eventProcPlay event world


eventProcBetween :: Event -> World -> World
eventProcBetween event world@(World clock lvl moles msg score state) =
  case event of
     PointerPress (x,y)    -> World 0 lvl moles msg score PLAYING    -- clock is zero here so it will start at zero during PLAYING state
     _                     -> world
  

--
-- as time passes, update the clock and the moles for the game world
--
-- filter (<3) [1,2,3,4,5,6]
--
eventProcPlay :: Event -> World -> World
eventProcPlay event world@(World clock lvl moles msg score state) =
  case event of
     TimePassing x         -> let newMoles = if (clock/100) > maxTime 
                                             then (levels !! (lvl+1))
                                             else map (updateMole (-1000,0) (clock/100)) moles 
                                  newScore = length $ filter moleWasHit moles
                                  newState = if (clock/100) > maxTime then BETWEEN else PLAYING
                                  newLevel = if (clock/100) > maxTime 
                                             then (if lvl+1 == maxLevel then 0 else lvl+1)
                                             else lvl
                              in World (clock+1) newLevel newMoles msg newScore newState
     PointerPress (x,y)    -> let checkedMoles = map (updateMole (x,y) clock) moles
                              in World clock lvl checkedMoles msg score state
     PointerMovement (x,y) -> let newMsg = ""
                                  topCorner = ((fst (holeLocations !! 7)) + (fst hdTopLeft), (snd (holeLocations !! 7)) + (snd hdTopLeft))
                                  botCorner = ((fst (holeLocations !! 7)) + (fst hdBotRight), (snd (holeLocations !! 7)) + (snd hdBotRight))
                              in World clock lvl moles newMsg score state
     _                     -> world
   


{-

         newMsg =  if(  (x > (fst topCorner)) && (x < (fst botCorner)) &&
                        (y < (snd topCorner)) && (y > (snd botCorner))    )
                   then show (r2 x) ++ "," ++ show (r2 y) ++ " in  " ++ 
                        show (r2 (fst topCorner) ) ++ "," ++ show (r2 (snd topCorner) ) ++ "->" ++
                        show (r2 (fst botCorner) ) ++ "," ++ show (r2 (snd botCorner) )
                   else show (r2 x ) ++ "," ++ show (r2 y ) ++ " out " ++ 
                        show (r2 (fst topCorner) ) ++ "," ++ show (r2 (snd topCorner) ) ++ "->" ++
                        show (r2 (fst botCorner) ) ++ "," ++ show (r2 (snd botCorner) )


         newMsg =       show (x > (fst topCorner)) ++ " " ++
                        show (x < (fst botCorner)) ++ " " ++
                        show (y < (snd topCorner)) ++ " " ++
                        show (y > (snd botCorner)) 

-}



roundToPlaces :: Double -> Int -> Double
roundToPlaces n s = fromIntegral (round (n * factor)) / factor
    where factor = fromIntegral (10^s) 

r2 :: Double -> Double
r2 n = roundToPlaces n 2

--
-- Start the program
--
main = activityOf initState eventProc drawWorld






