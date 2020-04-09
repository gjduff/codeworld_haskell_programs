{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.List as D
import qualified Data.Text as T

{-
     The tiles in the game are 
     created in the following functions
-}
player :: Double -> Double -> Picture
player x y = translated x y $
             (translated 0.5 0.5 $ colored yellow $ solidCircle 0.48) &
             (colored black $ solidPolygon [(0,0),(1,0),(1,1),(0,1)])

wall :: Picture
wall = (colored blue $ solidPolygon [(0,0),(1,0),(1,1)]) &
       (colored green $ solidPolygon [(0,0),(0,1),(1,1)])
       
floor' :: Picture       
floor' = (colored grey $ solidPolygon [(0.1,0.1),(0.9,0.1),(0.9,0.9),(0.1,0.9)]) &
         (colored black $ solidPolygon [(0,0),(1,0),(1,1),(0,1)])

box :: Picture
box = (colored black $ polygon [(0,0),(0,1),(1,1),(0,0),(1,0),(0,1),(1,1),(1,0)]) &
      (colored brown $ solidPolygon [(0,0),(1,0),(1,1),(0,1)]) 
      
dest :: Picture
dest = (translated 0.5 0.5 $ colored orange $ solidCircle 0.15) &
       (translated 0.5 0.5 $ colored red $ solidCircle 0.25)



{-
     The coordinates for the tiles in the game are 
     created in the following functions
-}
tileCoords :: [(Double, Double)]
tileCoords = (\x y -> (y,x)) <$> [0 .. 10] <*> [0 .. 10]       

wallCoords  :: [(Double, Double)]
wallCoords = filter (\(x,y) -> x==0 || y==0 || x==10 || y==10) tileCoords

obstacleCoords :: [[(Double, Double)]]
obstacleCoords = [ 
                   [(2,9),(8,9),(2,8),(4,8),(3,7),(3,6),(7,6),(8,6),(3,5),(8,5),(3,4),(8,4),(8,3),(8,2),(1,1),(2,1),(3,1),(8,1)],
                   [(3,9),(3,7),(6,7),(3,6),(6,6),(3,5),(4,5),(5,5),(6,5),(8,5),(9,5),(3,4),(6,4),(3,3),(6,3),(3,1)],
                   [(3,9),(4,9),(5,9),(6,9),(7,9),(8,9),(8,8),(8,7),(1,6),(3,6),(4,6),(5,6),(8,6),(1,5),(3,5),(8,5),(1,4),(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),(3,3)],
                   [(9,8),(1,7),(2,7),(3,7),(4,7),(5,7),(5,6),(6,6),(5,5),(5,4),(5,3),(9,3),(4,1),(5,1),(6,1)],
                   [(8,9),(1,8),(2,8),(8,8),(3,7),(7,7),(3,3),(7,3),(3,2),(8,2),(9,2),(2,1)],
                   [(4,7),(5,7),(6,7),(7,7),(4,6),(4,5),(4,4),(5,4),(6,4),(7,4),(6,2)],
                   [(1,5),(2,5),(4,5),(6,5),(8,5)], 
                   [(1,5),(3,5),(4,5),(5,5),(6,5),(7,5),(9,5),(5,1),(5,3),(5,4),(5,6),(5,7),(5,9)],
                   [(4,6),(5,6)] 
                 ]


boxCoords :: [[(Double, Double)]]
boxCoords = [ 
              [(6,5),(5,4),(4,3),(6,3)],
              [(2,8),(6,8),(2,6),(2,4)],
              [(4,2),(6,2),(8,2)],
              [(7,6),(3,5),(3,3)],
              [(6,7),(2,6),(5,2),(6,2)],
              [(3,8),(8,8),(3,7),(5,2)],
              [(2,7),(4,7),(6,7),(8,7)], 
              [(2,8),(8,8),(8,2)],
              [(3,8),(7,8)] 
            ]


destCoords :: [[(Double, Double)]]
destCoords = [ 
               [(1,9),(1,8),(9,2),(9,1)],
               [(5,6),(9,6),(5,4),(9,4)],
               [(1,9),(9,9),(4,5)],
               [(1,9),(2,9),(1,8)],
               [(1,9),(9,9),(1,1),(9,1)],
               [(5,6),(6,6),(5,5),(6,5)],
               [(4,2),(5,2),(6,2),(7,2)], 
               [(3,2),(3,3),(3,4)],
               [(6,2),(7,2)] 
             ]

incrementLevel :: Int -> Int
incrementLevel lvl = if lvl+1 < length destCoords then lvl+1 else 0



{-
     Translate a a picture by coordinates found in a tuple
-}
tupTrans :: Picture -> (Double,Double) -> Picture
tupTrans pic coords = translated (fst coords) (snd coords) pic


{-
     These functions put multiple tiles in their place
-}
wholeFloor :: Picture
wholeFloor = foldr (&) blank (map (tupTrans floor') tileCoords)

wholeWall :: Picture
wholeWall = foldr (&) blank (map (tupTrans wall) wallCoords)

wholeObstacles :: Int -> Picture
wholeObstacles lvl = foldr (&) blank (map (tupTrans wall) (obstacleCoords !! lvl))

wholeBoxes :: [(Double, Double)] -> Picture
wholeBoxes boxXYs = foldr (&) blank (map (tupTrans box) boxXYs)

wholeDests :: Int -> Picture
wholeDests lvl = foldr (&) blank (map (tupTrans dest) (destCoords !! lvl))


{-
     Display message indicating boxes are at their destinations
     if parameter passed is True
-}
completeText :: Bool -> Picture
completeText True  = ( colored red $ translated 5.5 6.5 $ lettering "Job Complete!" ) 
completeText False = blank


{-
     roundToPlaces and showClick are just for displaying mouse
     clicks for debugging purposes
-}
roundToPlaces :: Double -> Int -> String
roundToPlaces n s = show $ fromIntegral (round (n * factor)) / factor
    where factor = fromIntegral (10^s) 

showClick :: (Double, Double) -> Picture
showClick (x,y) = colored red $ translated 5.5 7.5 $ lettering $ 
                                T.pack (roundToPlaces st_x 2 ++ "," ++ roundToPlaces st_y 2)
  where
    st_x = (x + 9.2) / 1.8
    st_y = (y + 9.2) / 1.8


{-
     The game states: player coords, level number
-}
data States = States { px :: Double 
                     , py :: Double
                     , level :: Int 
                     , boxCoordsLvl :: [(Double, Double)]
                     , boxesAtDests :: Bool
                     , mousePress :: (Double, Double)
                     , screen :: Screen}


-- This will be used to render/do logic for either the working level
-- of the screen between levels
data Screen = Playing | Between deriving (Eq, Ord, Show) 


{-
     Rendering. determine if we're playing, or between levels
     and use appropriate event scene rendering function
-}
wholeScene :: States -> Picture
wholeScene state = if (screen state) == Between 
                    then wholeSceneBetween state
                    else wholeScenePlaying state


{-                                         
     renders the between levels screen
-}
wholeSceneBetween :: States -> Picture
wholeSceneBetween state = blank &
          ( colored blue $ translated (-1) (2) $ styledLettering Bold Monospace "Warehouse Worker" )  &
          ( colored red $ translated (-1) (-2) $ lettering $ T.pack $ "level " ++ show ((level state)+1) ) &
          ( colored red $ translated (-1) (-6) $ lettering "Click or tap to play" ) &
          ( colored red $ translated (-1) (-7) $ lettering "Press R to Reset" )

{-
     Renders the whole scene when playing
-}
wholeScenePlaying :: States -> Picture
wholeScenePlaying (States x y lvl boxXYs atDests mPress screen) = translated (-10) (-10) $ scaled 1.8 1.8
                          (  
                             (completeText atDests) & -- (showClick mPress) &
                             (player x y)           & wholeWall            & 
                             (wholeBoxes boxXYs)    & (wholeObstacles lvl) & 
                             (wholeDests lvl)       & wholeFloor           
                          )
                             


{-
     update the position of a box if the player
     is about to overlap it and if it's not going into a wall
-}
moveBoxIfNeeded :: Double -> Double -> Double -> Double -> Int -> (Double, Double) -> (Double, Double)
moveBoxIfNeeded px py npx npy lvl (bx, by) = (finalBx, finalBy)
  where
    newBx = if npx == bx && npy == by
            then bx + (npx - px) 
            else bx
    newBy = if npy == by && npx == bx 
            then by + (npy - py) 
            else by
    (finalBx, finalBy) = if (newBx, newBy) `elem` ( wallCoords ++ (obstacleCoords !! lvl) )
                         then (bx, by)
                         else (newBx, newBy)


{-
     checks collisions and returns the new state based on those
     collisions
-}
checkCollide :: Double -> Double -> Double -> Double -> Int -> [(Double,Double)] -> (Double,Double) -> States
checkCollide px py npx npy lvl boxXYs mPress = 
                               if atDests == False then
                                 if playerAgainstWall 
                                        || boxesOverlap 
                                        || boxPinnedToWall
                                 then (States px py lvl boxXYs atDests mPress Playing) 
                                 else (States npx npy lvl newBoxXYs atDests mPress Playing)
                               else initialStates (incrementLevel lvl)
  where
    newBoxXYs = map (moveBoxIfNeeded px py npx npy lvl) boxXYs
    atDests = D.sort newBoxXYs == D.sort (destCoords !! lvl) || D.sort boxXYs == D.sort (destCoords !! lvl)
    boxPinnedToWall = (npx, npy) `elem` newBoxXYs
    boxesOverlap = newBoxXYs /= (D.nub newBoxXYs)
    playerAgainstWall = (npx,npy) `elem` ( wallCoords ++ (obstacleCoords !! lvl) )
    

{-
     Event processing. determine if we're playing, or between levels
     and use appropriate event processing function
-}
processEventToUpdateState :: Event -> States -> States
processEventToUpdateState event state = if (screen state) == Between 
                                        then processEventToUpdateStateBetween event state
                                        else processEventToUpdateStatePlaying event state


{-
     Event processing when in the between level screen.
     just wait for the user to click/tap
-}
processEventToUpdateStateBetween :: Event -> States -> States
processEventToUpdateStateBetween event (States px py lvl boxXYs atDests mpress screen) =
   case event of
     PointerPress (x,y) -> (States px py lvl boxXYs atDests mpress Playing)
     KeyPress "Enter"   -> (States px py lvl boxXYs atDests mpress Playing)
     _                  -> (States px py lvl boxXYs atDests mpress screen)
     

{-
     Event processing when playing. detect key presses used in game and return
     the new state
     Mouse presses also move the player and must be adjusted since the
     player and everything else end up being translated by 10 and scaled by 1.8
-}
processEventToUpdateStatePlaying :: Event -> States -> States
processEventToUpdateStatePlaying event (States px py lvl boxXYs atDests mpress Playing) =
   case event of
     PointerPress (x,y) | (x_a > (px)) && ((abs (x_a-px)) > (abs (y_a-py))) -> checkCollide px py (px+1) py lvl boxXYs (x,y)
                        | (x_a < (px)) && ((abs (x_a-px)) > (abs (y_a-py))) -> checkCollide px py (px-1) py lvl boxXYs (x,y)
                        | (y_a > (py)) && ((abs (y_a-py)) > (abs (x_a-px))) -> checkCollide px py px (py+1) lvl boxXYs (x,y)
                        | (y_a < (py)) && ((abs (y_a-py)) > (abs (x_a-px))) -> checkCollide px py px (py-1) lvl boxXYs (x,y)
       where
         x_a = (x + 9.2) / 1.8
         y_a = (y + 9.2) / 1.8
     KeyPress "Up"    -> checkCollide px py px (py+1) lvl boxXYs mpress
     KeyPress "Down"  -> checkCollide px py px (py-1) lvl boxXYs mpress
     KeyPress "Left"  -> checkCollide px py (px-1) py lvl boxXYs mpress
     KeyPress "Right" -> checkCollide px py (px+1) py lvl boxXYs mpress
     KeyPress "R"     -> initialStates lvl
     _                -> States px py lvl boxXYs atDests mpress Playing
  



{-
     The initial values of the game states
-}
initialStates :: Int -> States
initialStates lvl = States 2 2 lvl (boxCoords !! lvl) False (0,0) Between


{-
     Main program. just start the animation
-}
main :: IO ()
main = activityOf (initialStates 0) processEventToUpdateState wholeScene


                 


