 {-# LANGUAGE OverloadedStrings #-}
-- import CodeWorld
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import qualified Data.Text as T
import qualified Data.List as D
import qualified System.Random as R
import qualified Control.Monad as C


{----------------------------------------------------}
--         CodeWorld emulation layer               --
{----------------------------------------------------}


{-
     CodeWorld uses doubles, and by and large gloss uses float
     so to make pasted CodeWorld code work ...
-}
import Prelude hiding (Double)
type Double = Float       


{-
     Types from CodeWorld that we need to make Events work
-}
data Event = KeyPress String | 
             PointerPress (Float,Float) | 
             TimePassing Float | 
             PointerRelease (Float,Float) |
             PointerMovement (Float,Float)
            -- might have to OR this with some other stuff, like for time, mouse click


{-
     Types from CodeWorld translated to Gloss
-}
type Picture = PG.Picture
type Point = PG.Point
type Color = PG.Color


{-
     location and shape functions from CodeWorld translated to Gloss
-}
scaled l w = PG.Scale (realToFrac l) (realToFrac w)
translated x y = PG.Translate (realToFrac x) (realToFrac y)
rotated n = PG.Rotate (realToFrac (n * (-1) * 180 / 3.14))
colored = PG.Color
circle n = PG.Circle (realToFrac n)  -- because gloss uses Float, not Double
solidCircle n = PG.circleSolid (realToFrac n)
solidRectangle l w = PG.rectangleSolid (realToFrac l) (realToFrac w)
rectangle = PG.rectangleWire
polygon = PG.lineLoop
polyline = PG.Line

solidPolygon p = PG.polygon $ (tail p) ++ [(head p)]


{-
     CodeWorld text functions translated to gloss.
     font types made just to make pasted code compile.
     fonts not actually used yet.
-}
data Monospace = Monospace
data Bold = Bold
styledLettering style font t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)
lettering t = PG.Scale 0.005 0.005 $ PG.Text (T.unpack t)


{-
     CodeWorld colors translated to gloss.
     NOTE:
        RGBA in CodeWorld is a data constructor
        gloss "makeColor" is a function, so no direct translation
        possible, so called it rgba
        so need to search and replace RGBA with rgba :(
-}
green = PG.green
red = PG.red
blue = PG.blue
purple = PG.violet
blank = PG.blank
orange = PG.orange
black = PG.black
white = PG.white
yellow = PG.yellow
grey = PG.greyN 0.5
brown = PG.makeColorI 185 122 87 255
pink = PG.makeColorI 255 174 201 255
light = PG.light
rgba = PG.makeColor  


{-
     CodeWorld puts together pictures with & operator
     so we need to define it to work with Gloss
-}
(&) f s = mappend s f


{-
     define the window that gloss application runs in
-}
windowDisplay :: PG.Display
windowDisplay = PG.InWindow "Window" (640, 480) (50, 10)


{-
     CodeWorld doesn't have seperate functions for the time
     passing event and the key/mouse events so we define the
     two that Gloss needs and pass them the CodeWorld event 
     function to call.
     Mouse Pointer events get scaled down by 20, since the whole
     scene gets rendered scaled up by 20
-}
updateFunc f dt state =  
    f (TimePassing dt) state

eventFunc f event state = 
  case event of 
    PG.EventKey (PG.Char chr) PG.Down _ (x,y)                  -> f (KeyPress [chr]) state
    PG.EventKey (PG.SpecialKey PG.KeyUp) PG.Down _ (x,y)       -> f (KeyPress "Up") state
    PG.EventKey (PG.SpecialKey PG.KeyDown) PG.Down _ (x,y)     -> f (KeyPress "Down") state
    PG.EventKey (PG.SpecialKey PG.KeyLeft) PG.Down _ (x,y)     -> f (KeyPress "Left") state
    PG.EventKey (PG.SpecialKey PG.KeyRight) PG.Down _ (x,y)    -> f (KeyPress "Right") state
    PG.EventKey (PG.SpecialKey PG.KeyEnter) PG.Down _ (x,y)    -> f (KeyPress "Enter") state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Down _ (x,y) -> f (PointerPress (x/20,y/20)) state
    PG.EventKey (PG.MouseButton PG.LeftButton) PG.Up _ (x,y)   -> f (PointerRelease (x/20,y/20)) state
    PG.EventMotion (x,y)                                       -> f (PointerMovement (x/20,y/20)) state
    _                                                          -> f (KeyPress "OTHER") state


{-
     We render the with the CodeWorld render function.
     scaling by 20 otherwise looks way too small in Gloss
-}
renderFunc f state = PG.Scale 20 20 $ f state

{-
     CodeWorld activityOf implementartion using
     Gloss's "play" function
-}
activityOf initStates processEvents render = PG.play 
   windowDisplay
   PG.white
   30
   initStates
   (renderFunc render)
   (eventFunc processEvents)
   (updateFunc processEvents)





-- renderFunc2 f state = PG.Scale 20 20 $ f state

{-
     the time passing event to make CodeWorld
     "animationOf" controller function happen with
     the ellapsed time passed to it
-}
updateFunc2 dt state = state + dt

{-
     CodeWorld animationOf implementartion using
     Gloss's "play" function. animationOf uses no
     mouse/key events so we just pass a do-nothing lambda
     to that event function
-}
animationOf control = PG.play 
   windowDisplay
   PG.white
   30
   0
   (renderFunc control)
   (eventFunc (\e s -> s) )
   (updateFunc2 )




{----------------------------------------------------}
--     Paste Code world code below!!                --
--                                                  --
-- caveats:                                         --
--     RGBA has to become rgba                      --
{----------------------------------------------------}



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