 {-# LANGUAGE OverloadedStrings #-}
-- import CodeWorld
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import qualified Data.Text as T
import qualified Data.List as D
import qualified System.Random as R
import qualified Control.Monad as C






windowDisplay :: PG.Display
windowDisplay = PG.InWindow "Window" (640, 480) (50, 10)


eventFunc2 :: PG.Event -> Float -> Float
eventFunc2 e s = s

renderFunc2 :: Float -> PG.Picture
renderFunc2 state = PG.Scale 0.25 0.25 $ PG.Text $ show state

updateFunc2 :: Float -> Float -> Float
updateFunc2 dt state = state + dt

{-
     main program. start the animation
-}
main = PG.play 
   windowDisplay
   PG.white
   30
   0
   (renderFunc2 )
   (eventFunc2  )
   (updateFunc2 )





