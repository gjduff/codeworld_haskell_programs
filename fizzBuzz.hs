{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as T

data States = States { tick :: Double, fizznum :: Int}

tmax :: Int
tmax = 101

--
-- The famous fizzbuzz function 
--
fizzBuzz :: Int -> String
fizzBuzz i = a ++ b ++ c 
  where a = if i `mod` 3 == 0 then "fizz" else ""
        b = if i `mod` 5 == 0 then "buzz" else ""
        c = if ((length a)+(length b))== 0 then show i else ""


--
-- draw numbers, or Fizz, or buzz or the occasional fizzbuzz
--
drawScene :: States -> Picture
drawScene s =  ( lettering $ T.pack $ fnum)
             & ( colored green $ solidCircle 3 )
  where
    etime = tick s
    fnum = fizzBuzz (fizznum s)
    

--
-- as time passes, extract the current second to convert to
-- fizzbuzz
--
processEvent :: Event -> States -> States
processEvent event s =
  let 
    etime = tick s
    fnum = round (etime / 100) `mod` tmax
  in
  case event of
    TimePassing x ->  States (etime+1) fnum
    _             -> s   -- weird hard to find bug if you miss matching this pattern



initState :: States
initState = States 0 0

main = activityOf initState processEvent drawScene


