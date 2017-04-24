module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Stream (Stream, cons, cycle, foldr, iterate, traverse_, window)

main :: Eff (console :: CONSOLE) Unit
main = traverse_ logShow climbingWindow
  where
    climbingWindow :: Stream (NonEmpty Array Int)
    climbingWindow = window 5 climbing
    climbing = cycle (0 :| [1, 2])
    {-
    climbing' = foldr cons climbing
    climbing = iterate (add one) (zero :: Int)
    -}
  {-
  do
  let s = iterate (add one) zero
  logShow (head s)
  logShow (head (tail s))
  logShow (head (tail (tail s)))
  let s' = add one <$> s
  logShow (head s')
  logShow (head (tail s'))
  logShow (head (tail (tail s')))
  let s'' = extend ((_ + 1) <<< head) s
  logShow (head s'')
  logShow (head (tail s''))
  logShow (head (tail (tail s'')))
  let sEffs = logShow <$> s
  sequence_ sEffs
  -}
  {-
  _ <- doEffG_ sEffs'
  pure unit
  -}

