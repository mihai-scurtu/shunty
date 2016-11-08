module Main where

import           Shunty

main :: IO ()
main = do
  putStrLn $ show $ Shunty.tokenize "foo"
  return ()
