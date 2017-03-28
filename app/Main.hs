module Main where

import           Control.Monad ((<=<))
import           Lib
import           System.IO     (hPrint, stderr)

main :: IO ()
main = either (hPrint stderr)
              (mapM_ (putStrLn . return) . extractOutput)
       . (runProgram <=< readInput)
       =<< getContents
