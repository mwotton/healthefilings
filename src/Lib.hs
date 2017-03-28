module Lib
    ( runProgram
    , readInput
    , extractOutput
    , Err
    , State
    , Operation
    ) where

import           Data.Text

data Operation
  = Append Text
  | Delete Int
  | Print Int
  | Undo

data Err
  = BadInput
  | TooManyUndos
  | DeletedMoreThanWeHad
  | PrintOutOfBounds
  deriving (Eq,Show)

data State = State

runProgram :: [Operation] -> Either Err State
runProgram = error "runprogram"

readInput :: String -> Either Err [Operation]
readInput = error "readinput"

writeOutput :: State -> IO ()
writeOutput = error "writeoutput"

extractOutput :: State -> [Char]
extractOutput = undefined
