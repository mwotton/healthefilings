{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runProgram
    , readInput
    , extractOutput
    , Err
    , State (..)
    , Operation (..)
    ) where

import           Control.Monad (foldM)
import           Data.Monoid   ((<>))
import           Safe          (atMay)
import           Text.Read     (readMaybe)

data Operation
  = Append String
  | Delete Word
  | Print Word
  | Undo
  deriving Show

data Err
  = BadInput
  | TooManyUndos
  | DeletedMoreThanWeHad
  | PrintOutOfBounds
  deriving (Eq,Show)

data State = State { history :: [String]
                   , current :: String
                   , output  :: String
                   }
  deriving (Show,Eq)

emptyState = State [] "" ""

runProgram :: [Operation] -> Either Err State
runProgram ops = foldM applyOp emptyState ops

applyOp  st@(State hist curr out) op =
  case op of
    Append t -> pure $ st { history = curr : hist
                          , current = curr <> t }
    Delete i -> do
      (fromIntegral i >= length curr) `alternatively` DeletedMoreThanWeHad
      pure $ st { history = curr : hist
                , current = (reverse $ drop (fromIntegral i) $ reverse $ curr) }
    Print i  ->
      -- 1-based indexing
      case atMay curr (fromIntegral i - 1) of
        Nothing -> Left PrintOutOfBounds
        Just c -> pure $ st { output = c : out }
    Undo ->
      case hist of
        [] -> Left TooManyUndos
        (x:xs) -> pure $ st { history = xs, current = x }

alternatively :: Bool -> a -> Either a ()
alternatively True _ = Right ()
alternatively False a = Left a

readInput :: String -> Either Err [Operation]
readInput = mapM readCommand . drop 1 . lines

readCommand :: String -> Either Err Operation
readCommand x =
  case words x of
    ["1", x] -> return $ Append x
    ["2", x] -> Delete <$> getInt x
    ["3", x] -> Print <$> getInt x
    ["4"]    -> return Undo
    _        -> Left BadInput

  where getInt = upgradeMaybe BadInput . readMaybe

upgradeMaybe _ (Just x) = Right x
upgradeMaybe x _        = Left x

writeOutput :: State -> IO ()
writeOutput = error "writeoutput"

extractOutput :: State -> [Char]
extractOutput = reverse . output
