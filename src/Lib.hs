{-# LANGUAGE OverloadedStrings #-}

-- | Fairly standard interpreter over a datatype.
--   We use the Either monad throughout in order to separate error
--   handling.
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
import           Safe.Exact    (dropExactMay)
import           Text.Read     (readMaybe)

data Operation
  = Append String
  -- we use Word here because a negative deletion or print is nonsensical
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

emptyState :: State
emptyState = State [] "" ""

extractOutput :: State -> [Char]
extractOutput = reverse . output

runProgram :: [Operation] -> Either Err State
runProgram ops = foldM applyOp emptyState ops

applyOp :: State -> Operation -> Either Err State
applyOp  st@(State hist curr out) op =
  case op of
    Append t -> pure $ st { history = curr : hist
                          , current = curr <> t }
    Delete i -> do
      case dropExactMay (fromIntegral i) (reverse curr) of
        Nothing -> Left DeletedMoreThanWeHad
        Just c -> pure $ st { history = curr : hist
                            , current = reverse c}
    Print i  ->
      -- 1-based indexing
      case atMay curr (fromIntegral i - 1) of
        Nothing -> Left PrintOutOfBounds
        Just c  -> pure $ st { output = c : out }
    Undo ->
      case hist of
        []     -> Left TooManyUndos
        (x:xs) -> pure $ st { history = xs, current = x }

readInput :: String -> Either Err [Operation]
readInput = mapM readCommand . drop 1 . lines

readCommand :: String -> Either Err Operation
readCommand x =
  case words x of
    ["1", x] -> Right $ Append x
    ["2", x] -> Delete <$> getInt x
    ["3", x] -> Print <$> getInt x
    ["4"]    -> Right Undo
    _        -> Left BadInput

  where getInt = upgradeMaybe BadInput . readMaybe

        upgradeMaybe :: a -> Maybe b -> Either a b
        upgradeMaybe _ (Just x) = Right x
        upgradeMaybe x _        = Left x
