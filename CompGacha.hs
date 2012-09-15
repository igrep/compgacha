module CompGacha  where

import Control.Monad
import System.Random
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.String

data Card =
  { name :: String
  , frequency :: Int } deriving (Show, Eq, Ord)

allCards :: [Card]
allCards =
  [ Card "" 6
  , Card "" 5
  , Card "" 5
  , Card "" 5
  , Card "" 1
  , Card "" 1
  , Card "" 1
  , Card "" 1 ]

cardsToComp :: Set Cards
cardsToComp = Set.fromList $ filter ( (1==) . frequency ) allCards

cardTable :: [Card]
cardTable = concatMap (\c -> replicate ( frequency c ) c ) allCards

{-instance Ord Card where-}
  {-compare  :: Card -> Ordering-}
  {-compare = comparing frequency-}

welcome :: IO()
welcome = putStrLn ""

showHelp :: IO()
showHelp = do
  putStrLn ""

evalCommand :: String -> ( Int, Either Char Card )
evalCommand cmd =
  execCmdf $ cmdf args
  where
    ws = words cmd
    (cmdName:args) = if null ws then "":[] else ws
    cmdf = Map.lookup commands cmdName
    execCmdf Nothing _ = ( 0, Left 'E' )
    execCmdf (Just f) a = f a

play :: Int -> Set Card -> IO()
play _ ss
  | complete ss = putStrLn ""
play leftMoney ss = do
  cmd <- getCommand
  let ( lostMoney, newCard ) = evalCommand cmd
  putStrLn $ getResultMessage lostMoney newCard

complete :: Set Card -> Bool
complete = Set.subset cardsToComp

main :: IO ()
main = do
  welcome
  showHelp
  play 0 Set.empty
