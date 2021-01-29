{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module UncertainGantt.Gantt (
  Period (..),
  Gantt (..),
  emptyGantt,
  PrintGanttOptions (..),
  defaultPrintOptions,
  printGantt,
  completionTime,
) where

import Control.Arrow (Arrow ((&&&)))
import Data.Foldable qualified as F
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import UncertainGantt.Task (Task (..), unTaskName)

data Period = Period {fromInclusive :: Word, toExclusive :: Word}
  deriving stock (Eq, Ord, Show)

newtype Gantt r d = Gantt {unGantt :: Map (Task r d) Period}
  deriving stock (Eq, Ord, Show)

emptyGantt :: Gantt r d
emptyGantt = Gantt Map.empty

data PrintGanttOptions r d = PrintGanttOptions
  { sortingBy :: (Task r d, Period) -> (Task r d, Period) -> Ordering
  , resourceLegend :: r -> Char
  , resourceName :: r -> String
  }

completionTime :: Gantt r d -> Word
completionTime = F.maximum . (0 :|) . fmap (toExclusive . snd) . Map.toList . unGantt

defaultPrintOptions :: PrintGanttOptions r d
defaultPrintOptions =
  PrintGanttOptions
    { sortingBy = compare `on` ((fromInclusive &&& toExclusive) . snd)
    , resourceLegend = const '#'
    , resourceName = const ""
    }

printGantt :: PrintGanttOptions r d -> Gantt r d -> IO ()
printGantt PrintGanttOptions{sortingBy, resourceLegend, resourceName} gantt@(Gantt periodMap) = do
  F.traverse_ (uncurry printTask) (List.sortBy sortingBy . Map.toAscList $ periodMap)
  putStrLn $ "Completes at: " <> show (completionTime gantt)
 where
  printTask Task{taskName, resource} Period{fromInclusive, toExclusive} = do
    putStr . toWidth 20 . unTaskName $ taskName
    putStr . toWidth 10 . (<> [' ', resourceLegend resource]) . resourceName $ resource
    printChars ' ' fromInclusive
    printChars (resourceLegend resource) (toExclusive - fromInclusive)
    putStrLn ""
  printChars c n = putStr $ replicate (fromIntegral n) c
  toWidth w s =
    case length s of
      l
        | length s > w -> take w s
        | otherwise -> s ++ replicate (w - l) ' '
