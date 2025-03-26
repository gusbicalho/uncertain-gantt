{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module UncertainGantt.Gantt (
  Period (..),
  Gantt (..),
  emptyGantt,
  PrintGanttOptions (..),
  defaultPrintOptions,
  printGantt,
  renderGantt,
  completionTime,
) where

import Control.Arrow (Arrow ((&&&)))
import Data.Foldable qualified as F
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import UncertainGantt.Script.ToText (ToText (toText))
import UncertainGantt.Task (Task (..))

data Period = Period {fromInclusive :: Word, toExclusive :: Word}
  deriving stock (Eq, Ord, Show)

newtype Gantt r d = Gantt {unGantt :: Map (Task r d) Period}
  deriving stock (Eq, Ord, Show)

emptyGantt :: Gantt r d
emptyGantt = Gantt Map.empty

data PrintGanttOptions r d = PrintGanttOptions
  { sortingBy :: (Task r d, Period) -> (Task r d, Period) -> Ordering
  , resourceLegend :: r -> Char
  , resourceName :: r -> Text
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
printGantt options gantt = F.traverse_ Text.IO.putStrLn (renderGantt options gantt)

renderGantt :: PrintGanttOptions r d -> Gantt r d -> [Text]
renderGantt PrintGanttOptions{sortingBy, resourceLegend, resourceName} gantt@(Gantt periodMap) =
  concatMap (uncurry renderTask) (List.sortBy sortingBy . Map.toAscList $ periodMap)
    <> ["Completes at: " <> toText (show $ completionTime gantt)]
 where
  renderTask Task{taskName, resource} Period{fromInclusive, toExclusive} =
    [ toWidth 20 . toText $ taskName
    , toWidth 10 . (<> (" " <> Text.singleton (resourceLegend resource))) . resourceName $ resource
    , replicateChars ' ' fromInclusive
    , replicateChars (resourceLegend resource) (toExclusive - fromInclusive)
    ]
  replicateChars c n = Text.replicate (fromIntegral n) (Text.singleton c)
  toWidth w s =
    case Text.length s of
      l
        | Text.length s > w -> Text.take w s
        | otherwise -> s <> Text.replicate (w - l) " "
