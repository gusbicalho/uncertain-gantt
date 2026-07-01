{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Graphics.Vty qualified as V
import Reflex
import Reflex.Vty
import UncertainGantt qualified as UG

-- A tiny hard-coded project, standing in until we wire this up to load a
-- real .ug script. Durations are plain 'Word's (no probability distribution)
-- so the schedule below is deterministic.
data Resource = Dev | Design
  deriving stock (Eq, Ord, Show)

demoProject :: UG.Project Resource Word
demoProject =
  case UG.buildProject $ do
    UG.addResource Dev 2
    UG.addResource Design 1
    UG.addTask (task "Design" "Design the widget" Design 3 [])
    UG.addTask (task "Backend" "Build the backend" Dev 5 ["Design"])
    UG.addTask (task "Frontend" "Build the frontend" Dev 4 ["Design"])
    UG.addTask (task "Integration" "Wire it all together" Dev 2 ["Backend", "Frontend"]) of
    Left err -> error (show err)
    Right project -> project
 where
  task name description resource duration dependencies =
    UG.Task
      { UG.taskName = name
      , UG.description = description
      , UG.resource = resource
      , UG.duration = duration
      , UG.dependencies = Set.fromList dependencies
      }

demoGantt :: UG.Gantt Resource Word
demoGantt =
  case runIdentity (UG.simulate UG.mostDependentsFirst Identity demoProject) of
    (gantt, Nothing) -> gantt
    (_, Just leftover) -> error ("could not schedule: " <> show (Map.keys leftover))

demoRows :: [(UG.Task Resource Word, UG.Period)]
demoRows =
  List.sortOn (UG.fromInclusive . snd) . Map.toList $ ganttPeriods demoGantt
 where
  ganttPeriods (UG.Gantt periods) = periods

demoCompletion :: Word
demoCompletion = UG.completionTime demoGantt

-- | Plays the simulated schedule back like a progress timeline: a day
-- counter ticks forward, looping once the project completes, and each
-- task's bar fills in as the simulated day passes through its period.
main :: IO ()
main = mainWidget def $ do
  startTime <- liftIO getCurrentTime
  tick <- tickLossy 0.4 startTime
  dayDyn <- foldDyn (\_ day -> if day >= demoCompletion then 0 else day + 1) 0 tick
  initManager_ $ col $ do
    grout (fixed 1) $ text (pure "Uncertain Gantt -- reflex-vty demo (q or Ctrl+C to quit)")
    grout (fixed 1) $ text $ dayLabel <$> current dayDyn
    grout flex $ void $ scrollableText def $ renderFrame <$> dayDyn
  quitKey <- key (V.KChar 'q')
  quitEsc <- key V.KEsc
  quitC <- ctrlc
  pure $ leftmost [() <$ quitKey, () <$ quitEsc, quitC]
 where
  dayLabel day = "Day " <> Text.pack (show day) <> " / " <> Text.pack (show demoCompletion)

renderFrame :: Word -> Text
renderFrame day = Text.unlines (taskLine day <$> demoRows)

taskLine :: Word -> (UG.Task Resource Word, UG.Period) -> Text
taskLine day (task, period) =
  Text.justifyLeft 14 ' ' name
    <> Text.justifyLeft 9 ' ' resourceText
    <> Text.replicate start " "
    <> bar
 where
  name = Text.pack (show (UG.taskName task))
  resourceText = Text.pack (show (UG.resource task))
  start = fromIntegral (UG.fromInclusive period) :: Int
  end = fromIntegral (UG.toExclusive period) :: Int
  total = end - start
  filled = max 0 (min (fromIntegral day) end - start)
  bar = Text.replicate filled "#" <> Text.replicate (total - filled) "."
