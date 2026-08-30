{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure derivation of everything a screen shows: task table rows in
dependency order, resource\/duration panel rows with usage, the
vocabulary strip, and the compact duration notation. See tui/DESIGN.md.
The terminal-specific renderers ('renderTaskTable', 'renderResourcePanel',
'renderDurationPanel', 'stripLine', 'separatorLine') are only used by the
TUI; a web frontend uses the row types directly.
-}
module Editor.View (
  durationDisplay,
  TaskRow (..),
  taskRows,
  renderTaskTable,
  taskDetailLine,
  ResourceRow (..),
  resourceRows,
  renderResourcePanel,
  resourceDetailLine,
  DurationRow (..),
  durationRows,
  renderDurationPanel,
  durationDetailLine,
  stripLine,
  separatorLine,
) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Editor.Doc (Doc, Element (ElemAlias, ElemResource, ElemTask), docProjectIssues, renderIssue)
import Numeric (showFFloat)
import UncertainGantt qualified as UG
import UncertainGantt.Lang.Types (
  DurationD (LogNormalD, NormalD, UniformD),
  ResourceDescription (ResourceDescription),
  TaskDescription (TaskDescription),
  unDurationAlias,
  unResource,
 )
import UncertainGantt.Project.Tolerant (
  BuildIssue (TaskMissingResource, TaskUnknownDuration),
  issueTasks,
 )
import UncertainGantt.ToText (ToText (toText), showText)

-- * Duration notation

{- | Compact, unit-carrying notation: @1–5d@ (uniform), @~13d ±2@ (normal),
@~13d ×1.6@ (logNormal, geometric spread factor e^σ).
-}
durationDisplay :: DurationD -> Text
durationDisplay = \case
  UniformD from to -> num (fromIntegral from) <> "–" <> num (fromIntegral to) <> "d"
  NormalD average stddev -> "~" <> num average <> "d ±" <> num stddev
  LogNormalD median stddev -> "~" <> num median <> "d ×" <> num (exp stddev)

-- | At most one decimal, no trailing @.0@.
num :: Double -> Text
num x
  | rounded == fromIntegral (truncate rounded :: Integer) = showText (truncate rounded :: Integer)
  | otherwise = Text.pack (showFFloat (Just 1) rounded "")
 where
  rounded = fromIntegral (round (x * 10) :: Integer) / 10 :: Double

-- * Task table

data TaskRow = TaskRow
  { taskRowIndex :: Int
  -- ^ Index of the task's element in the 'Doc'.
  , taskRowDepth :: Int
  -- ^ Longest dependency path from a root; drives indentation.
  , taskRowName :: Text
  , taskRowResource :: Text
  , taskRowDuration :: Text
  , taskRowAfter :: [Text]
  , taskRowDependents :: [Text]
  -- ^ Names of tasks that depend on this one (for the delete guard).
  , taskRowDescription :: Text
  , taskRowIssues :: [Text]
  {- ^ Rendered issues implicating this task ('Editor.Doc.docProjectIssues');
  non-empty means the row is flagged.
  -}
  , taskRowUndefinedResource :: Maybe Text
  {- ^ A non-empty resource name the task references that is not defined
  (drives editors' create-from-use quick fixes).
  -}
  , taskRowUndefinedDuration :: Maybe Text
  -- ^ Like 'taskRowUndefinedResource' for duration-alias references.
  }

{- | Tasks in dependency (topological) order, dependencies before
dependents; tasks stuck in a cycle come last, flagged as problems.
-}
taskRows :: Doc -> [TaskRow]
taskRows doc = go Map.empty tasks
 where
  tasks = [(i, t) | (i, ElemTask t) <- zip [0 ..] doc]
  taskNames = Set.fromList [name | (_, TaskDescription name _ _ _ _) <- tasks]
  issues = snd (docProjectIssues doc)
  issuesOf name = [renderIssue issue | issue <- issues, name `elem` issueTasks issue]
  undefinedResourceOf name =
    listToMaybe
      [ r'
      | TaskMissingResource t r <- issues
      , t == name
      , let r' = toText (unResource r)
      , not (Text.null r')
      ]
  undefinedDurationOf name =
    listToMaybe
      [ a'
      | TaskUnknownDuration t a <- issues
      , t == name
      , let a' = toText (unDurationAlias a)
      , not (Text.null a')
      ]
  dependentsOf name =
    [ toText (UG.unTaskName other)
    | (_, TaskDescription other _ _ _ deps) <- tasks
    , name `elem` deps
    ]
  go depths pending =
    case List.partition (ready . snd) pending of
      ([], []) -> []
      ([], stuck) -> [row (-1) t i | (i, t) <- stuck]
      (readyNow, rest) ->
        let depths' = foldr insertDepth depths (snd <$> readyNow)
         in [row (depths' Map.! name) t i | (i, t@(TaskDescription name _ _ _ _)) <- readyNow]
              <> go depths' rest
   where
    ready (TaskDescription _ _ _ _ deps) =
      all (\dep -> dep `Map.member` depths || dep `Set.notMember` taskNames) deps
    insertDepth (TaskDescription name _ _ _ deps) acc =
      Map.insert name (foldr (max . depthAfter) 0 deps) acc
     where
      depthAfter dep = maybe 0 (+ 1) (Map.lookup dep depths)
  row depth (TaskDescription name description resource duration deps) i =
    TaskRow
      { taskRowIndex = i
      , taskRowDepth = max 0 depth
      , taskRowName = toText (UG.unTaskName name)
      , taskRowResource = toText (unResource resource)
      , taskRowDuration = either (toText . unDurationAlias) durationDisplay duration
      , taskRowAfter = toText . UG.unTaskName <$> deps
      , taskRowDependents = dependentsOf name
      , taskRowDescription = description
      , taskRowIssues = issuesOf name
      , taskRowUndefinedResource = undefinedResourceOf name
      , taskRowUndefinedDuration = undefinedDurationOf name
      }

-- | Header plus scrolling rows; the selected row is kept in view.
renderTaskTable :: Int -> Int -> Int -> [TaskRow] -> Text
renderTaskTable width height sel rows
  | null rows = "(no tasks yet — press a to add one)"
  | otherwise = renderTable width height sel ["TASK", "RESOURCE", "DURATION", "AFTER"] (cells <$> rows)
 where
  cells r =
    [ Text.replicate (2 * taskRowDepth r) " "
        <> taskRowName r
        <> (if Text.null (taskRowDescription r) then "" else " ≡")
        <> (if null (taskRowIssues r) then "" else " !")
    , taskRowResource r
    , taskRowDuration r
    , case taskRowAfter r of
        [] -> "—"
        after -> Text.intercalate ", " after
    ]

{- | Detail line under the table: the selected task's issues if it has
any, otherwise its description.
-}
taskDetailLine :: Int -> [TaskRow] -> Text
taskDetailLine sel rows = case drop sel rows of
  (r : _)
    | not (null (taskRowIssues r)) -> "! " <> Text.intercalate "  ·  " (taskRowIssues r)
    | not (Text.null (taskRowDescription r)) -> "≡ " <> taskRowDescription r
  _ -> ""

-- * Resource and duration panels

data ResourceRow = ResourceRow
  { resourceRowIndex :: Int
  , resourceRowName :: Text
  , resourceRowCapacity :: Word
  , resourceRowUsedBy :: [Text]
  }

resourceRows :: Doc -> [ResourceRow]
resourceRows doc =
  [ ResourceRow
      { resourceRowIndex = i
      , resourceRowName = toText (unResource r)
      , resourceRowCapacity = capacity
      , resourceRowUsedBy =
          [ toText (UG.unTaskName name)
          | ElemTask (TaskDescription name _ resource _ _) <- doc
          , resource == r
          ]
      }
  | (i, ElemResource (ResourceDescription r capacity)) <- zip [0 ..] doc
  ]

renderResourcePanel :: Int -> Int -> Int -> [ResourceRow] -> Text
renderResourcePanel width height sel rows
  | null rows = "(no resources yet — press a to add one)"
  | otherwise = renderTable width height sel ["NAME", "CAPACITY", "USED BY"] (cells <$> rows)
 where
  cells r = [resourceRowName r, showText (resourceRowCapacity r), usedByCell (resourceRowUsedBy r)]

resourceDetailLine :: Int -> [ResourceRow] -> Text
resourceDetailLine sel rows = case drop sel rows of
  (r : _) ->
    resourceRowName r
      <> " — capacity "
      <> showText (resourceRowCapacity r)
      <> " — "
      <> usedByFull (resourceRowUsedBy r)
  _ -> ""

data DurationRow = DurationRow
  { durationRowIndex :: Int
  , durationRowName :: Text
  , durationRowDefinition :: Text
  , durationRowUsedBy :: [Text]
  }

durationRows :: Doc -> [DurationRow]
durationRows doc =
  [ DurationRow
      { durationRowIndex = i
      , durationRowName = toText (unDurationAlias alias)
      , durationRowDefinition = durationDisplay definition
      , durationRowUsedBy = usedBy alias
      }
  | (i, ElemAlias alias definition) <- zip [0 ..] doc
  ]
 where
  usedBy alias =
    [ toText (UG.unTaskName name)
    | ElemTask (TaskDescription name _ _ (Left a) _) <- doc
    , a == alias
    ]

renderDurationPanel :: Int -> Int -> Int -> [DurationRow] -> Text
renderDurationPanel width height sel rows
  | null rows = "(no duration aliases yet — press a to add one)"
  | otherwise = renderTable width height sel ["NAME", "DEFINITION", "USED BY"] (cells <$> rows)
 where
  cells r = [durationRowName r, durationRowDefinition r, usedByCell (durationRowUsedBy r)]

durationDetailLine :: Int -> [DurationRow] -> Text
durationDetailLine sel rows = case drop sel rows of
  (r : _) ->
    durationRowName r
      <> " — "
      <> durationRowDefinition r
      <> " — "
      <> usedByFull (durationRowUsedBy r)
  _ -> ""

usedByCell :: [Text] -> Text
usedByCell [] = "unused"
usedByCell tasks = showText (length tasks) <> pluralTasks tasks <> ": " <> Text.intercalate ", " tasks

usedByFull :: [Text] -> Text
usedByFull [] = "unused"
usedByFull tasks =
  "used by " <> showText (length tasks) <> pluralTasks tasks <> ": " <> Text.intercalate ", " tasks

pluralTasks :: [a] -> Text
pluralTasks tasks = if length tasks == 1 then " task" else " tasks"

-- * Vocabulary strip

{- | One strip line, e.g.
@Resources [R]: TeamA ×2 · TeamB ×5  (+3 more)@; items that don't fit
are folded into the @+n more@ tail (the panel shows everything).
-}
stripLine :: Text -> [Text] -> Int -> Text
stripLine label items width = prefix <> go items (width - Text.length prefix)
 where
  prefix = label <> ": "
  go [] _ = "(none)"
  go allItems budget =
    let fits candidate =
          Text.length (Text.intercalate " · " candidate)
            + (if length candidate == length allItems then 0 else Text.length (moreTail (length allItems - length candidate)))
            <= budget
        keep = List.last (0 : [n | n <- [1 .. length allItems], fits (take n allItems)])
        kept = take keep allItems
     in if keep == length allItems
          then Text.intercalate " · " kept
          else Text.intercalate " · " kept <> moreTail (length allItems - keep)
  moreTail n = "  (+" <> showText n <> " more)"

separatorLine :: Int -> Text
separatorLine width = Text.replicate (max 0 width) "─"

-- * Shared table rendering

{- | Fixed columns sized to content (clamped), last column takes the rest;
cells truncate with @…@. Row 'sel' is marked and kept in view.
-}
renderTable :: Int -> Int -> Int -> [Text] -> [[Text]] -> Text
renderTable width height sel headers rows =
  Text.intercalate "\n" (header : take (height - 1) (drop offset markedRows))
 where
  gap = 2
  prefixWidth = 2
  columns = List.transpose (headers : rows)
  fixedColumns = init columns
  budgets =
    [ min (maximum (Text.length <$> column)) clamp
    | (column, clamp) <- zip fixedColumns columnClamps
    ]
  -- The first (name) column gets the most room; the rest are compact.
  columnClamps = 28 : repeat 14
  lastBudget =
    max minLastColumn $
      width - prefixWidth - sum budgets - gap * length budgets
  allBudgets = budgets <> [lastBudget]
  minLastColumn = 6
  renderCells cells =
    Text.stripEnd . Text.intercalate (Text.replicate gap " ") $
      zipWith fitCell allBudgets cells
  fitCell budget cell
    | Text.length cell > budget = Text.take (max 1 (budget - 1)) cell <> "…"
    | otherwise = cell <> Text.replicate (budget - Text.length cell) " "
  header = Text.take (max 1 width) $ Text.replicate prefixWidth " " <> renderCells headers
  markedRows =
    [ Text.take (max 1 width) $ (if i == sel then "> " else "  ") <> renderCells cells
    | (i, cells) <- zip [0 ..] rows
    ]
  offset = max 0 (min (sel - (height - 1) `div` 2) (length rows - (height - 1)))
