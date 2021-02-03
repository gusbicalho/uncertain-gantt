{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module UncertainGantt.Script.Runner (
  runScript,
  runString,
  runFromFile,
  runInteractive,
) where

import Control.Exception (Handler (Handler), catchJust, catches, throwIO)
import Control.Monad (unless)
import Control.Monad.Bayes.Class qualified as Bayes
import Control.Monad.Bayes.Population qualified as Population
import Control.Monad.Bayes.Sampler qualified as Sampler
import Data.Bifunctor (Bifunctor (first))
import Data.Bool (bool)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Functor ((<&>))
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import System.IO (Handle, IOMode (ReadMode), hFlush, hGetContents, hGetLine, hIsClosed, hPutStr, withFile)
import System.IO.Error (isEOFError, isUserError)
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (BuildProjectError, Project (projectTasks), addResource, addTask, buildProject', editProject', projectResources)
import UncertainGantt.Script.Parser (
  DurationD (..),
  MoreInputExpected (ExpectedMultilineInput),
  Resource (..),
  ResourceDescription (..),
  Statement (..),
  TaskDescription (..),
  parseScript,
 )
import UncertainGantt.Simulator (mostDependentsFirst, simulate)
import UncertainGantt.Task (Task (Task, description, resource, taskName), unTaskName)

runString :: String -> IO ()
runString scriptText =
  case parseScript scriptText of
    Left (parseError, _) -> throwIO . userError $ parseError
    Right script -> runScript script

runScript :: [Statement] -> IO ()
runScript statements = do
  initialProject <- buildProject' estimateDuration (pure ())
  state_ <- IORef.newIORef (emptyState initialProject)
  F.traverse_ (runStatement state_) statements

runFromFile :: FilePath -> IO ()
runFromFile path = withFile path ReadMode $ \handle ->
  runBlocks (const $ safeGetContents handle) []
 where
  safeGetContents handle =
    hIsClosed handle >>= \case
      False -> Just <$> hGetContents handle
      True -> pure Nothing

runInteractive :: Handle -> Handle -> IO ()
runInteractive handleIn handleOut = runBlocks getBlock errorHandlers
 where
  printError = putStrLn
  errorHandlers =
    [ Handler $ \(ex :: IOError) ->
        if isUserError ex
          then printError (show ex)
          else throwIO ex
    , Handler $ \(ex :: BuildProjectError) -> printError (show ex)
    ]
  getBlock expectation = getBlock' expectation <&> fmap (<> "\n")
  getBlock' Nothing = prompt "> "
  getBlock' (Just ExpectedMultilineInput) = Just . unlines <$> getMultilineBlock
  getMultilineBlock = do
    prompt "|   " >>= \case
      Nothing -> pure []
      Just "" -> pure []
      Just line -> ("  " <> line :) <$> getMultilineBlock
  prompt promptStr = do
    hPutStr handleOut promptStr
    hFlush handleOut
    (Just <$> hGetLine handleIn) `catchEOF` pure Nothing
  catchEOF action onEOF =
    catchJust (bool Nothing (Just ()) . isEOFError) action (const onEOF)

runBlocks :: (Maybe MoreInputExpected -> IO (Maybe String)) -> [Handler ()] -> IO ()
runBlocks getBlock errorHandlers = do
  initialProject <- buildProject' estimateDuration (pure ())
  state_ <- IORef.newIORef (emptyState initialProject)
  go state_ "" Nothing
 where
  go state_ prefix inputExpectation =
    getBlock inputExpectation >>= \case
      Nothing -> pure ()
      Just inputString -> case parseScript (prefix <> inputString) of
        Left (_, Just inputExpectation') ->
          go state_ (prefix <> inputString) (Just inputExpectation')
        Left (parseError, _) -> do
          throwIO (userError parseError)
            `catches` errorHandlers
          go state_ "" Nothing
        Right statements -> do
          F.traverse_ (runStatement state_) statements
            `catches` errorHandlers
          go state_ "" Nothing

estimateDuration :: Bayes.MonadSample m => DurationD -> m Word
estimateDuration = fmap (max 1) . estimator
 where
  estimator (UniformD from to) = Bayes.uniformD [from .. to]
  estimator (NormalD avg stdDev) =
    round . max 1 <$> Bayes.normal avg stdDev
  -- LogNormalD loosely based on https://erikbern.com/2019/04/15/why-software-projects-take-longer-than-you-think-a-statistical-model.html
  estimator (LogNormalD median logBlowupStdDev) = do
    logBlowup <- Bayes.normal 0 logBlowupStdDev
    pure . round . max 1 $ median * exp logBlowup

data RunState = RunState
  { runStateProject :: Project Resource DurationD
  , runStateDurationAliases :: Map String DurationD
  , runStateSimulations :: [(Gantt.Gantt Resource DurationD, Double)]
  }

emptyState :: Project Resource DurationD -> RunState
emptyState project =
  RunState
    { runStateProject = project
    , runStateSimulations = []
    , runStateDurationAliases = Map.empty
    }

runStatement :: IORef.IORef RunState -> Statement -> IO ()
runStatement state_ = go
 where
  updateProject update = do
    project <- runStateProject <$> IORef.readIORef state_
    project' <- editProject' project update
    IORef.atomicModifyIORef' state_ $ \state ->
      ( state
          { runStateProject = project'
          , runStateSimulations = []
          }
      , ()
      )
    pure ()
  resolveDuration (Right duration) = pure duration
  resolveDuration (Left alias) = do
    aliases <- runStateDurationAliases <$> IORef.readIORef state_
    case Map.lookup alias aliases of
      Nothing -> throwIO . userError $ "Unknown duration alias " <> alias
      Just duration -> pure duration
  go (AddResource (ResourceDescription resource amount)) =
    updateProject $ addResource resource amount
  go (AddTask (TaskDescription taskName description resource durationDescription dependencies)) = do
    duration <- resolveDuration durationDescription
    updateProject . addTask $ Task taskName description resource duration (Set.fromList dependencies)
  go (DurationAlias alias duration) =
    IORef.atomicModifyIORef' state_ $ \state ->
      (state{runStateDurationAliases = Map.insert alias duration (runStateDurationAliases state)}, ())
  go PrintExample = do
    project <- runStateProject <$> IORef.readIORef state_
    putStrLn "Example run:"
    (gantt, Nothing) <- Sampler.sampleIO $ simulate mostDependentsFirst project
    Gantt.printGantt (printGanttOptions project) gantt
    putStrLn ""
  go PrintDescriptions = do
    project <- runStateProject <$> IORef.readIORef state_
    putStrLn "Tasks:"
    F.for_ (List.sortOn taskName . Map.elems . projectTasks $ project) $ \Task{taskName, description} -> do
      putStr $ unTaskName taskName
      unless (null description) $
        putStr $ ": " <> description
      putStrLn ""
    putStrLn ""
  go (RunSimulations n) = do
    project <- runStateProject <$> IORef.readIORef state_
    putStrLn $ "Running " <> show n <> " simulations..."
    population <-
      Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn (fromIntegral n) *>)
        $ simulate mostDependentsFirst project
    IORef.atomicModifyIORef' state_ $ \state ->
      (state{runStateSimulations = fmap (first fst) . filter (Maybe.isNothing . snd . fst) $ population}, ())
  go PrintCompletionTimes = do
    simulations <- runStateSimulations <$> IORef.readIORef state_
    putStrLn "Completion times:"
    case nonEmpty simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . weightedCompletionTimes
          $ simulations'
  go PrintCompletionTimeMean = do
    simulations <- runStateSimulations <$> IORef.readIORef state_
    putStr "Completion time mean: "
    case nonEmpty simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . weightedAverage
          . weightedCompletionTimes
          $ simulations'
  go (PrintCompletionTimeQuantile numerator denominator) = do
    simulations <- runStateSimulations <$> IORef.readIORef state_
    putStr "Completion time "
    if denominator == 100
      then putStr $ "p" <> show numerator <> ": "
      else putStr $ "quantile " <> show numerator <> "/" <> show denominator <> ": "
    case nonEmpty simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . quantile numerator denominator
          . weightedCompletionTimes
          $ simulations'

weightedCompletionTimes :: NonEmpty (Gantt.Gantt r d, b) -> NonEmpty (Double, b)
weightedCompletionTimes = NonEmpty.sortWith fst . fmap (first (fromIntegral . Gantt.completionTime))

weightedAverage :: NonEmpty (Double, Double) -> Double
weightedAverage ((v0, w0) :| vws) = weightedTotal / totalWeight
 where
  weightedTotal = v0 * w0 + sum (uncurry (*) <$> vws)
  totalWeight = w0 + sum (snd <$> vws)

quantile :: Word -> Word -> NonEmpty (Double, Double) -> Double
quantile numerator denominator ((v0, w0) :| vws) = go v0 w0 vws
 where
  targetW = fromIntegral numerator * (w0 + sum (snd <$> vws)) / fromIntegral denominator
  go v _ [] = v
  go v w ((nextV, nextW) : moreVws)
    | w < targetW = go nextV (w + nextW) moreVws
    | otherwise = v + ((nextV - v) * (targetW - w) / (nextW - w))

printGanttOptions :: Project Resource d -> Gantt.PrintGanttOptions Resource d
printGanttOptions project =
  Gantt.defaultPrintOptions
    { Gantt.sortingBy = compare `on` uncurry sortKey
    , Gantt.resourceName = \(Resource s) -> s
    , Gantt.resourceLegend = Maybe.fromMaybe (head legendChars) . (`Map.lookup` legend)
    }
 where
  sortKey Task{taskName, resource} Gantt.Period{Gantt.fromInclusive, Gantt.toExclusive} =
    (fromInclusive, resource, toExclusive, taskName)
  legendChars = "#*>%"
  legend = Map.fromList $ zip (Map.keys $ projectResources project) (cycle legendChars)
