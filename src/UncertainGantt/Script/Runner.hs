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
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import System.IO (Handle, IOMode (ReadMode), hFlush, hGetContents, hGetLine, hIsClosed, hPutStr, withFile)
import System.IO.Error (isEOFError, isUserError)
import UncertainGantt.Gantt qualified as Gantt
import UncertainGantt.Project (BuildProjectError, Project (projectTasks), addResource, addTask, buildProject', editProject', projectResources)
import UncertainGantt.Script.Parser (
  DurationD (..),
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
    Left parseError -> throwIO . userError $ parseError
    Right script -> runScript script

runScript :: [Statement] -> IO ()
runScript statements = do
  initialProject <- buildProject' estimateDuration (pure ())
  project <- IORef.newIORef initialProject
  simulations <- IORef.newIORef []
  F.traverse_ (runStatement project simulations) statements

runFromFile :: FilePath -> IO ()
runFromFile path = withFile path ReadMode $ \handle ->
  runBlocks (safeGetContents handle) []
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
  getBlock = do
    prompt "> " >>= \case
      Nothing -> pure Nothing
      Just line
        | "\\" `List.isSuffixOf` line -> Just . (cleanLine line <>) . unlines <$> getMultilineBlock
        | otherwise -> pure . Just $ line <> "\n"
  getMultilineBlock = do
    prompt "|   " >>= \case
      Nothing -> pure []
      Just "" -> pure []
      Just line -> ("  " <> line :) <$> getMultilineBlock
  cleanLine [] = "\n"
  cleanLine ['\\'] = "\n"
  cleanLine (c : cs) = c : cleanLine cs
  prompt promptStr = do
    hPutStr handleOut promptStr
    hFlush handleOut
    (Just <$> hGetLine handleIn) `catchEOF` pure Nothing
  catchEOF action onEOF =
    catchJust (bool Nothing (Just ()) . isEOFError) action (const onEOF)

runBlocks :: IO (Maybe String) -> [Handler ()] -> IO ()
runBlocks getBlock errorHandlers = do
  initialProject <- buildProject' estimateDuration (pure ())
  project_ <- IORef.newIORef initialProject
  simulations_ <- IORef.newIORef []
  go project_ simulations_
 where
  go project_ simulations_ =
    getBlock >>= \case
      Nothing -> pure ()
      Just inputString -> case parseScript inputString of
        Left parseError -> do
          throwIO (userError parseError)
            `catches` errorHandlers
          go project_ simulations_
        Right statements -> do
          F.traverse_ (runStatement project_ simulations_) statements
            `catches` errorHandlers
          go project_ simulations_

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

runStatement :: IORef.IORef (Project Resource DurationD) -> IORef.IORef [(Gantt.Gantt Resource DurationD, Double)] -> Statement -> IO ()
runStatement project_ simulations_ = go
 where
  updateProject update = do
    project <- IORef.readIORef project_
    project' <- editProject' project update
    IORef.atomicWriteIORef project_ project'
    IORef.atomicWriteIORef simulations_ []
    pure ()
  go (AddResource (ResourceDescription resource amount)) =
    updateProject $ addResource resource amount
  go (AddTask (TaskDescription taskName description resource duration dependencies)) =
    updateProject . addTask $ Task taskName description resource duration (Set.fromList dependencies)
  go PrintExample = do
    project <- IORef.readIORef project_
    putStrLn "Example run:"
    (gantt, Nothing) <- Sampler.sampleIO $ simulate mostDependentsFirst project
    Gantt.printGantt (printGanttOptions project) gantt
    putStrLn ""
  go PrintDescriptions = do
    project <- IORef.readIORef project_
    putStrLn "Tasks:"
    F.for_ (List.sortOn taskName . Map.elems . projectTasks $ project) $ \Task{taskName, description} -> do
      putStr $ unTaskName taskName
      unless (null description) $
        putStr $ ": " <> description
      putStrLn ""
    putStrLn ""
  go (RunSimulations n) = do
    project <- IORef.readIORef project_
    putStrLn $ "Running " <> show n <> " simulations..."
    population <-
      Sampler.sampleIO
        . Population.explicitPopulation
        . (Population.spawn (fromIntegral n) *>)
        $ simulate mostDependentsFirst project
    IORef.writeIORef simulations_ $
      fmap (first fst) . filter (Maybe.isNothing . snd . fst) $ population
  go PrintCompletionTimes = do
    simulations <- IORef.readIORef simulations_
    putStrLn "Completion times:"
    case nonEmpty simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . weightedCompletionTimes
          $ simulations'
  go PrintCompletionTimeMean = do
    simulations <- IORef.readIORef simulations_
    putStr "Completion time mean: "
    case nonEmpty simulations of
      Nothing -> putStrLn "No simulations available."
      Just simulations' ->
        print
          . weightedAverage
          . weightedCompletionTimes
          $ simulations'
  go (PrintCompletionTimeQuantile numerator denominator) = do
    simulations <- IORef.readIORef simulations_
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
