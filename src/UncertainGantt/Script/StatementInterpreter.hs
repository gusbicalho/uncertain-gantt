{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.StatementInterpreter (StatementInterpreter (..)) where

import UncertainGantt.Script.Types (Statement (..))

class StatementInterpreter a where
  interpretStatement :: Statement -> a -> IO a
