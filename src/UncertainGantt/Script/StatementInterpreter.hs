{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.StatementInterpreter (
  StatementInterpreter (..),
  interpretStream,
) where

import Control.Monad.Trans.Class (lift)
import Streaming (Stream)
import Streaming.Prelude qualified as S
import UncertainGantt.Script.Types (Statement (..))

class StatementInterpreter a where
  type Output a
  interpretStmt :: a -> Statement -> Stream (S.Of (Output a)) IO a

interpretStream ::
  (StatementInterpreter a) =>
  a ->
  Stream (S.Of Statement) IO () ->
  Stream (S.Of (Output a)) IO a
interpretStream a statements =
  -- Pretty sure there is a helper for this...
  lift (S.next statements) >>= \case
    Left () -> pure a
    Right (stmt, statements') -> do
      a' <- interpretStmt a stmt
      interpretStream a' statements'
