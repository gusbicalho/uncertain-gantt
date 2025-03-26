{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module UncertainGantt.Script.Streaming.StatementInterpreter (
  StreamStatementInterpreter (..),
  interpretStream,
) where

import Control.Monad.Trans.Class (lift)
import Streaming (Stream)
import Streaming.Prelude qualified as S
import UncertainGantt.Script.Types (Statement (..))

class StreamStatementInterpreter a where
  type Output a
  interpretStmt :: a -> Statement -> Stream (S.Of (Output a)) IO a

interpretStream ::
  (StreamStatementInterpreter a) =>
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
