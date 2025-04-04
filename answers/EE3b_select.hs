{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE FlexibleContexts #-}
module EE3b_select where

import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal (SqlSelect)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)

b_select :: (SqlSelect a r, MonadIO m, SqlBackendCanRead backend) => SqlQuery a -> ReaderT backend m [r]
b_select = undefined
