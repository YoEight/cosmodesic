{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Network.Cosmodesic.Types
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Network.Cosmodesic.Types where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString
import           Data.Dynamic
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
import Control.Monad.Except
import Control.Monad.Reader

--------------------------------------------------------------------------------
data ExpectedType = ExpectedType String String

--------------------------------------------------------------------------------
data RespError
  = WrongArity Int Int
  | InvalidType [ExpectedType]

--------------------------------------------------------------------------------
type Value = [Response Dynamic] -> Response Dynamic

--------------------------------------------------------------------------------
newtype Values = Values { _vals :: M.Map ByteString Value }

--------------------------------------------------------------------------------
data Context = Context { ctxValues :: Values }

--------------------------------------------------------------------------------
newtype Response a =
    Response (ReaderT Context (ExceptT RespError IO) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Context
             , MonadError RespError
             )
