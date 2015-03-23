{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
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
import qualified Data.Map.Strict as M
import           Data.Typeable

--------------------------------------------------------------------------------
import Control.Monad.Except
import Control.Monad.Reader
import Data.Serialize

--------------------------------------------------------------------------------
data ExpectedType = ExpectedType String String

--------------------------------------------------------------------------------
data RespError
  = WrongArity Int Int
  | InvalidType [ExpectedType]

--------------------------------------------------------------------------------
data Object = forall a. Serializable a => Object a

--------------------------------------------------------------------------------
object :: Serializable a => a -> Object
object = Object

--------------------------------------------------------------------------------
fromObject :: Serializable a => Object -> Maybe a
fromObject (Object o) = cast o

--------------------------------------------------------------------------------
objectRep :: Object -> TypeRep
objectRep (Object o) = typeOf o

--------------------------------------------------------------------------------
type Value = [Response Object] -> Response Object

data RemoteLabel
    = RemoteLabel ByteString
    | RemoteApply RemoteLabel RemoteLabel

data Remote' a = Remote' RemoteLabel

data Value' a = Value' (Remote' (Context -> a)) Context

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

--------------------------------------------------------------------------------
class (Typeable a, Serialize a) => Serializable a

--------------------------------------------------------------------------------
instance (Typeable a, Serialize a) => Serializable a
