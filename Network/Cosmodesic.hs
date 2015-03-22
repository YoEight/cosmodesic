{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
--------------------------------------------------------------------------------
-- |
-- Module : Network.Cosmodesic
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Network.Cosmodesic where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad ((>=>))
import Data.ByteString hiding (length, zip)
import Data.Dynamic
import Data.Monoid
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Reader hiding (local)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

--------------------------------------------------------------------------------
import Network.Cosmodesic.Response
import Network.Cosmodesic.Types
import Network.Cosmodesic.Values

--------------------------------------------------------------------------------
decls = declare "sum" $ pureFn $ \(xs :: [Int]) -> sum xs
