{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Network.Cosmodesic.Remote
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Network.Cosmodesic.Remote where

--------------------------------------------------------------------------------
import           Data.ByteString
import           Data.Monoid
import qualified Data.Set as S
import           Data.Typeable

--------------------------------------------------------------------------------
import Network.Cosmodesic.Response

--------------------------------------------------------------------------------
data Remote a where
    Ref :: ByteString -> Remote a

    Local :: Typeable a => ByteString -> a -> Remote a

    Ap  :: Remote (a -> b)
        -> Remote a
        -> Remote b

    Ap2 :: Remote (a -> b -> c)
        -> Remote a
        -> Remote b
        -> Remote c

    Ap3 :: Remote (a -> b -> c -> d)
        -> Remote a
        -> Remote b
        -> Remote c
        -> Remote d

--------------------------------------------------------------------------------
ref :: ByteString -> proxy a -> Remote a
ref n _ = Ref n

--------------------------------------------------------------------------------
local :: Typeable a => a -> Remote a
local n = Local "tag" n

--------------------------------------------------------------------------------
refs :: Remote a -> S.Set ByteString
refs (Ref n)       = S.singleton n
refs (Local _ _)   = S.empty
refs (Ap f a)      = refs f <> refs a
refs (Ap2 f a b)   = refs f <> refs a <> refs b
refs (Ap3 f a b c) = refs f <> refs a <> refs b <> refs c

--------------------------------------------------------------------------------
app :: Remote (a -> b) -> Remote a -> Remote b
app = Ap

--------------------------------------------------------------------------------
app2 :: Remote (a -> b -> c) -> Remote a -> Remote b -> Remote c
app2 = Ap2

--------------------------------------------------------------------------------
app3 :: Remote (a -> b -> c -> d)
     -> Remote a
     -> Remote b
     -> Remote c
     -> Remote d
app3 = Ap3
