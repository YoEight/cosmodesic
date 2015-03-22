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
collectRefs :: Remote a -> S.Set ByteString
collectRefs (Ref n)       = S.singleton n
collectRefs (Local _ _)   = S.empty
collectRefs (Ap f a)      = collectRefs f <> collectRefs a
collectRefs (Ap2 f a b)   = collectRefs f <>
                            collectRefs a <>
                            collectRefs b
collectRefs (Ap3 f a b c) = collectRefs f <>
                            collectRefs a <>
                            collectRefs b <>
                            collectRefs c

--------------------------------------------------------------------------------
remoteAp :: Remote (a -> b) -> Remote a -> Remote b
remoteAp = Ap

--------------------------------------------------------------------------------
remoteAp2 :: Remote (a -> b -> c) -> Remote a -> Remote b -> Remote c
remoteAp2 = Ap2

--------------------------------------------------------------------------------
remoteAp3 :: Remote (a -> b -> c -> d)
          -> Remote a
          -> Remote b
          -> Remote c
          -> Remote d
remoteAp3 = Ap3
