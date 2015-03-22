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
data RegError = AlreadyBounded ByteString

--------------------------------------------------------------------------------
newtype Values = Values { _vals :: M.Map ByteString Value }

--------------------------------------------------------------------------------
emptyValues :: Values
emptyValues = Values M.empty

--------------------------------------------------------------------------------
type Register = M.Map ByteString Value

--------------------------------------------------------------------------------
newtype Declarations = Decls (Register -> Either RegError Register)

--------------------------------------------------------------------------------
declare :: ByteString -> Value -> Declarations
declare n v = Decls $ \rg ->
    if M.member n rg
    then Left (AlreadyBounded n)
    else Right $ M.insert n v rg

--------------------------------------------------------------------------------
instance Monoid Declarations where
    mempty = Decls Right

    mappend (Decls kl) (Decls kr) = Decls (kl >=> kr)

--------------------------------------------------------------------------------
register :: Declarations -> Values -> Either RegError Values
register (Decls k) (Values m) = fmap Values $ k m

--------------------------------------------------------------------------------
tests :: Declarations
tests = declare "sum" $ strict1 $ \(xs :: [Int]) -> sum xs

--------------------------------------------------------------------------------
data Context =
    Context
    { ctxValues :: Values }

--------------------------------------------------------------------------------
data ExpectedType = ExpectedType String String

--------------------------------------------------------------------------------
data RespError
  = WrongArity Int Int
  | InvalidType [ExpectedType]

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
type Value = [Response Dynamic] -> Response Dynamic

--------------------------------------------------------------------------------
filterTypeRep :: [(TypeRep, Dynamic)] -> [ExpectedType]
filterTypeRep xs =
  [ ExpectedType (show tpe) (show tped)
      | (tpe, dyn) <- xs, let tped = dynTypeRep dyn, tpe /= tped ]

--------------------------------------------------------------------------------
strict :: Typeable a => a -> Value
strict a [] = return $ toDyn a
strict _ xs = throwError $ WrongArity 0 (length xs)

--------------------------------------------------------------------------------
strict1 :: forall a b. (Typeable a, Typeable b) => (a -> b) -> Value
strict1 f [xm] = do
    x <- xm
    case fromDynamic x of
        Just a -> return $ toDyn $ f a
        _      ->
            let tpee = show $ typeRep (Proxy :: Proxy a)
                tpea = show $ dynTypeRep x
                exp  = ExpectedType tpee tpea in
            throwError $ InvalidType [exp]
strict1 _ xs = throwError $ WrongArity 1 (length xs)

--------------------------------------------------------------------------------
strict2 :: forall a b c. (Typeable a, Typeable b, Typeable c)
        => (a -> b -> c)
        -> Value
strict2 f xs@[xm,ym] = do
    xs'@[am, bm] <- sequence xs
    case f <$> fromDynamic am <*> fromDynamic bm of
        Just c -> return $ toDyn c
        _      ->
            let exps = [ typeRep (Proxy :: Proxy a)
                       , typeRep (Proxy :: Proxy b)
                       ]
                tpes = filterTypeRep $ zip exps xs' in
            throwError $ InvalidType tpes

--------------------------------------------------------------------------------
strict3 :: forall a b c d. (Typeable a, Typeable b, Typeable c, Typeable d)
        => (a -> b -> c -> d)
        -> Value
strict3 f xs@[xm,ym,zm] = do
    xs'@[am, bm, cm] <- sequence xs
    case f <$>
         fromDynamic am <*>
         fromDynamic bm <*>
         fromDynamic cm of
        Just d -> return $ toDyn d
        _      ->
            let exps = [ typeRep (Proxy :: Proxy a)
                       , typeRep (Proxy :: Proxy b)
                       , typeRep (Proxy :: Proxy c)
                       ]
                tpes = filterTypeRep $ zip exps xs' in
            throwError $ InvalidType tpes

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

--------------------------------------------------------------------------------
exe :: Remote Int
exe =
  let summing = ref "sum" (Proxy :: Proxy ([Int] -> Int)) in
  remoteAp summing (local [1..3])
