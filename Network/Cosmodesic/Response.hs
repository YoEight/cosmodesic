{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module : Network.Cosmodesic.Response
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Network.Cosmodesic.Response where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Dynamic

--------------------------------------------------------------------------------
import Control.Monad.Except

--------------------------------------------------------------------------------
import Network.Cosmodesic.Types

--------------------------------------------------------------------------------
newtype Simple a = Simple a

--------------------------------------------------------------------------------
newtype Effect a = Effect a

--------------------------------------------------------------------------------
newtype PureFn a = PureFn a

--------------------------------------------------------------------------------
simple :: a -> Simple a
simple = Simple

--------------------------------------------------------------------------------
effect :: a -> Effect a
effect = Effect

--------------------------------------------------------------------------------
pureFn :: a -> PureFn a
pureFn = PureFn

--------------------------------------------------------------------------------
filterTypeRep :: [(TypeRep, Dynamic)] -> [ExpectedType]
filterTypeRep xs =
  [ ExpectedType (show tpe) (show tped)
      | (tpe, dyn) <- xs, let tped = dynTypeRep dyn, tpe /= tped ]

--------------------------------------------------------------------------------
class Valuable a where
    valuable :: a -> [Response Dynamic] -> Response Dynamic

--------------------------------------------------------------------------------
instance Typeable a => Valuable (Simple a) where
    valuable (Simple a) [] = return $ toDyn a
    valuable _ xs          = throwError $ WrongArity 0 (length xs)

--------------------------------------------------------------------------------
instance (Typeable a, Typeable b) => Valuable (PureFn (a -> b)) where
    valuable (PureFn f) [xm] = do
        x <- xm
        case fromDynamic x of
            Just a -> return $ toDyn $ f a
            _      ->
                let tpee = show $ typeRep (Proxy :: Proxy a)
                    tpea = show $ dynTypeRep x
                    exp  = ExpectedType tpee tpea in
                throwError $ InvalidType [exp]
    valuable _ xs = throwError $ WrongArity 1 (length xs)

--------------------------------------------------------------------------------
instance ( Typeable a
         , Typeable b
         , Typeable c )
         => Valuable (PureFn (a -> b -> c)) where
    valuable (PureFn f) xs@[xm,ym] = do
        xs'@[am, bm] <- sequence xs
        case f <$> fromDynamic am <*> fromDynamic bm of
            Just c -> return $ toDyn c
            _      ->
                let exps = [ typeRep (Proxy :: Proxy a)
                           , typeRep (Proxy :: Proxy b)
                           ]
                    tpes = filterTypeRep $ zip exps xs' in
                throwError $ InvalidType tpes
    valuable _ xs = throwError $ WrongArity 2 (length xs)

--------------------------------------------------------------------------------
instance ( Typeable a
         , Typeable b
         , Typeable c
         , Typeable d )
         => Valuable (PureFn (a -> b -> c -> d)) where
    valuable (PureFn f) xs@[xm,ym,zm] = do
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
    valuable _ xs = throwError $ WrongArity 3 (length xs)
