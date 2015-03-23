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
filterTypeRep :: [(TypeRep, Object)] -> [ExpectedType]
filterTypeRep xs =
  [ ExpectedType (show tpe) (show tped)
      | (tpe, obj) <- xs, let tped = objectRep obj, tpe /= tped ]

--------------------------------------------------------------------------------
class Valuable a where
    valuable :: a -> Value

--------------------------------------------------------------------------------
instance Serializable a => Valuable (Simple a) where
    valuable (Simple a) [] = return $ object a
    valuable _ xs          = throwError $ WrongArity 0 (length xs)

--------------------------------------------------------------------------------
instance (Serializable a, Serializable b) => Valuable (PureFn (a -> b)) where
    valuable (PureFn f) [xm] = do
        x <- xm
        case fromObject x of
            Just a -> return $ object $ f a
            _      ->
                let tpee = show $ typeRep (Proxy :: Proxy a)
                    tpea = show $ objectRep x
                    exp  = ExpectedType tpee tpea in
                throwError $ InvalidType [exp]
    valuable _ xs = throwError $ WrongArity 1 (length xs)

-- --------------------------------------------------------------------------------
-- instance ( Serializable a
--          , Serializable b
--          , Serializable c )
--          => Valuable (PureFn (a -> b -> c)) where
--     valuable (PureFn f) xs@[xm,ym] = do
--         xs'@[am, bm] <- sequence xs
--         case f <$> fromObject am <*> fromObject bm of
--             Just c -> return $ object c
--             _      ->
--                 let exps = [ typeRep (Proxy :: Proxy a)
--                            , typeRep (Proxy :: Proxy b)
--                            ]
--                     tpes = filterTypeRep $ zip exps xs' in
--                 throwError $ InvalidType tpes
--     valuable _ xs = throwError $ WrongArity 2 (length xs)

-- --------------------------------------------------------------------------------
-- instance ( Serializable a
--          , Serializable b
--          , Serializable c
--          , Serializable d )
--          => Valuable (PureFn (a -> b -> c -> d)) where
--     valuable (PureFn f) xs@[xm,ym,zm] = do
--         xs'@[am, bm, cm] <- sequence xs
--         case f <$>
--              fromObject am <*>
--              fromObject bm <*>
--              fromObject cm of
--             Just d -> return $ object d
--             _      ->
--                 let exps = [ typeRep (Proxy :: Proxy a)
--                            , typeRep (Proxy :: Proxy b)
--                            , typeRep (Proxy :: Proxy c)
--                            ]
--                     tpes = filterTypeRep $ zip exps xs' in
--                 throwError $ InvalidType tpes
--     valuable _ xs = throwError $ WrongArity 3 (length xs)
