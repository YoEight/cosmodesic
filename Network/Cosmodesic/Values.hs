--------------------------------------------------------------------------------
-- |
-- Module : Network.Cosmodesic.Values
-- Copyright : (C) 2015 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Network.Cosmodesic.Values where

--------------------------------------------------------------------------------
import Control.Monad
import Data.ByteString
import Data.Monoid

--------------------------------------------------------------------------------
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
import Network.Cosmodesic.Response
import Network.Cosmodesic.Types

--------------------------------------------------------------------------------
type Register = M.Map ByteString Value

--------------------------------------------------------------------------------
data RegError = AlreadyBounded ByteString

--------------------------------------------------------------------------------
newtype Declarations = Decls (Register -> Either RegError Register)

--------------------------------------------------------------------------------
instance Monoid Declarations where
    mempty = Decls Right

    mappend (Decls kl) (Decls kr) = Decls (kl >=> kr)

--------------------------------------------------------------------------------
declare :: Valuable v => ByteString -> v -> Declarations
declare n v = Decls $ \rg ->
    if M.member n rg
    then Left (AlreadyBounded n)
    else Right $ M.insert n (valuable v) rg

--------------------------------------------------------------------------------
emptyValues :: Values
emptyValues = Values M.empty

--------------------------------------------------------------------------------
register :: Declarations -> Values -> Either RegError Values
register (Decls k) (Values m) = fmap Values $ k m
