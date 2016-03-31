{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{- |
   Module      : Control.Quiver.Binary
   Description : Support Binary inside Quiver
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Control.Quiver.Binary (
    -- * Simple encoding\/decoding
    spput
  , spget
    -- * Conversions to\/from ByteStrings
  , spdecode
  , spdecodeL
  , spencode
  , spencodeL
  ) where

import Control.Quiver.ByteString
import Control.Quiver.SP

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L

--------------------------------------------------------------------------------

-- | Encode all values.
spput :: (Binary a) => SConsumer a PutM e
spput = sptraverse_ put

-- | Decode all values.
spget :: (Binary a) => SProducer a Get e
spget = spuntilM isEmpty get

spuntilM :: (Monad m) => m Bool -> m a -> SProducer a m e
spuntilM chk prod = go
  where
    go = do done <- qlift chk
            if done
               then spcomplete
               else do a <- qlift prod
                       spemit a
                       go

--------------------------------------------------------------------------------

-- | Decode all values from the provided stream of strict ByteStrings.  Note
-- that the error message does not return the 'ByteOffset' from the
-- 'Decoder' as it will probably not match the actual location of the
-- source ByteString.
spdecode :: forall a m. (Binary a, Monad m) => SP ByteString a m String
spdecode = runDecode nextD
  where
    nextD :: Decoder a
    nextD = runGetIncremental get

    runDecode :: Decoder a -> SP ByteString a m String
    runDecode d = case d of
                    Fail b' _ err -> if B.null b'
                                        then spcomplete -- No more input! (should probably also check the offset)
                                        else spfailed err
                    Partial p     -> spfetch >>= runDecode . p
                    Done b' o a   -> a >:> runDecode (nextD `pushChunk` b')

-- TODO: consider using state internally to keep track of the offset throughout the entire thing

-- | Decode all values from the provided stream of lazy ByteStrings.  Note
-- that the error message does not return the 'ByteOffset' from the
-- 'Decoder' as it will probably not match the actual location of the
-- source ByteString.
spdecodeL :: forall a m. (Binary a, Monad m) => SP L.ByteString a m String
spdecodeL = toChunks >->> spdecode >&> snd

-- | Encode all values to a stream of strict ByteStrings.
spencode :: (Binary a, Functor m) => SP a ByteString m ()
spencode = spencodeL >->> toChunks >&> fst

-- | Encode all values to a stream of lazy ByteStrings.
spencodeL :: (Binary a) => SP a L.ByteString m ()
spencodeL = sppure encode
