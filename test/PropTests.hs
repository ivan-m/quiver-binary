{-# LANGUAGE RankNTypes #-}
{- |
   Module      : Main
   Description : Properties for quiver-binary
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import Control.Quiver.Binary

import Control.Quiver.SP
import Data.Functor.Identity

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $
  describe "decode . encode = id" $ do
    prop "strict ByteStrings" $
      forAllShrink (arbitrary :: Gen [Int]) shrink $ idProp spencode  spdecode
    prop "lazy ByteStrings" $
      forAllShrink (arbitrary :: Gen [Int]) shrink $ idProp spencodeL spdecodeL

idProp :: (Eq a) => SP a b Identity () -> SP b a Identity String -> [a] -> Bool
idProp enc dec as = runIdentity (sprun pipeline) == as
  where
    pipeline = spevery as >->> enc >->> dec >->> spToList >&> snd

spToList :: SQ a x f [a]
spToList = spfoldr (:) []
