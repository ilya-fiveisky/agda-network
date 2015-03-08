module Network.Primitive where

open import Data.Nat
open import Data.String
open import Data.Unit
open import Foreign.Haskell
open import IO.Primitive

postulate
  withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
  connectTo     : String → Integer → IO Handle

{-# IMPORT Network #-}
{-# IMPORT Text.Read #-}
{-# COMPILED withSocketsDo (\_ _ -> Network.withSocketsDo) #-}
{-# COMPILED connectTo (\s i -> Network.connectTo s $ Network.PortNumber (fromIntegral i)) #-}
