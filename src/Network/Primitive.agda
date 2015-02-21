module Network.Primitive where

open import Data.Nat
open import Data.String
open import Data.Unit
open import IO.Primitive

postulate
  Integer : Set
  withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
  connectTo     : String → String → IO Handle

{-# IMPORT Network #-}
{-# IMPORT Text.Read #-}
--{-# BUILTIN INTEGER Integer #-}
{-# COMPILED withSocketsDo (\_ _ -> Network.withSocketsDo) #-}
{-# COMPILED_TYPE Integer Integer  #-}
{-# COMPILED connectTo (\s is -> Network.connectTo s $ Network.PortNumber (fromIntegral (Text.Read.read is))) #-}
