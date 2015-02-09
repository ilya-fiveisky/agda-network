module Network.Primitive where

open import Data.String
open import Data.Unit
open import IO.Primitive

postulate
  withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A

{-# IMPORT Network #-}
{-# COMPILED withSocketsDo (\_ _ -> Network.withSocketsDo) #-}
