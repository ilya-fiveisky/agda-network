module Network.Primitive where

open import Data.String
open import Data.Unit
open import IO.Primitive

postulate
  withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
  bracket : ∀ {a b c} {A : Set a} {B : Set b} {C : Set c} → IO A → (A → IO B) → (A → IO C) → IO C

{-# IMPORT Network #-}
{-# IMPORT Control.Exception.Base #-}
{-# COMPILED withSocketsDo (\_ _ -> Network.withSocketsDo) #-}
{-# COMPILED bracket (\_ _ _ _ _ _ -> Control.Exception.Base.bracket) #-}
