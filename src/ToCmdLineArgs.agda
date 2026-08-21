{-# OPTIONS --safe #-}
module ToCmdLineArgs where

open import Class.Prelude

record ToCmdLineArgs (A : Type ℓ) : Type ℓ where
  constructor mkToCmdLineArgs
  field toCmdLineArgs : A → List String
open ToCmdLineArgs ⦃...⦄ public

