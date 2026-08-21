{-# OPTIONS --safe #-}
module ToCmdLineArgs where

open import Class.Core
open import Class.Prelude

record ToCmdLineArgs (A : Type ℓ) : Type ℓ where
  constructor mkToCmdLineArgs
  field toCmdLineArgs : A → List String
open ToCmdLineArgs ⦃...⦄ public

ToCmdLineArgs¹ = ToCmdLineArgs ¹
ToCmdLineArgs² = ToCmdLineArgs ²
ToCmdLineArgs³ = ToCmdLineArgs ³

ToCmdLineArgs-List : ⦃ ToCmdLineArgs A ⦄ → ToCmdLineArgs (List A)
ToCmdLineArgs-List .toCmdLineArgs = concat ∘ (map toCmdLineArgs)
