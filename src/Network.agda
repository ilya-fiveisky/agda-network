module Network where

open import IO
import Network.Primitive as Prim

withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
withSocketsDo io = lift (Prim.withSocketsDo (run io))
