module Network.Primitive where

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.String using (String)
open import IO.Primitive.Core using (IO)
open import IO.Primitive.Handle using (Handle)

{-# FOREIGN GHC
import Network
import Text.Read
#-}

postulate
  withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
  connectTo     : String → Nat → IO Handle

{-# COMPILE GHC withSocketsDo (\_ _ -> Network.withSocketsDo) #-}
{-# COMPILE GHC connectTo (\s i -> Network.connectTo s $ Network.PortNumber (fromIntegral i)) #-}
