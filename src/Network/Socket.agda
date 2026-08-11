{-# OPTIONS  --guardedness #-}

module Network.Socket where

open import Data.Fin hiding (lift; _<_)
open import Function
open import Data.Nat
open import Data.Nat.Show
open import Data.String hiding (show; map)
open import Data.Vec hiding (_++_)
open import IO
open import IO.Primitive.Handle using (Handle)
import Network.Socket.Primitive as NSP

withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
withSocketsDo io = lift (NSP.withSocketsDo (run io))

byteSize : ℕ
byteSize = 256

wordSize : ℕ
wordSize = byteSize * byteSize

data IPAddress : Set where
 IPv4 : Vec (Fin byteSize) 4 → IPAddress
 IPv6 : Vec (Fin wordSize) 8 → IPAddress

{-
private
  showIPVec : {m n : ℕ} → Vec (Fin m) n → String
  showIPVec ip = foldl₁ (λ x y → x ++ "." ++ y) (map (show ∘ toℕ) ip)
-}

showIP : IPAddress → String
showIP (IPv4 ip) = foldl₁ (λ x y → x ++ "." ++ y) (map (show ∘ toℕ) ip)
showIP (IPv6 ip) = foldl₁ (λ x y → x ++ ":" ++ y) (map ((showInBase 16) ∘ toℕ) ip)

data PortNumber : Set where
  portNum : Fin wordSize → PortNumber

showPort : PortNumber → String
showPort (portNum n) = show (toℕ n)

connectTo : IPAddress → PortNumber → IO Handle
connectTo ip (portNum n) = lift (NSP.connectTo (showIP ip) (toℕ n))
