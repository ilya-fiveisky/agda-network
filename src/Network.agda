module Network where

open import Data.Fin hiding (lift; _<_)
open import Function
open import Data.Nat
open import Data.Nat.Show
open import Data.String hiding (show)
open import Data.Vec hiding (_++_)
open import Foreign.Haskell
open import IO
import IO.Primitive as IOPrim
import Network.Primitive as NetPrim

withSocketsDo : ∀ {a} {A : Set a} → IO A → IO A
withSocketsDo io = lift (NetPrim.withSocketsDo (run io))

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

connectTo : IPAddress → PortNumber → IO IOPrim.Handle
connectTo ip (portNum n) = lift (NetPrim.connectTo (showIP ip) (toInteger (toℕ n)))
