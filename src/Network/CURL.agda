{-# OPTIONS  --guardedness #-}

module Network.CURL where

open import Data.List.Base using ([])
open import Data.String using (String)
open import Function.Base using (_$_; case_of_)
open import IO
open import System.Process using (readProcess)

curlCmd = "curl.exe"

curl : IO String
curl = readProcess curlCmd [] ""
