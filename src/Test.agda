{-# OPTIONS  --guardedness #-}

module Test where

open import Data.Default using (default)
open import Data.List using ([]; _∷_)
open import Data.Product.Base using (proj₁; proj₂)
open import Function using (_$_)
open import IO

open import Network.CURL

main : Main
main = run $ do
  result ← curl (- "https://www.example.com/" ∷ [])
  putStrLn $ proj₁ $ proj₂ result
