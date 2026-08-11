{-# OPTIONS  --guardedness #-}

module Test where

open import Function.Base using (_$_)
open import IO

open import Network.CURL using (curl)

main : Main
main = run $ do
  s ← curl
  putStrLn s
