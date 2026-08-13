{-# OPTIONS  --guardedness #-}

module Test where

open import Data.Default using (default)
open import Data.List using ([]; _∷_)
open import Function using (_$_)
open import IO

open import Network.CURL

open CallResult

main : Main
main = run $ do
  r ← curl $ ？ "--url" ∷  ？ "https://www.example.com/" ∷ []
  putStrLn $ r .stdOut
