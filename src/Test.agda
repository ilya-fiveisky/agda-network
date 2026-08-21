{-# OPTIONS  --guardedness #-}

module Test where

open import Data.Default using (default)
open import Data.List using ([]; _∷_)
open import Function using (_$_)
open import IO
open import Class.Show
open import Network.CURL

main : Main
main = run $ do
  r ← curl $ ？ "--url" ∷  ？ "https://www.example.com/" ∷ []
  putStrLn $ show r
  r ← curl $ d (str "name=xxx") ∷ ？ "--url" ∷ ？ "https://www.example.com/guest.cgi" ∷ []
  putStrLn $ show r
