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
  -- For some weird reason all full --data options don't work (just -d works).
  -- Nevertheless directly in terminal copied curl cmdLine works.
  r ← curl $ d (str "name=xxx") ∷ ？ "--url" ∷ ？ "https://www.example.com/guest.cgi" ∷ []
  putStrLn $ show r
  r ← curl $ data′ (a (str "name=xxx")) ∷ ？ "https://www.example.com/guest.cgi" ∷ []
  putStrLn $ show r 
