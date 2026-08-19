{-# OPTIONS  --guardedness #-}

module Network.CURL.Option where

open import Data.List using (List;  []; [_]; map)
open import Data.Product.Base using (_,_)
open import Data.String using (String; _++_)
open import Function using (_$_)

data Option : Set where
  ？ : String → Option -- just for raw command line args. Examples: (？ "--help") or (？ "https://www.example.com/")
 
show : Option → String
show (？ s) = s
