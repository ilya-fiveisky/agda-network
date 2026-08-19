{-# OPTIONS  --safe #-}

module Network.CURL.Option where

open import Data.Bool using (if_then_else_)
open import Data.Maybe using (just)
open import Data.Product.Base using (_,_)
open import Data.String using (String; _++_;  _<+>_; between; toList)
open import Function using (_$_; _∋_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary.Decidable.Core using (does)

open import Class.Show

data Content : Set where
  string file : String → Content
  stdin : Content

import Data.Char.Properties as Char using (_≟_)
open import Data.List.Membership.DecPropositional Char._≟_
open import Relation.Nullary.Decidable.Core using (does)

-- enclose string with " if it contains a space character
quotesIfSpace : String → String
quotesIfSpace s = if does (' ' ∈? toList s) then between "\"" "\"" s else s

_ : quotesIfSpace "x y" ≡ "\"x y\""
_ = refl

instance
  Show-Content = Show Content ∋ λ where
    .show (string s) → quotesIfSpace s
    .show (file s) → "@" ++ quotesIfSpace s
    .show stdin → "@-"

_ : show (string "x y") ≡ "\"x y\""
_ = refl

_ : show stdin ≡ "@-"
_ = refl

data Data : Set where
  ascii binary : Content → Data

instance
  Show-Data = Show Data ∋ λ where
    .show (ascii c) → "-d" <+> show c
    .show (binary c) → "--data-binary" <+> show c

_ : show (ascii (string "some data")) ≡ "-d \"some data\""
_ = refl

data Option : Set where
  ？ : String → Option -- just for raw command line args. Examples: (？ "--help") or (？ "https://www.example.com/")
  data′ d : Data → Option
{-
show : Option → String
show (？ s) = s
show (data′ (ascii (string s))) = "-d " ++ quotesIfSpace s

show (d x) = show $ data′ x
-}
