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

pattern str s = string s

import Data.Char.Properties as Char using (_≟_)
open import Data.List.Membership.DecPropositional Char._≟_
open import Relation.Nullary.Decidable.Core using (does)

-- enclose string with " if it contains a space character
quotesIfSpace : String → String
quotesIfSpace s = if does (' ' ∈? toList s) then between "\"" "\"" s else s

_ : quotesIfSpace "x y" ≡ "\"x y\""; _ = refl

instance
  Show-Content = Show Content ∋ λ where
    .show (str s) → quotesIfSpace s
    .show (file s) → "@" ++ quotesIfSpace s
    .show stdin → "@-"

_ : show (str "x y") ≡ "\"x y\""; _ = refl
_ : show stdin ≡ "@-"; _ = refl

data Data : Set where
  ascii binary : Content → Data

pattern a c = ascii c
pattern b c = binary c

instance
  Show-Data = Show Data ∋ λ where
    .show (a c) → "-ascii" <+> show c
    .show (b c) → "-binary" <+> show c

_ : show (a (str "some data")) ≡ "-ascii \"some data\""; _ = refl

data Option : Set where
  ？ : String → Option -- just for raw command line args. Examples: (？ "--help") or (？ "https://www.example.com/")
  data′ : Data → Option
  d : Content → Option

--pattern d x = data′ x

instance
  Show-Option = Show Option ∋ λ where
    .show (？ s) → s
    .show (data′ dat) → "--data" ++ show dat
    .show (d c) → "-d" <+> show c

_ : show (data′ (a (str "ascii data"))) ≡ "--data-ascii \"ascii data\""; _ = refl
_ : show (d (str "data")) ≡ "-d data"; _ = refl
_ : show (data′ (b (str "binary data"))) ≡ "--data-binary \"binary data\""; _ = refl
_ : show (data′ (b (file "filename"))) ≡ "--data-binary @filename"; _ = refl

