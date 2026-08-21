{-# OPTIONS  --safe #-}

module Network.CURL.Option where

open import Data.Bool using (if_then_else_)
open import Data.List using ([]; [_]; _∷_)
open import Data.Product.Base using (_,_)
open import Data.String using (String; _++_; between; toList)
open import Function using (_$_; _∋_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
open import Relation.Nullary.Decidable.Core using (does)
open import ToCmdLineArgs

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
  ToCmdLineArgs-Content = ToCmdLineArgs Content ∋ λ where
    .toCmdLineArgs (str s) → [ quotesIfSpace s ]
    .toCmdLineArgs (file s) → [ "@" ++ quotesIfSpace s ]
    .toCmdLineArgs stdin → [ "@-" ]

_ : toCmdLineArgs (str "x y") ≡ [ "\"x y\"" ]; _ = refl
_ : toCmdLineArgs stdin ≡ [ "@-" ]; _ = refl

data Option : Set where
  ？ : String → Option -- just for raw command line args. Examples: (？ "--help") or (？ "https://www.example.com/")
  data′ : Content → Option
  data-binary : Content → Option

pattern d x = data′ x
pattern db x = data-binary x

instance
  ToCmdLineArgs-Option = ToCmdLineArgs Option ∋ λ where
    .toCmdLineArgs (？ s) → [ s ]
    .toCmdLineArgs (d c) → "--data" ∷  toCmdLineArgs c
    .toCmdLineArgs (db c) → "--data-binary" ∷  toCmdLineArgs c

_ : toCmdLineArgs (d (str "some data")) ≡ "--data" ∷ "\"some data\"" ∷ []; _ = refl
_ : toCmdLineArgs (db (str "binary data")) ≡ "--data-binary" ∷ "\"binary data\"" ∷ []; _ = refl
_ : toCmdLineArgs (db (file "filename")) ≡ "--data-binary" ∷ "@filename" ∷ []; _ = refl

