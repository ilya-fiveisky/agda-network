{-# OPTIONS  --guardedness #-}

module Network.CURL where

open import Data.Bool using (if_then_else_)
open import Data.List using (List;  []; [_]; map)
open import Data.Product.Base using (_×_)
open import Data.String using (String; _++_)
open import IO
open import System.Exit using (ExitCode; isSuccess)
open import System.Process using (callProcessWithExitCode; readProcessWithExitCode)

data CURLOption : Set where
  - : String → CURLOption -- just for raw command line args. Examples: (- "--help") or (- "https://www.example.com/")

show : CURLOption → String
show (- s) = s

-- Necessary for Windows detection (see note at https://learn.microsoft.com/en-us/windows/curl/)
getCURLName : IO String
getCURLName = do
  ec ← callProcessWithExitCode "systeminfo" ["/?"]
  pure ("curl" ++ (if isSuccess ec then ".exe" else ""))

curl : List CURLOption → String → IO (ExitCode × String × String)
curl opts stdin = do
  curlName ← getCURLName
  readProcessWithExitCode curlName (map show opts) stdin
