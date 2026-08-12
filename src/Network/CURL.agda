{-# OPTIONS  --guardedness #-}

module Network.CURL where

open import Data.Bool using (if_then_else_)
open import Data.List using ([]; [_])
open import Data.String using (String; _++_)
open import IO
open import System.Exit using (isSuccess)
open import System.Process using (callProcessWithExitCode; readProcess)

-- Necessary for Windows detection (see note at https://learn.microsoft.com/en-us/windows/curl/)
getCurlName : IO String
getCurlName = do
  ec ← callProcessWithExitCode "systeminfo" ["/?"]
  pure ("curl" ++ (if isSuccess ec then ".exe" else ""))

curl : IO String
curl = do
  curlName ← getCurlName
  readProcess curlName [] ""
