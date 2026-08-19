{-# OPTIONS  --guardedness #-}

module Network.CURL where

open import Data.Bool using (if_then_else_)
open import Data.Default
open import Data.List using (List;  []; [_]; map)
open import Data.Product.Base using (_,_)
open import Data.String using (String; _++_)
open import Function using (_$_)
open import IO
open import System.Exit using (ExitCode; isSuccess)
open import System.Process using (callProcessWithExitCode; readProcessWithExitCode)
open import Network.CURL.Option

{-
-- Necessary for Windows detection (see note at https://learn.microsoft.com/en-us/windows/curl/).
-- Well... I found that it somehow works without .exe. I guess it's PowerShell issue only.
getCURLName : IO String
getCURLName = do
  ec ← callProcessWithExitCode "systeminfo" ["/?"]
  pure ("curl" ++ (if isSuccess ec then ".exe" else ""))
-}
record CallResult : Set where
  field
    exitCode : ExitCode
    stdOut : String
    stdErr : String

curl : {{stdin : WithDefault ""}} → List Option → IO CallResult
curl {{stdin}} opts = do
--  curlName ← getCURLName
  (exitCode , (stdOut , stdErr)) ← readProcessWithExitCode "curl" (map show opts) $ stdin .value
  pure $ record {exitCode = exitCode; stdOut = stdOut; stdErr = stdErr}
