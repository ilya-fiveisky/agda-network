{-# OPTIONS  --guardedness #-}

module Network.CURL where

open import Data.Bool using (if_then_else_)
open import Data.Default
open import Data.Integer.Show renaming (show to intShow)
open import Data.List using (List;  []; [_]; map)
open import Data.Product.Base using (_,_)
open import Data.String using (String; _++_; _<+>_; unwords)
open import Function using (_$_; _∋_)
open import IO
open import System.Exit using (ExitCode; ExitSuccess; ExitFailure; isSuccess)
open import System.Process using (callProcessWithExitCode; readProcessWithExitCode)

open import Class.Show

open import Network.CURL.Option public

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
    cmdLine : String
    exitCode : ExitCode
    stdOut : String
    stdErr : String
open CallResult

instance
  Show-ExitCode = Show ExitCode ∋ λ where
    .show ExitSuccess → "0"
    .show (ExitFailure i) → intShow i

instance
  Show-CallResult = Show CallResult ∋ λ where
    .show r →
      "cmdLine=" <+> r .cmdLine ++ "\n" ++
      "exitCode=" <+> show (r .exitCode) ++ "\n" ++
      "stdOut:\n" ++ r .stdOut  ++ "\n" ++
      "stdErr:\n" ++ r .stdErr

curlName = "curl"

curl : {{stdi : WithDefault ""}} → List Option → IO CallResult
curl {{stdi}} opts = do
--  curlName ← getCURLName
  let strOpts = map show opts
  let optsStr =  unwords strOpts
  (exitCode , (stdOut , stdErr)) ← readProcessWithExitCode curlName strOpts $ stdi .value
  pure $ record {exitCode = exitCode; stdOut = stdOut; stdErr = stdErr; cmdLine = curlName <+> optsStr}
