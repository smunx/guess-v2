{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Guess.OffChain where

import PlutusTx.Prelude (Integer)
import PlutusTx qualified

import Data.Void (Void)
import Prelude (String, IO, return, ($), (<>))
import Data.Text (Text)
import Data.Map qualified as Map
import Text.Printf (printf)
import Data.Functor (void)
import Control.Monad.Freer.Extras (logInfo)
import Data.Default (def)

import Plutus.Trace.Emulator (
    EmulatorTrace,
    runEmulatorTraceIO',
    activateContractWallet,
    waitNSlots)
import Plutus.Contract.Test (w1, w2)
import Plutus.Contract (
    Contract, EmptySchema, awaitTxConfirmed, submitTxConstraintsWith,
    utxosAt)
import Plutus.Contract qualified as Contract
import Ledger.Tx (getCardanoTxId)

import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (
    mustPayToOtherScript,
    otherScript,
    mustSpendScriptOutput,
    unspentOutputs)
import Plutus.V2.Ledger.Api (Datum(..), Redeemer (..))

import Guess.OnChain qualified as OnChain

go :: IO ()
go = runEmulatorTraceIO' def def trace

trace :: EmulatorTrace ()
trace = do
    logInfo @String "Hello!"
    void $ activateContractWallet w1 (lock 0)
    logInfo @String "Locked!"
    void $ waitNSlots 3
    void $ activateContractWallet w2 (guess 1)
    logInfo @String "Guessed!"

lock :: Integer -> Contract () EmptySchema Text ()
lock i = do
    let dat = Datum $ PlutusTx.toBuiltinData $ OnChain.Dat i
    let val = lovelaceValueOf 5_000_000
    let tx = mustPayToOtherScript OnChain.hash dat val
    let lookups = otherScript OnChain.validator
    stx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId stx
    Contract.logInfo @String "Locked lovelace!" 

guess :: Integer -> Contract () EmptySchema Text ()
guess i = do
    utxos <- utxosAt OnChain.address
    let xs = Map.toList utxos
    case xs of 
        [] -> Contract.logInfo @String "No UTxOs to spend."
        (oref, txo) : _ -> do
            let rdr = Redeemer $ PlutusTx.toBuiltinData $ OnChain.Redeem i
            let tx = mustSpendScriptOutput oref rdr
            let lookups = otherScript OnChain.validator <> unspentOutputs (Map.singleton oref txo)
            stx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId stx
            Contract.logInfo @String "Guessed!"
            -- try using typesafe validator in Onchain and also change @Void here to @Simple

