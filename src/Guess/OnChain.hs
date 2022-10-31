--1 Extensions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ImportQualifiedPost #-}

--This is to work not only with Strings
{-# LANGUAGE OverloadedStrings   #-}

-- required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}


module Guess.OnChain where

import PlutusTx qualified

import PlutusTx.Prelude (
    ($),
    (==),
    Bool(..), 
    Integer,
    traceIfFalse)

import Ledger (
    scriptHashAddress,
    Versioned(..),
    Language (..),
    toValidatorHash)

import Plutus.V2.Ledger.Api (
    Address,
    ValidatorHash,
    ScriptContext(..),
    Validator,
    mkValidatorScript)

import Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)

import Prelude(Show(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (fromJust)

data Redeem = Redeem Integer
    deriving (Generic, FromJSON, ToJSON, Show)
PlutusTx.makeIsDataIndexed ''Redeem [('Redeem, 0)]

data Dat = Dat Integer
    deriving (Generic, FromJSON, ToJSON, Show)
PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

{-# INLINABLE simple #-}
simple :: Dat -> Redeem -> ScriptContext -> Bool
simple (Dat i) (Redeem j) _ = traceIfFalse "Sorry the guess is not correct" (i == j)

validator :: Validator
validator = mkValidatorScript 
              $ $$(PlutusTx.compile [|| wrap ||])
    where wrap = mkUntypedValidator simple

address :: Address
address = mkValidatorAddress validator

hash :: ValidatorHash
hash = fromJust $ toValidatorHash address