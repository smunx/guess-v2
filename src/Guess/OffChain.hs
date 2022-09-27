
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


module Guess.OffChain where

--haskell
import qualified Control.Monad            as Monad (void)
import qualified Data.Map                 as Map
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)

--Plutus
import qualified Text.Printf              as TextPrintf (printf)
import qualified Prelude                  as P
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.Contract          as PlutusContract
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import           Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Data.Void                as Void (Void)
import           PlutusTx.Prelude
import qualified Data.Text                as DT (Text)
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Data.Text                as DataText (Text)

import           Ledger.Value
import           Ledger.Ada                 as Ada

import           Data.Default               (Default (..))
import qualified Data.Aeson as Aeson
import           Plutus.Trace.Emulator      as Emulator
import           Wallet.Emulator.Wallet
import           Wallet.Emulator.MultiAgent
import           Plutus.Trace.Emulator.Types
import           System.IO
import           Control.Monad.Freer.Extras as Extras
import           PlutusPrelude                   (void)
import qualified PlutusPrelude                   as PP

import qualified Guess.OnChain as OnChain
-- 1. Create utxos in the script address

--endpoints (contract monads)

data GiveParams = GiveParams
    {
        giveAmount :: !Integer,
        giveDat :: !Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, P.Show)

data GrabParams = GrabParams
    {
        grabRedeem :: !Integer
    } deriving (P.Eq, P.Ord, GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, P.Show)

type GiftSchema = 
    PlutusContract.Endpoint "give" GiveParams
    PlutusContract..\/ PlutusContract.Endpoint "grab" GrabParams

give :: PlutusContract.AsContractError e => GiveParams -> PlutusContract.Contract w s e ()
give gp = do
    pkh <- PlutusContract.ownPaymentPubKeyHash
    PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "----------------Give endpoint initialize--------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "------------------------------------------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf $ P.show gp
    PlutusContract.logInfo @P.String $ TextPrintf.printf $ P.show pkh

    let d = OnChain.Dat
            { 
                ddata = giveDat gp
            }
        q = giveAmount gp
        tx = Constraints.mustPayToOtherScript OnChain.validatorHash (ScriptsLedger.Datum $ PlutusTx.toBuiltinData d) (lovelaceValueOf q)
        lookups = Constraints.plutusV2OtherScript OnChain.validator
    submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Made transaction of %d adas" q

grab :: forall w s. GrabParams -> PlutusContract.Contract w s DataText.Text ()
grab GrabParams{..} = do
    maybeutxo <- findUtxoInValidator grabRedeem
    case maybeutxo of
        Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %d" grabRedeem
        Just (oref, o) -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s" (P.show oref)
            let 
                r = OnChain.Redeem 
                    {
                        OnChain.redeem = grabRedeem
                    }
                lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                          Constraints.plutusV2OtherScript OnChain.validator
                tx = Constraints.mustSpendScriptOutput oref $ ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r
            submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String $ "collected gifts"

getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    let datHashOrDatum = LedgerTx._ciTxOutScriptDatum o
    LedgerApiV2.Datum e <- snd datHashOrDatum
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Integer -> Bool
checkUTXO (oref, o) n = do
    case getDatum (oref, o) of
        Nothing -> False 
        Just OnChain.Dat{..}
            | ddata == n -> True 
            | otherwise -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref, o)] n = do
    if checkUTXO (oref, o) n then
        return (oref, o)
    else 
        Nothing
findUTXO ((oref, o):xs) n
    | checkUTXO (oref, o) n = return (oref, o)
    | otherwise = findUTXO xs n

findUtxoInValidator :: Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator n = do
    utxos <- PlutusContract.utxosAt OnChain.address
    let xs = [(oref, o) | (oref, o) <- Map.toList utxos]
        out = findUTXO xs n
    return out

endpoints :: PlutusContract.Contract () GiftSchema DT.Text ()
endpoints = PlutusContract.awaitPromise (give' `PlutusContract.select` grab') >> endpoints
    where 
        give' = PlutusContract.endpoint @"give" give
        grab' = PlutusContract.endpoint @"grab" $ grab




customShowEvent :: EmulatorEvent' -> Maybe P.String
customShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog (Aeson.String msg)) _ _)   -> Just $ "*** CONTRACT LOG: " <> P.show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> P.show err
  ev                                                                   -> Nothing 

traceConfig :: TraceConfig
traceConfig =
  TraceConfig
    customShowEvent
    stdout


w1, w2 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2

currSymbol :: CurrencySymbol
currSymbol = currencySymbol "dcddcaa"

tName :: TokenName
tName = tokenName "T1"

token :: AssetClass
token = AssetClass (currSymbol, tName)


test:: IO()
test = do
  test' 1


test' :: Integer  -> IO ()
-- test' c1  = runEmulatorTraceIO' def emCfg $ myTrace c1 
test' testID   = runEmulatorTraceIO' traceConfig emCfg $ myTrace testID  -- passing c1 as the parameter to the trace
  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue token 1)
                    , (w2, v)
                    ]
                }

    v :: Ledger.Value.Value
    v = Ada.lovelaceValueOf 1_000_000_000

myTrace :: Integer -> EmulatorTrace ()
myTrace testID   = do
    Extras.logInfo $ "Running myTrace with ParamOV as : " ++ P.show testID
    
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints

    let grabParams = GrabParams {
                       grabRedeem = 12
                     }

        giveParams = GiveParams{
                          giveAmount = 200_000_000
                        , giveDat = 12
                    }
        ppkh1 = mockWalletPaymentPubKeyHash w1
        ppkh2 = mockWalletPaymentPubKeyHash w2

    PP.void $ Emulator.waitNSlots 3
    callEndpoint @"give" h2 giveParams
    PP.void $ Emulator.waitNSlots 3

    Extras.logInfo $ "w1 ppkh : " ++ P.show ppkh1
    Extras.logInfo $ "w2 ppkh : " ++ P.show ppkh2    