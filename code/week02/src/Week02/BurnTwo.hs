{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-} -- prevents normal haskell Prelude from being imported
{-# LANGUAGE OverloadedStrings   #-} -- alows us to use string literals
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.BurnTwo where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract     hiding (when)
import           PlutusTx            (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude    hiding (Semigroup(..), unless) --imported instead of regular Prelude
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (Semigroup (..)) -- Imported one function for special reason.
import           Text.Printf         (printf)


    -- INLINABLE makes the fuction able to be used by Temlate Haskell via the validator function
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ r _ 
    | r == I 42 = ()
    | otherwise = traceError "Wrong reedemer"


-- {-# INLINABLE mkValidator #-}
-- mkValidator :: Data -> Data -> Data -> ()
-- mkValidator _ _ _ = traceError "NO WAY!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash

type GiftSchema =
    BlockchainActions
        .\/ Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
grab r = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>=  grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
