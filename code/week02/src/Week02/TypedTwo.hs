{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.TypedTwo where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract      hiding (when)
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

{-# INLINABLE mkValidator#-}
mkValidator :: () -> Integer -> ValidatorCtx -> Bool
mkValidator() r _ 
    | r == 42 = True 
    | otherwise = False

-- *** BOILERPLATE ***
data Typed
instance Scripts.ScriptType Typed where
    type instance DatumType Typed       = () -- name given to Datum in mkValidator @ line 33
    type instance RedeemerType Typed    = Integer -- name given to Redeemer in mkValidator @ line 33


-- *** BOILERPLATE ***
inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed -- from line 39 above
    $$(PlutusTx.compile [|| mkValidator ||]) -- from mkValidator @ line 33
    $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.wrapValidator @() @Integer -- from line 33 above

-- *** BOILERPLATE ***
validator :: Validator 
validator = Scripts.validatorScript inst -- inst function from line 47










-- {-# INLINABLE mkValidator #-}
-- mkValidator :: () -> Integer -> ValidatorCtx -> Bool
-- mkValidator () r _
--     | r == 42   = True
--     | otherwise = False

-- data Typed
-- instance Scripts.ScriptType Typed where
--     type instance DatumType Typed = ()
--     type instance RedeemerType Typed = Integer

-- inst :: Scripts.ScriptInstance Typed
-- inst = Scripts.validator @Typed
--     $$(PlutusTx.compile [|| mkValidator ||])
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @() @Integer

-- validator :: Validator
-- validator = Scripts.validatorScript inst

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
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints inst tx
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
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
