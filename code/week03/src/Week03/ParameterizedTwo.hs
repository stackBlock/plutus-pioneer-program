{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- added for the parameter feature
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.ParameterizedTwo where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

data VestingParam = VestingParam --changed, made it a param
    { beneficiary   :: PubKeyHash
    , deadline      :: Slot
    } deriving (Show)

PlutusTx.unstableMakeIsData ''VestingParam --changed to the param
PlutusTx.makeLift ''VestingParam -- added for the param


{-# INLINABLE mkValidator #-}
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool 
mkValidator vParam () () ctx =  -- changed, added vParam
    traceIfFalse "benificiary sig is missing" checkSig      &&
    traceIfFalse "deadline not reached" checkDeadline
  where
    info :: TxInfo 
    info = scriptContextTxInfo ctx

    checkSig :: Bool  
    checkSig = beneficiary vParam `elem` txInfoSignatories info -- chancged p

    checkDeadline = from (deadline vParam) `contains` txInfoValidRange info -- changed p

data Vesting 
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = () --changed
    type instance RedeemerType Vesting = ()

inst :: VestingParam -> Scripts.ScriptInstance Vesting -- changed to the param
inst vParam = Scripts.validator @Vesting 
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode vParam) -- add applycode and liftcode
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @() 

validator :: VestingParam -> Validator -- add the VestingParam
validator = Scripts.validatorScript . inst -- must compose (add the '.')

scrAddress :: VestingParam -> Ledger.Address -- add the VestingParam
scrAddress = scriptAddress . validator


-- data VestingParam = VestingParam
--     { beneficiary :: PubKeyHash
--     , deadline    :: Slot
--     } deriving Show

-- PlutusTx.unstableMakeIsData ''VestingParam
-- PlutusTx.makeLift ''VestingParam

-- {-# INLINABLE mkValidator #-}
-- mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
-- mkValidator p () () ctx =
--     traceIfFalse "beneficiary's signature missing" checkSig      &&
--     traceIfFalse "deadline not reached"            checkDeadline
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     checkSig :: Bool
--     checkSig = beneficiary p `elem` txInfoSignatories info

--     checkDeadline :: Bool
--     checkDeadline = from (deadline p) `contains` txInfoValidRange info

-- data Vesting
-- instance Scripts.ScriptType Vesting where
--     type instance DatumType Vesting = ()
--     type instance RedeemerType Vesting = ()

-- inst :: VestingParam -> Scripts.ScriptInstance Vesting
-- inst p = Scripts.validator @Vesting
--     ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @() @()

-- validator :: VestingParam -> Validator
-- validator = Scripts.validatorScript . inst

-- scrAddress :: VestingParam -> Ledger.Address
-- scrAddress = scriptAddress . validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" Slot

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let vParam  = VestingParam
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx = mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (inst vParam) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Slot -> Contract w s e ()
grab d = do
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    if now < d
        then logInfo @String $ "too early"
        else do
            let vParam = VestingParam
                        { beneficiary = pkh
                        , deadline    = d
                        }
            utxos <- utxoAt $ scrAddress vParam
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos      <>
                                  Constraints.otherScript (validator vParam)
                        tx :: TxConstraints Void Void
                        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                                  mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
