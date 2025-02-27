{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week03.VestingTwo where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON, Value (Bool))
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

data VestingDatum = VestingDatum    -- creating datum
    { beneficiary   :: PubKeyHash   -- make sure ada goes to correct wallet
    , deadline      :: Slot         -- make sure it is available after a certain slot (time)
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum  -- allows plutus to use VestingDatum data

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool 
mkValidator dat () ctx =  
    traceIfFalse "Benificiary's signature is nowhere to be found!" checkSig -- using 'checksig' function
    &&
    traceIfFalse "Come on, It's not time yet!" checkDeadline -- using 'checkDeadline' function
  where
    info :: TxInfo 
    info = scriptContextTxInfo ctx 

    checkSig :: Bool    -- maing sure that receiver wallet has the correct public key hash (inputed public key)
    checkSig = beneficiary dat `elem` txInfoSignatories (scriptContextTxInfo ctx)
                                                        -- using (scriptContextTxInfo ctx) 
                                                        -- insted of info (look at info function above)
    checkDeadline :: Bool   -- make sure the slot is after the current slot   
                            -- 'from' current block chain slot until dealine dat (inputed deadline)                           
    checkDeadline = to (deadline dat) `contains` txInfoValidRange info 

-- *** BOILERPLATE ***
data Vesting 
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

-- *** BOILERPLATE ***
inst :: Scripts.ScriptInstance Vesting 
inst = Scripts.validator @Vesting 
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

-- *** BOILERPLATE ***
validator :: Validator 
validator = Scripts.validatorScript inst

-- *** BOILERPLATE ***
scrAddress :: Ledger.Address 
scrAddress = scriptAddress validator 


-- data VestingDatum = VestingDatum
--     { beneficiary :: PubKeyHash
--     , deadline    :: Slot
--     } deriving Show

-- PlutusTx.unstableMakeIsData ''VestingDatum

-- {-# INLINABLE mkValidator #-}
-- mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
-- mkValidator dat () ctx =
--     traceIfFalse "beneficiary's signature missing" checkSig      &&
--     traceIfFalse "deadline not reached"            checkDeadline
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     checkSig :: Bool
--     checkSig = beneficiary dat `elem` txInfoSignatories info

--     checkDeadline :: Bool
--     checkDeadline = from (deadline dat) `contains` txInfoValidRange info

-- data Vesting
-- instance Scripts.ScriptType Vesting where
--     type instance DatumType Vesting = VestingDatum
--     type instance RedeemerType Vesting = ()

-- inst :: Scripts.ScriptInstance Vesting
-- inst = Scripts.validator @Vesting
--     $$(PlutusTx.compile [|| mkValidator ||])
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @VestingDatum @()

-- validator :: Validator
-- validator = Scripts.validatorScript inst

-- scrAddress :: Ledger.Address
-- scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter (isSuitable pkh now) <$> utxoAt scrAddress
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                          mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PubKeyHash -> Slot -> TxOutTx -> Bool
    isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> False
                Just d  -> beneficiary d == pkh && deadline d >= now

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []