{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Ledger
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a   
    -- 
-- EmulatorTrace a
    -- emulator trace lets you run code and test code without starting the playground
    -- emulator trace what we do manually in the simulator tab of the playground

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints 
    h2 <- activateContractWallet (Wallet 2) endpoints 
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2 
        , gpDeadline    = Slot 20
        , gpAmount      = 1000
        }
    void $ waitUntilSlot 20  -- 'void' just ignores warning 
    callEndpoint @"grab" h2 ()
 -- void $ waitNSlots 1      -- 'void' just ignores warning 
    s <- waitNSlots 1
    Extras.logInfo $ "reached slot " ++ show s
    
    
































-- test :: IO ()
-- test = runEmulatorTraceIO myTrace

-- myTrace :: EmulatorTrace ()
-- myTrace = do
--     h1 <- activateContractWallet (Wallet 1) endpoints
--     h2 <- activateContractWallet (Wallet 2) endpoints
--     callEndpoint @"give" h1 $ GiveParams
--         { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
--         , gpDeadline    = Slot 20
--         , gpAmount      = 1000
--         }
--     void $ waitUntilSlot 20
--     callEndpoint @"grab" h2 ()
--     s <- waitNSlots 1
--     Extras.logInfo $ "reached slot " ++ show s
