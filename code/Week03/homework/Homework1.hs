{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       TxInfo (txInfoValidRange, txInfoSignatories), to,
                                       mkValidatorScript)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (traceIfFalse, Bool (..), elem)
import           Utilities            (wrap)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = 
    traceIfFalse "beneficiary's signature missing" checkBeneficiaryAndTime
    -- traceIfFalse "beneficiary's signature missing" checkSig      &&
    -- traceIfFalse "deadline not reached"            checkDeadline
    where
        txinf :: txInfo
        txinf = scriptContextTxInfo _ctx

        checkDeadline :: Bool
        checkDeadline = to (deadline _dat) `contains` txInfoValidRange info 

        isBeneficiary1 :: Bool
        isBeneficiary1 = beneficiary1 _dat `elem` txInfoSignatories txinf

        isBeneficiary2 :: Bool
        isBeneficiary2 = beneficiary2 _dat `elem` txInfoSignatories txinf

        checkBeneficiaryAndTime :: Bool
        checkBeneficiaryAndTime = 
            case checkDeadline of
                True    -> isBeneficiary1
                False   -> isBeneficiary2
                
            

    
    deadlineReached :: Bool
   deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
    
    signer      = frst txInfoSignatories txinf
    valid_range = txInfoValidRange txinf 
    member 30 $ from (30 :: Integer)

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
