{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,TxInfo (txInfoValidRange, txInfoSignatories),
                                       from, mkValidatorScript)
import           PlutusTx             (applyCode, compile, liftCode, )
import           PlutusTx.Prelude     (Bool (False), (.), (&&), elem, traceIfFalse)
import           Utilities            (wrap)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator _beneficiary _deadline () _ctx = 
    traceIfFalse "Validation PubKeyHash for the _beneficiary  Fail!!!" signedByBeneficiary &&
    traceIfFalse "Validation deadline Time Fail!!!" checkDeadlinePassed 

    where
        txinf :: TxInfo
        txinf = scriptContextTxInfo _ctx

        checkDeadlinePassed :: Bool
        checkDeadlinePassed = from (_deadline) `contains` txInfoValidRange txinf 
        
        signedByBeneficiary :: Bool
        signedByBeneficiary = _beneficiary `elem` txInfoSignatories txinf

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
