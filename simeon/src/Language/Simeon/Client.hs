{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Simeon.Client where
import           Control.Lens
import           Control.Monad                (forM_, void)
import           Control.Monad.Error.Lens     (catching, throwing)
import           Data.Aeson                   (FromJSON, ToJSON, parseJSON, toJSON)
import           Data.Default                 (Default (def))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (isNothing, maybeToList)
import           Data.Monoid                  (First (..))
import           Data.Semigroup.Generic       (GenericSemigroupMonoid (..))
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import           Data.Void                    (absurd)
import           GHC.Generics                 (Generic)
import           Language.Simeon.Semantics   hiding (Contract)
import qualified Language.Simeon.Semantics   as Simeon
import           Language.Simeon.Util        (extractContractRoles)
import           Ledger                       (CurrencySymbol, Datum (..), PubKeyHash, ScriptContext (..), Slot (..),
                                               TokenName, TxOut (..), ValidatorHash, inScripts, mkValidatorScript,
                                               pubKeyHash, txOutValue, validatorHash, valueSpent)
import qualified Ledger
import           Ledger.Bcc                   (bccSymbol, bccValueOf)
import           Ledger.Address               (pubKeyHashAddress, scriptHashAddress)
import           Ledger.Constraints
import qualified Ledger.Constraints           as Constraints
import qualified Ledger.Interval              as Interval
import           Ledger.Scripts               (Validator, datumHash, unitRedeemer)
import qualified Ledger.TimeSlot              as TimeSlot
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx              (TypedScriptTxOut (..), tyTxOutData)
import qualified Ledger.Value                 as Val
import           Zerepoch.ChainIndex            (_ValidTx, citxInputs, citxOutputs, citxTxId)
import           Zerepoch.Contract
import           Zerepoch.Contract.StateMachine (AsSMContractError (..), StateMachine (..), StateMachineClient (..), Void,
                                               WaitingResult (..))
import qualified Zerepoch.Contract.StateMachine as SM
import qualified Zerepoch.Contracts.Currency    as Currency
import qualified ZerepochTx
import qualified ZerepochTx.AssocMap            as AssocMap
import qualified ZerepochTx.Prelude             as P

type SimeonSlotRange = (Slot, Slot)
type SimeonInput = (SimeonSlotRange, [Input])

type SimeonSchema =
        Endpoint "create" (AssocMap.Map Val.TokenName PubKeyHash, Simeon.Contract)
        .\/ Endpoint "apply-inputs" (SimeonParams, Maybe SlotInterval, [Input])
        .\/ Endpoint "auto" (SimeonParams, Party, Slot)
        .\/ Endpoint "redeem" (SimeonParams, TokenName, PubKeyHash)
        .\/ Endpoint "close" ()


type SimeonCompanionSchema = EmptySchema
type SimeonFollowSchema = Endpoint "follow" SimeonParams


data SimeonError =
    StateMachineError SM.SMContractError
    | TransitionError (SM.InvalidTransition SimeonData SimeonInput)
    | SimeonEvaluationError TransactionError
    | OtherContractError ContractError
    | RolesCurrencyError ContractError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


makeClassyPrisms ''SimeonError

instance AsSMContractError SimeonError where
    _SMContractError = _StateMachineError

instance AsContractError SimeonError where
    _ContractError = _OtherContractError

instance AsCheckpointError SimeonError where
    _CheckpointError = _OtherContractError . _CheckpointError

data PartyAction
             = PayDeposit AccountId Party Token Integer
             | WaitForTimeout Slot
             | WaitOtherActionUntil Slot
             | NotSure
             | CloseContract
  deriving (Show)

type RoleOwners = AssocMap.Map Val.TokenName PubKeyHash

data ContractHistory =
    ContractHistory
        { chParams  :: First (SimeonParams, SimeonData)
        , chHistory :: [TransactionInput]
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)
        deriving (Semigroup, Monoid) via (GenericSemigroupMonoid ContractHistory)

created :: SimeonParams -> SimeonData -> ContractHistory
created p d = mempty{chParams = First (Just (p, d)) }

transition :: TransactionInput -> ContractHistory
transition i = mempty{chHistory = [i] }

isEmpty :: ContractHistory -> Bool
isEmpty = isNothing . getFirst . chParams

data ContractProgress = InProgress | Finished
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup ContractProgress where
    _ <> Finished     = Finished
    any <> InProgress = any

instance Monoid ContractProgress where
    mempty = InProgress


data LastResult = OK | SomeError SimeonError | Unknown
  deriving (Show,Eq,Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup LastResult where
    any <> Unknown = any
    _ <> last      = last

instance Monoid LastResult where
    mempty = Unknown

type SimeonContractState = LastResult


simeonFollowContract :: Contract ContractHistory SimeonFollowSchema SimeonError ()
simeonFollowContract = awaitPromise $ endpoint @"follow" $ \params -> do
    let client = mkSimeonClient params
    let go [] = pure InProgress
        go (tx:rest) = do
            res <- updateHistoryFromTx client params tx
            case res of
                Finished   -> pure Finished
                InProgress -> go rest

    go [] >>= checkpointLoop (follow client params)

  where
    follow client params = \case
        Finished -> do
            logDebug @String ("Contract finished " <> show params)
            pure $ Left () -- close the contract
        InProgress -> do
            result <- SM.waitForUpdateTimeout @_ @SimeonInput client never >>= awaitPromise
            case result of
                Timeout t -> absurd t
                ContractEnded _ (itvl, inputs) -> do
                    tell @ContractHistory (transition $ TransactionInput itvl inputs)
                    pure (Right Finished)
                Transition _ (itvl, inputs) _ -> do
                    tell @ContractHistory (transition $ TransactionInput itvl inputs)
                    pure (Right InProgress)
                InitialState _ SM.OnChainState{ocsTxOut} -> do
                    let initialSimeonData = tyTxOutData ocsTxOut
                    tell @ContractHistory (created params initialSimeonData)
                    pure (Right InProgress)

    updateHistoryFromTx StateMachineClient{scInstance, scChooser} params tx = do
        logInfo @String $ "Updating history from tx " <> show (view citxTxId tx)
        let inst = SM.typedValidator scInstance
        let address = Scripts.validatorAddress inst
        utxos <- fmap ( Map.filter ((==) address . view Ledger.ciTxOutAddress . fst)
                      . Map.fromList
                      ) $ utxosTxOutTxFromTx tx
        let states = SM.getStates scInstance utxos
        case findInput inst tx of
            -- if there's no TxIn for Simeon contract that means
            -- it's a contract creation transaction, and there is Simeon TxOut
            Nothing -> case scChooser states of
                Left err    -> throwing _SMContractError err
                Right SM.OnChainState{SM.ocsTxOut=state} -> do
                    let initialSimeonData = tyTxOutData state
                    logInfo @String ("Contract created " <> show initialSimeonData)
                    tell $ created params initialSimeonData
                    pure InProgress
            -- There is TxIn with Simeon contract, hence this is a state transition
            Just (interval, inputs) -> do
                let txInput = TransactionInput {
                        txInterval = interval,
                        txInputs = inputs }
                tell $ transition txInput
                case states of
                    -- when there is no Simeon TxOut the contract is closed
                    -- and we can close the follower contract
                    [] -> pure Finished
                    -- otherwise we continue following
                    _  -> pure InProgress

    findInput inst tx = do
        let inputs = Set.toList (view citxInputs tx) >>= maybeToList . inScripts
        let script = Scripts.validatorScript inst
        -- find previous Simeon contract
        let simeonTxInputs = filter (\(validator, _, _) -> validator == script) inputs
        case simeonTxInputs of
            []                          -> Nothing
            [(_, Ledger.Redeemer d, _)] -> ZerepochTx.fromBuiltinData d
            _                           -> Nothing

{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
simeonZerepochContract :: Contract SimeonContractState SimeonSchema SimeonError ()
simeonZerepochContract = do
    catching _SimeonError
        (void $ mapError (review _SimeonError) $
            selectList [create, apply, auto, redeem, close])
        (\er -> do
            tell (SomeError er)
            simeonZerepochContract)
  where
    create = endpoint @"create" $ \(owners, contract) -> do
        (params, distributeRoleTokens) <- setupSimeonParams owners contract
        slot <- currentSlot
        let StateMachineClient{scInstance} = mkSimeonClient params
        let simeonData = SimeonData {
                simeonContract = contract,
                simeonState = emptyState slot }
        let payValue = bccValueOf 0
        let SM.StateMachineInstance{SM.typedValidator} = scInstance
        let tx = mustPayToTheScript simeonData payValue <> distributeRoleTokens
        let lookups = Constraints.typedValidatorLookups typedValidator
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
        submitTxConfirmed utx
        tell OK
        simeonZerepochContract
    apply = endpoint @"apply-inputs" $ \(params, slotInterval, inputs) -> do
        _ <- applyInputs params slotInterval inputs
        tell OK
        simeonZerepochContract
    redeem = promiseMap (mapError (review _SimeonError)) $ endpoint @"redeem" $ \(SimeonParams{rolesCurrency}, role, pkh) -> do
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        utxos <- utxosAt address
        let spendPayoutConstraints tx ref txout = let
                expectedDatumHash = datumHash (Datum $ ZerepochTx.toBuiltinData role)
                amount = view Ledger.ciTxOutValue txout
                dh = either id Ledger.datumHash <$> preview Ledger.ciTxOutDatum txout
                in case dh of
                    Just datumHash | datumHash == expectedDatumHash ->
                        -- we spend the rolePayoutScript address
                        Constraints.mustSpendScriptOutput ref unitRedeemer
                        -- and pay to a token owner
                            <> Constraints.mustPayToPubKey pkh amount
                    _ -> tx

        let spendPayouts = Map.foldlWithKey spendPayoutConstraints mempty utxos
            constraints = spendPayouts
                -- must spend a role token for authorization
                <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
            -- lookup for payout validator and role payouts
            validator = rolePayoutScript rolesCurrency
            lookups = Constraints.otherScript validator
                <> Constraints.unspentOutputs utxos
                <> Constraints.ownPubKeyHash pkh
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx @Void lookups constraints)
        _ <- submitUnbalancedTx utx
        tell OK
        simeonZerepochContract
    auto = endpoint @"auto" $ \(params, party, untilSlot) -> do
        let theClient = mkSimeonClient params
        let continueWith :: SimeonData -> Contract SimeonContractState SimeonSchema SimeonError ()
            continueWith md@SimeonData{simeonContract} =
                if canAutoExecuteContractForParty party simeonContract
                then autoExecuteContract theClient party md
                else do
                    tell OK
                    simeonZerepochContract

        maybeState <- SM.getOnChainState theClient
        case maybeState of
            Nothing -> do
                wr <- SM.waitForUpdateUntilSlot theClient untilSlot
                case wr of
                    ContractEnded{} -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        tell OK
                        simeonZerepochContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        tell OK
                        simeonZerepochContract
                    Transition _ _ simeonData -> continueWith simeonData
                    InitialState _ simeonData -> continueWith simeonData
            Just (SM.OnChainState{SM.ocsTxOut=st}, _) -> do
                let simeonData = tyTxOutData st
                continueWith simeonData
    close = endpoint @"close" $ \_ -> tell OK


    autoExecuteContract :: StateMachineClient SimeonData SimeonInput
                      -> Party
                      -> SimeonData
                      -> Contract SimeonContractState SimeonSchema SimeonError ()
    autoExecuteContract theClient party simeonData = do
        slot <- currentSlot
        let slotRange = (slot, slot + defaultTxValidationRange)
        let action = getAction slotRange party simeonData
        case action of
            PayDeposit acc p token amount -> do
                logInfo @String $ "PayDeposit " <> show amount <> " at within slots " <> show slotRange
                let payDeposit = do
                        simeonData <- SM.runStep theClient (slotRange, [IDeposit acc p token amount])
                        case simeonData of
                            SM.TransitionFailure e -> throwing _TransitionError e
                            SM.TransitionSuccess d -> continueWith d

                catching _StateMachineError payDeposit $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 slots"
                    _ <- awaitSlot (slot + 2)
                    continueWith simeonData
            WaitForTimeout timeout -> do
                logInfo @String $ "WaitForTimeout " <> show timeout
                _ <- awaitSlot timeout
                continueWith simeonData
            WaitOtherActionUntil timeout -> do
                logInfo @String $ "WaitOtherActionUntil " <> show timeout
                wr <- SM.waitForUpdateUntilSlot theClient timeout
                case wr of
                    ContractEnded{} -> do
                        logInfo @String $ "Contract Ended"
                        tell OK
                        simeonZerepochContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith simeonData
                    Transition _ _ simeonData -> continueWith simeonData
                    InitialState _ simeonData -> continueWith simeonData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                tell OK
                simeonZerepochContract

            NotSure -> do
                logInfo @String $ "NotSure"
                tell OK
                simeonZerepochContract

          where
            continueWith = autoExecuteContract theClient party


setupSimeonParams
    :: forall s e i o.
    (AsSimeonError e)
    => RoleOwners -> Simeon.Contract -> Contract SimeonContractState s e (SimeonParams, TxConstraints i o)
setupSimeonParams owners contract = mapError (review _SimeonError) $ do
    creator <- pubKeyHash <$> ownPubKey
    let roles = extractContractRoles contract
    if Set.null roles
    then do
        let params = SimeonParams
                { rolesCurrency = bccSymbol
                , rolePayoutValidatorHash = defaultRolePayoutValidatorHash }
        pure (params, mempty)
    else if roles `Set.isSubsetOf` Set.fromList (AssocMap.keys owners)
    then do
        let tokens = fmap (, 1) $ Set.toList roles
        cur <- mapError (\(Currency.CurContractError ce) -> RolesCurrencyError ce) $ Currency.mintContract creator tokens
        let rolesSymbol = Currency.currencySymbol cur
        let giveToParty (role, pkh) = Constraints.mustPayToPubKey pkh (Val.singleton rolesSymbol role 1)
        let distributeRoleTokens = foldMap giveToParty (AssocMap.toList owners)
        let params = SimeonParams
                { rolesCurrency = rolesSymbol
                , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesSymbol }
        pure (params, distributeRoleTokens)
    else do
        let missingRoles = roles `Set.difference` Set.fromList (AssocMap.keys owners)
        let message = T.pack $ "You didn't specify owners of these roles: " <> show missingRoles
        throwing _ContractError $ OtherError message


getAction :: SimeonSlotRange -> Party -> SimeonData -> PartyAction
getAction slotRange party SimeonData{simeonContract,simeonState} = let
    env = Environment slotRange
    in case reduceContractUntilQuiescent env simeonState simeonContract of
        ContractQuiescent _reduced _warnings _payments state contract ->
            -- here the contract is either When or Close
            case contract of
                When [Case (Deposit acc depositParty tok value) _] _ _
                    | party == depositParty -> let
                        amount = Simeon.evalValue env state value
                        in PayDeposit acc party tok amount
                When [Case (Deposit _ depositParty _ _) _] timeout _
                    | party /= depositParty    ->
                        WaitOtherActionUntil timeout
                When [] timeout _ -> WaitForTimeout timeout
                Close -> CloseContract
                _ -> NotSure
        -- When timeout is in the slot range
        RRAmbiguousSlotIntervalError ->
            {- FIXME
                Consider contract:
                    When [cases] (Slot 100) (When [Case Deposit Close]] (Slot 105) Close)

                For a slot range (95, 105) we get RRAmbiguousSlotIntervalError
                because timeout 100 is inside the slot range.
                Now, we wait for slot 105, and we miss the Deposit.

                To avoid that we need to know what was the original timeout
                that caused RRAmbiguousSlotIntervalError (i.e. Slot 100).
                Then we'd rather wait until slot 100 instead and would make the Deposit.
                I propose to modify RRAmbiguousSlotIntervalError to include the expected timeout.
             -}
            WaitForTimeout (snd slotRange)



canAutoExecuteContractForParty :: Party -> Simeon.Contract -> Bool
canAutoExecuteContractForParty party = check
  where
    check cont =
        case cont of
            Close                                    -> True
            When [] _ cont                           -> check cont
            When [Case Deposit{} cont] _ timeoutCont -> check cont && check timeoutCont
            When cases _ timeoutCont                 -> all checkCase cases && check timeoutCont
            Pay _ _ _ _ cont                         -> check cont
            If _ c1 c2                               -> check c1 && check c2
            Let _ _ cont                             -> check cont
            Assert _ cont                            -> check cont


    checkCase (Case (Choice (ChoiceId _ p) _) cont) | p /= party = check cont
    checkCase _                                     = False


applyInputs :: AsSimeonError e
    => SimeonParams
    -> Maybe SlotInterval
    -> [Input]
    -> Contract SimeonContractState SimeonSchema e SimeonData
applyInputs params slotInterval inputs = mapError (review _SimeonError) $ do
    slotRange <- case slotInterval of
            Just si -> pure si
            Nothing -> do
                slot <- currentSlot
                pure (slot, slot + defaultTxValidationRange)
    let theClient = mkSimeonClient params
    dat <- SM.runStep theClient (slotRange, inputs)
    case dat of
        SM.TransitionFailure e -> do
            logError e
            throwing _TransitionError e
        SM.TransitionSuccess d -> return d

rolePayoutScript :: CurrencySymbol -> Validator
rolePayoutScript symbol = mkValidatorScript ($$(ZerepochTx.compile [|| wrapped ||]) `ZerepochTx.applyCode` ZerepochTx.liftCode symbol)
  where
    wrapped s = Scripts.wrapValidator (rolePayoutValidator s)


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
rolePayoutValidator currency role _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role P.> 0


mkRolePayoutValidatorHash :: CurrencySymbol -> ValidatorHash
mkRolePayoutValidatorHash symbol = validatorHash (rolePayoutScript symbol)


defaultRolePayoutValidatorHash :: ValidatorHash
defaultRolePayoutValidatorHash = mkRolePayoutValidatorHash bccSymbol

simeonParams :: CurrencySymbol -> SimeonParams
simeonParams rolesCurrency = SimeonParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = mkRolePayoutValidatorHash rolesCurrency }


defaultSimeonParams :: SimeonParams
defaultSimeonParams = simeonParams bccSymbol


{-# INLINABLE mkSimeonStateMachineTransition #-}
mkSimeonStateMachineTransition
    :: SimeonParams
    -> SM.State SimeonData
    -> SimeonInput
    -> Maybe (TxConstraints Void Void, SM.State SimeonData)
mkSimeonStateMachineTransition params SM.State{ SM.stateData=SimeonData{..}, SM.stateValue=scriptInValue}
    (interval@(minSlot, maxSlot), inputs) = do
    let positiveBalances = validateBalances simeonState ||
            -- Avoid creating a too-big string literal
            P.traceError ("Invalid contract state. " `P.appendString` "There exists an account with non positive balance")

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsConstraints = validateInputs params inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts simeonState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = inputBalance == scriptInValue

    let preconditionsOk = P.traceIfFalse "Preconditions are false" $ positiveBalances && balancesOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput simeonState simeonContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let simeonData = SimeonData {
                    simeonContract = txOutContract,
                    simeonState = txOutState }

            let (outputsConstraints, finalBalance) = let
                    payoutsByParty = AssocMap.toList $ P.foldMap payoutByParty txOutPayments
                    in case txOutContract of
                        Close -> (payoutConstraints payoutsByParty, P.zero)
                        _ -> let
                            outputsConstraints = payoutConstraints payoutsByParty
                            totalIncome = P.foldMap collectDeposits inputs
                            totalPayouts = P.foldMap snd payoutsByParty
                            finalBalance = inputBalance P.+ totalIncome P.- totalPayouts
                            in (outputsConstraints, finalBalance)
            -- TODO Push this use of time further down the code
            let range = TimeSlot.slotRangeToPOSIXTimeRange def $ Interval.interval minSlot maxSlot
            let constraints = inputsConstraints <> outputsConstraints <> mustValidateIn range
            if preconditionsOk
            then Just (constraints, SM.State simeonData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    validateInputs :: SimeonParams -> [Input] -> TxConstraints Void Void
    validateInputs SimeonParams{rolesCurrency} inputs = let
        (keys, roles) = P.foldMap validateInputWitness inputs
        mustSpendSetOfRoleTokens = P.foldMap mustSpendRoleToken (AssocMap.keys roles)
        in P.foldMap mustBeSignedBy keys P.<> mustSpendSetOfRoleTokens
      where
        validateInputWitness :: Input -> ([PubKeyHash], AssocMap.Map TokenName ())
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> (P.mempty, P.mempty)
          where
            validatePartyWitness (PK pk)     = ([pk], P.mempty)
            validatePartyWitness (Role role) = ([], AssocMap.singleton role ())

        mustSpendRoleToken :: TokenName -> TxConstraints Void Void
        mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1

    collectDeposits :: Input -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> TxConstraints i0 o0
    payoutConstraints payoutsByParty = P.foldMap payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ ZerepochTx.toBuiltinData role
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value


{-# INLINABLE isFinal #-}
isFinal :: SimeonData -> Bool
isFinal SimeonData{simeonContract=c} = isClose c

{-# INLINABLE mkValidator #-}
mkValidator :: SimeonParams -> Scripts.ValidatorType SimeonStateMachine
mkValidator p = SM.mkValidator $ SM.mkStateMachine Nothing (mkSimeonStateMachineTransition p) isFinal


mkSimeonValidatorCode
    :: SimeonParams
    -> ZerepochTx.CompiledCode (Scripts.ValidatorType SimeonStateMachine)
mkSimeonValidatorCode params =
    $$(ZerepochTx.compile [|| mkValidator ||]) `ZerepochTx.applyCode` ZerepochTx.liftCode params


type SimeonStateMachine = StateMachine SimeonData SimeonInput

typedValidator :: SimeonParams -> Scripts.TypedValidator SimeonStateMachine
typedValidator params = Scripts.mkTypedValidator @SimeonStateMachine
    (mkSimeonValidatorCode params)
    $$(ZerepochTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @SimeonData @SimeonInput


mkMachineInstance :: SimeonParams -> SM.StateMachineInstance SimeonData SimeonInput
mkMachineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine Nothing (mkSimeonStateMachineTransition params) isFinal)
    (typedValidator params)


mkSimeonClient :: SimeonParams -> SM.StateMachineClient SimeonData SimeonInput
mkSimeonClient params = SM.mkStateMachineClient (mkMachineInstance params)


defaultTxValidationRange :: Slot
defaultTxValidationRange = 10


newtype CompanionState = CompanionState (Map SimeonParams SimeonData)
  deriving (Semigroup,Monoid) via (Map SimeonParams SimeonData)

instance ToJSON CompanionState where
    toJSON (CompanionState m) = toJSON $ Map.toList m

instance FromJSON CompanionState where
    parseJSON v = CompanionState . Map.fromList <$> parseJSON v

{-|
    Contract that monitors a user wallet for receiving a Simeon role token.
    When it sees that a Simeon contract exists on chain with a role currency
    of a token the user owns it updates its @CompanionState@
    with contract's @SimeonParams@ and @SimeonData@
-}
simeonCompanionContract :: Contract CompanionState SimeonCompanionSchema SimeonError ()
simeonCompanionContract = contracts
  where
    contracts = do
        pkh <- pubKeyHash <$> ownPubKey
        let ownAddress = pubKeyHashAddress pkh
        utxo <- utxosAt ownAddress
        let txOuts = fmap Ledger.toTxOut $ Map.elems utxo
        forM_ txOuts notifyOnNewContractRoles
        checkpointLoop (fmap Right <$> cont) ownAddress
    cont ownAddress = do
        txns <- NonEmpty.toList <$> awaitUtxoProduced ownAddress
        let txOuts = txns >>= view (citxOutputs . _ValidTx)
        forM_ txOuts notifyOnNewContractRoles
        pure ownAddress

notifyOnNewContractRoles :: TxOut
    -> Contract CompanionState SimeonCompanionSchema SimeonError ()
notifyOnNewContractRoles txout = do
    let curSymbols = filterRoles txout
    forM_ curSymbols $ \cs -> do
        contract <- findSimeonContractsOnChainByRoleCurrency cs
        case contract of
            Just (params, md) -> do
                logDebug @String $ "Updating observable state"
                tell $ CompanionState (Map.singleton params md)
            Nothing           -> do
                logWarn @String $ "On-chain state not found!"
                pure ()


filterRoles :: TxOut -> [CurrencySymbol]
filterRoles TxOut { txOutValue, txOutDatumHash = Nothing } =
    let curSymbols = filter (/= bccSymbol) $ AssocMap.keys $ Val.getValue txOutValue
    in  curSymbols
filterRoles _ = []


findSimeonContractsOnChainByRoleCurrency
    :: CurrencySymbol
    -> Contract CompanionState
                SimeonCompanionSchema
                SimeonError
                (Maybe (SimeonParams, SimeonData))
findSimeonContractsOnChainByRoleCurrency curSym = do
    let params = simeonParams curSym
    let client = mkSimeonClient params
    maybeState <- SM.getOnChainState client
    case maybeState of
        Just (SM.OnChainState{SM.ocsTxOut}, _) -> do
            let simeonData = tyTxOutData ocsTxOut
            pure $ Just (params, simeonData)
        Nothing -> pure Nothing
