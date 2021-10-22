{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Stablecoin(
    tests
    , stablecoinTrace
    , maxReservesExceededTrace
    ) where


import           Control.Lens                (preview)
import           Control.Monad               (void)
import           Data.Maybe                  (listToMaybe, mapMaybe)
import           Prelude                     hiding (negate)

import           Ledger.Bcc                  (bccSymbol, bccToken)
import qualified Ledger.Bcc                  as Bcc
import           Ledger.Address              (Address)
import           Ledger.Crypto               (PrivateKey, privateKey2)
import           Ledger.Oracle               (Observation, SignedMessage, signObservation)
import           Ledger.Time                 (POSIXTime)
import qualified Ledger.TimeSlot             as TimeSlot
import           Ledger.Typed.Scripts        (validatorAddress)
import           Ledger.Value                (Value)
import qualified Ledger.Value                as Value
import           Zerepoch.Contract.Test
import           Zerepoch.Contracts.Stablecoin (BC (..), ConversionRate, Input (..), RC (..), SC (..), SCAction (..),
                                              Stablecoin (..), StablecoinError, StablecoinSchema)
import qualified Zerepoch.Contracts.Stablecoin as Stablecoin
import           Zerepoch.Trace.Emulator       (ContractHandle, EmulatorTrace)
import qualified Zerepoch.Trace.Emulator       as Trace
import           Zerepoch.Trace.Emulator.Types (_ContractLog, cilMessage)
import           ZerepochTx.Numeric            (negate, one, zero)
import           ZerepochTx.Ratio              as Ratio
import           Wallet.Emulator.MultiAgent  (eteEvent)

import           Test.Tasty

user :: Wallet
user = w1

oracle :: Wallet
oracle = w2

oraclePrivateKey :: PrivateKey
oraclePrivateKey = privateKey2

onePercent :: Ratio Integer
onePercent = 1 % 100

coin :: Stablecoin
coin = Stablecoin
    { scOracle = walletPubKey oracle
    , scFee = onePercent
    , scMinReserveRatio = zero
    , scMaxReserveRatio = 4 % 1
    , scReservecoinDefaultPrice = BC 1
    , scBaseCurrency = Value.assetClass bccSymbol bccToken
    , scStablecoinTokenName = "stablecoin"
    , scReservecoinTokenName = "reservecoin"
    }

signConversionRate :: POSIXTime -> ConversionRate -> SignedMessage (Observation ConversionRate)
signConversionRate startTime rate = signObservation startTime rate oraclePrivateKey

stablecoinAddress :: Address
stablecoinAddress = validatorAddress $ Stablecoin.typedValidator coin

initialDeposit :: Value
initialDeposit = Bcc.entropicValueOf 100

initialFee :: Value
initialFee = Bcc.entropicValueOf 1

tests :: TestTree
tests = testGroup "Stablecoin"
    [ checkPredicate "mint reservecoins"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee))
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange user (Stablecoin.reserveCoins coin 100 <> negate (initialDeposit <> initialFee))
        )
        $ initialise >>= mintReserveCoins (RC 100) one

    , checkPredicate "mint reservecoins and stablecoins"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee <> Bcc.entropicValueOf 50))
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange user (Stablecoin.stableCoins coin 50 <> Stablecoin.reserveCoins coin 100 <> negate (initialDeposit <> initialFee <> Bcc.entropicValueOf 50))
        )
        $ do
            hdl <- initialise
            mintReserveCoins (RC 100) one hdl
            -- Mint 50 stablecoins at a rate of 1 Bcc: 1 USD
            void $ mintStableCoins (SC 50) one hdl

    , checkPredicate "mint reservecoins, stablecoins and redeem stablecoin at a different price"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee <> Bcc.entropicValueOf 30))
        .&&. assertNoFailedTransactions
        .&&. walletFundsChange user (Stablecoin.stableCoins coin 40 <> Stablecoin.reserveCoins coin 100 <> negate (initialDeposit <> initialFee <> Bcc.entropicValueOf 30))
        )
        stablecoinTrace

    , let expectedLogMsg = "New state is invalid: MaxReserves {allowed = BC {unBC = (200 % 1)}, actual = BC {unBC = (201 % 1)}}. The transition is not allowed." in
      checkPredicate "Cannot exceed the maximum reserve ratio"
        (valueAtAddress stablecoinAddress (== (initialDeposit <> initialFee <> Bcc.entropicValueOf 50))
        .&&. assertNoFailedTransactions
        .&&. assertInstanceLog (Trace.walletInstanceTag w1) ((==) (Just expectedLogMsg) . listToMaybe . reverse . mapMaybe (preview (eteEvent . cilMessage . _ContractLog)))
        )
        maxReservesExceededTrace

    ]

initialise :: Trace.EmulatorTrace (ContractHandle () StablecoinSchema StablecoinError)
initialise = do
    hdl <- Trace.activateContractWallet user Stablecoin.contract
    Trace.callEndpoint @"initialise" hdl coin
    _ <- Trace.waitNSlots 2
    pure hdl

mintReserveCoins :: RC Integer -> ConversionRate -> ContractHandle () StablecoinSchema StablecoinError -> Trace.EmulatorTrace ()
mintReserveCoins rc rate hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    Trace.callEndpoint @"run step" hdl
        Input
            { inpConversionRate = signConversionRate startTime rate
            , inpSCAction = MintReserveCoin rc
            }
    void $ Trace.waitNSlots 2

mintStableCoins :: SC Integer -> ConversionRate -> ContractHandle () StablecoinSchema StablecoinError -> Trace.EmulatorTrace ()
mintStableCoins sc rate hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    Trace.callEndpoint @"run step" hdl
        Input
            { inpConversionRate = signConversionRate startTime rate
            , inpSCAction = MintStablecoin sc
            }
    void $ Trace.waitNSlots 2

redeemStableCoins :: SC Integer -> ConversionRate -> ContractHandle () StablecoinSchema StablecoinError -> Trace.EmulatorTrace ()
redeemStableCoins sc rate hdl = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    Trace.callEndpoint @"run step" hdl
        Input
            { inpConversionRate = signConversionRate startTime rate
            , inpSCAction = MintStablecoin (negate sc)
            }
    void $ Trace.waitNSlots 2

-- | Mint 100 reserve coins, mint 50 stablecoins, then redeem ten of
--   them at a higher exchange rate
stablecoinTrace :: EmulatorTrace ()
stablecoinTrace = do
    hdl <- initialise
    mintReserveCoins (RC 100) one hdl
    mintStableCoins (SC 50) one hdl
    -- redeem 10 stablecoins at an exchange rate of 2 Bcc : 1 USD (so we get 20 entropic from the bank)
    redeemStableCoins (SC 10) (Ratio.fromInteger 2) hdl

-- | Mint 100 reserve coins, mint 50 stablecoins, then attempt to mint
--   another 49 reserve coins. This fails because the max. reserve ratio
--   would be exceeded.
maxReservesExceededTrace :: EmulatorTrace ()
maxReservesExceededTrace = do
    hdl <- initialise
    mintReserveCoins (RC 100) one hdl
    mintStableCoins (SC 50) one hdl

    -- At this point we have:
    -- Stablecoins: 50 (equiv. to 50 Entropic on the 1:1 conversion
    -- rate)
    -- Max. reserve ratio: 4:1
    -- Reserves: 151 Entropic (100 from minting reserve coins, 50 from
    -- minting stablecoins, 1 from fees)
    -- Maximum reserves: 200 Entropic (50 stablecoins * 4 (Entropic /
    -- stablecoin))

    -- The next transition is not allowed as it would bring the reserve
    -- ratio above the maximum.
    mintReserveCoins (RC 49) one hdl
