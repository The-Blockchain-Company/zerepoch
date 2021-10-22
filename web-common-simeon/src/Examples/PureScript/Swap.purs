module Examples.PureScript.Swap
  ( contractTemplate
  , fullExtendedContract
  , metaData
  , fixedTimeoutContract
  , defaultSlotContent
  ) where

import Prelude
import Data.BigInteger (BigInteger, fromInt)
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Examples.Metadata as Metadata
import Simeon.Extended (Action(..), Case(..), Contract(..), Payee(..), Timeout(..), Value(..))
import Simeon.Extended.Metadata (MetaData, ContractTemplate)
import Simeon.Template (TemplateContent(..), fillTemplate)
import Simeon.Semantics (Party(..), Token(..))

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract: fullExtendedContract }

fixedTimeoutContract :: Contract
fixedTimeoutContract =
  fillTemplate
    ( TemplateContent
        { slotContent: defaultSlotContent
        , valueContent: Map.empty
        }
    )
    fullExtendedContract

defaultSlotContent :: Map String BigInteger
defaultSlotContent =
  Map.fromFoldable
    [ "Timeout for Bcc deposit" /\ fromInt 600
    , "Timeout for dollar deposit" /\ fromInt 1200
    ]

metaData :: MetaData
metaData = Metadata.swap

bcc :: Token
bcc = Token "" ""

entropicPerBcc :: Value
entropicPerBcc = Constant (fromInt 1000000)

amountOfBcc :: Value
amountOfBcc = ConstantParam "Amount of Bcc"

amountOfEntropic :: Value
amountOfEntropic = MulValue entropicPerBcc amountOfBcc

amountOfDollars :: Value
amountOfDollars = ConstantParam "Amount of dollars"

bccDepositTimeout :: Timeout
bccDepositTimeout = SlotParam "Timeout for Bcc deposit"

dollarDepositTimeout :: Timeout
dollarDepositTimeout = SlotParam "Timeout for dollar deposit"

dollars :: Token
dollars = Token "85bb65" "dollar"

type SwapParty
  = { party :: Party
    , currency :: Token
    , amount :: Value
    }

bccProvider :: SwapParty
bccProvider =
  { party: Role "Bcc provider"
  , currency: bcc
  , amount: amountOfEntropic
  }

dollarProvider :: SwapParty
dollarProvider =
  { party: Role "Dollar provider"
  , currency: dollars
  , amount: amountOfDollars
  }

makeDeposit :: SwapParty -> Timeout -> Contract -> Contract -> Contract
makeDeposit src timeout timeoutContinuation continuation =
  When
    [ Case (Deposit src.party src.party src.currency src.amount)
        continuation
    ]
    timeout
    timeoutContinuation

makePayment :: SwapParty -> SwapParty -> Contract -> Contract
makePayment src dest continuation = Pay src.party (Party $ dest.party) src.currency src.amount continuation

fullExtendedContract :: Contract
fullExtendedContract =
  makeDeposit bccProvider bccDepositTimeout Close
    $ makeDeposit dollarProvider dollarDepositTimeout Close
    $ makePayment bccProvider dollarProvider
    $ makePayment dollarProvider bccProvider
        Close
