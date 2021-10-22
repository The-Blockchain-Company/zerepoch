{-# LANGUAGE OverloadedStrings #-}
module ZeroCouponBond where

import           Language.Simeon.Extended

main :: IO ()
main = print . pretty $ contract

discountedPrice, notionalPrice :: Value
discountedPrice = ConstantParam "Amount"
notionalPrice = AddValue (ConstantParam "Interest") discountedPrice

investor, issuer :: Party
investor = Role "Lender"
issuer = Role "Borrower"

initialExchange, maturityExchangeTimeout :: Timeout
initialExchange = SlotParam "Loan deadline"
maturityExchangeTimeout = SlotParam "Payback deadline"

transfer :: Timeout -> Party -> Party -> Value -> Contract -> Contract
transfer timeout from to amount continuation =
    When [ Case (Deposit from from bcc amount)
                (Pay from (Party to) bcc amount continuation) ]
         timeout
         Close

contract :: Contract
contract = transfer initialExchange investor issuer discountedPrice
         $ transfer maturityExchangeTimeout issuer investor notionalPrice
           Close
