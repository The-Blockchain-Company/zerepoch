{-# LANGUAGE OverloadedStrings #-}
module EscrowWithCollateral where

import           Language.Simeon.Extended

main :: IO ()
main = print . pretty $ contract

-- We can set explicitRefunds True to run Close refund analysis
-- but we get a shorter contract if we set it to False
explicitRefunds :: Bool
explicitRefunds = False

seller, buyer, burnAddress :: Party
buyer = Role "Buyer"
seller = Role "Seller"
burnAddress = PK "0000000000000000000000000000000000000000000000000000000000000000"

price, collateral :: Value
price = ConstantParam "Price"
collateral = ConstantParam "Collateral amount"

sellerCollateralTimeout, buyerCollateralTimeout, depositTimeout, disputeTimeout, answerTimeout :: Timeout
sellerCollateralTimeout = SlotParam "Collateral deposit by seller timeout"
buyerCollateralTimeout = SlotParam "Deposit of collateral by buyer timeout"
depositTimeout = SlotParam "Deposit of price by buyer timeout"
disputeTimeout = SlotParam "Dispute by buyer timeout"
answerTimeout = SlotParam "Complaint deadline"

depositCollateral :: Party -> Timeout -> Contract -> Contract -> Contract
depositCollateral party timeout timeoutContinuation continuation =
    When [Case (Deposit party party bcc collateral) continuation]
         timeout
         timeoutContinuation

burnCollaterals :: Contract -> Contract
burnCollaterals =
    Pay seller (Party burnAddress) bcc collateral
    . Pay buyer (Party burnAddress) bcc collateral

deposit :: Timeout -> Contract -> Contract -> Contract
deposit timeout timeoutContinuation continuation =
    When [Case (Deposit seller buyer bcc price) continuation]
         timeout
         timeoutContinuation

choice :: ChoiceName -> Party -> Integer -> Contract -> Case
choice choiceName chooser choiceValue = Case (Choice (ChoiceId choiceName chooser)
                                                     [Bound choiceValue choiceValue])

choices :: Timeout -> Party -> Contract -> [(Integer, ChoiceName, Contract)] -> Contract
choices timeout chooser timeoutContinuation list =
    When [choice choiceName chooser choiceValue continuation
          | (choiceValue, choiceName, continuation) <- list]
         timeout
         timeoutContinuation

sellerToBuyer :: Contract -> Contract
sellerToBuyer = Pay seller (Account buyer) bcc price

refundSellerCollateral :: Contract -> Contract
refundSellerCollateral
  | explicitRefunds = Pay seller (Party seller) bcc collateral
  | otherwise = id

refundBuyerCollateral :: Contract -> Contract
refundBuyerCollateral
  | explicitRefunds = Pay buyer (Party buyer) bcc collateral
  | otherwise = id

refundCollaterals :: Contract -> Contract
refundCollaterals = refundSellerCollateral . refundBuyerCollateral

refundBuyer :: Contract
refundBuyer
 | explicitRefunds = Pay buyer (Party buyer) bcc price Close
 | otherwise = Close

refundSeller :: Contract
refundSeller
 | explicitRefunds = Pay seller (Party seller) bcc price Close
 | otherwise = Close

contract :: Contract
contract = depositCollateral seller sellerCollateralTimeout Close $
           depositCollateral buyer buyerCollateralTimeout (refundSellerCollateral Close) $
           deposit depositTimeout (refundCollaterals Close) $
           choices disputeTimeout buyer (refundCollaterals refundSeller)
              [ (0, "Everything is alright"
                , refundCollaterals refundSeller
                )
              , (1, "Report problem"
                , sellerToBuyer $
                  choices answerTimeout seller (refundCollaterals refundBuyer)
                     [ (1, "Confirm problem"
                       , refundCollaterals refundBuyer
                       )
                     , (0, "Dispute problem"
                       , burnCollaterals refundBuyer
                       )
                     ]
                )
              ]
