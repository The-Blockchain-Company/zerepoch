.. _actus-simeon:

ACTUS and Simeon
=================

This tutorial gives an introduction to the general idea of the ACTUS
standards for the algorithmic representation of financial contracts,
plus examples implemented in Simeon.

ACTUS
-----

The ACTUS Financial Research Foundation https://www.actusfrf.org has
created a standard for financial contracts, categorised by means of a
`taxonomy <https://www.actusfrf.org/taxonomy>`_ and described in a
detailed `technical
specification <https://www.actusfrf.org/techspecs>`_.

The ACTUS standards build on the understanding that financial contracts
are legal agreements between two (or more) counterparties on the
exchange of future cash flows. Historically, such legal agreements are
described in natural language leading to ambiguity and artificial
diversity. As a response, the ACTUS standards define financial contracts
by means of a set of contractual terms and deterministic functions
mapping these terms onto future payment obligations. Thereby, it is
possible to describe the vast majority of financial instruments through
a set of little more than 30 Contract Types or modular templates,
respectively.

The ACTUS specifications provide a breadth of exercises for
implementation in Simeon, and we illustrate an approach to this in the
following example.

Simple Zero Coupon Bond Example
-------------------------------

A zero-coupon bond is a debt security that does not pay interest (a
coupon) but is issued at a discount, rendering profit at maturity when
the bond is redeemed for its full face value.

For example, an ``investor`` can buy a bond that costs 1000 Entropic
with 15% discount. She pays 850 Entropic to the bond issuer before
*start time*, here ``10``.

Later, after *maturity date*, time ``20`` here, the investor can
exchange the bond for its full notional, i.e. 1000 Entropic.

.. code:: haskell

   When [
     (Case
        (Deposit
           "investor"
           "investor" bcc
           (Constant 850))
        (Pay
           "investor"
           (Party "issuer") bcc
           (Constant 850)
           (When [
              (Case
                 (Deposit
                    "investor"
                    "issuer" bcc
                    (Constant 1000))
                 (Pay
                    "investor"
                    (Party "investor") bcc
                    (Constant 1000)
                    Close))
               ]
               20
               Close)))
        ]
        10
        Close

This contract has a significant drawback. Once the ``investor`` has
deposited the 850 Entropic, it will be immediately paid to the
``issuer`` (if the ``investor`` does not invest in time, the contract
ends). After that, two outcomes are possible

-  the ``issuer`` deposits 1000 Entropic in the ``investor``'s account,
   and that is then immediately paid to the ``investor`` in full;

-  if the ``investor`` doesn’t make the deposit, then the contract is
   closed and all the money in the contract is refunded, but there is
   *no* money in the contract at this point, so the ``investor`` loses
   her money.

How can we avoid this problem of the ``issuer`` defaulting?

There are at least two ways to solve this: we could ask the ``issuer``
to deposit the full amount before the contract begins, but that would
defeat the object of issuing the bond in the first place. More
realistically, we could ask a third party to be a guarantor of the deal.

   **Exercise**

   Give a variant of the ``zeroCouponBond`` contract in which it is
   necessary for the ``issuer`` to put the full value of the up before
   the bond is issued.

..

   **Exercise**

   Give a variant of the ``zeroCouponBond`` contract which also includes
   a ``guarantor`` who puts up the full payment up before the bond is
   issued, and who will pay that counterparty if the issuer defaults; if
   the issuer does make the payment in time, then the guarantor should
   recover their money.

Guaranteed Coupon Bond Example
------------------------------

This more complex bond involves an ``investor`` who deposits 1000
Entropic, which is immediately paid to the ``issuer``. The ``issuer``
then has to pay as interest 10 Entropic every 10 slots. On maturity the
investor should receive back the interest plus the full value of the
bond.

.. code:: haskell

   couponBondFor3Month12Percent =
       -- investor deposits 1000 Entropic
       When [ Case (Deposit "investor" "investor" bcc (Constant 1000))
           -- and pays it to the issuer
           (Pay "investor" (Party "issuer") bcc (Constant 1000)
               -- after 10 slots expect to receive 10 Entropic interest
               (When [ Case (Deposit "investor" "issuer" bcc (Constant 10))
                   -- and pay it to the investor
                   (Pay "investor" (Party "investor" ) bcc (Constant 10)
                       -- same for 2nd 10 slots
                       (When [ Case (Deposit "investor" "issuer" bcc (Constant 10))
                           (Pay "investor" (Party "investor" ) bcc (Constant 10)
                               -- after maturity date investor
                               -- expects to receive notional + interest payment
                               (When [ Case (Deposit "investor" "issuer" bcc (Constant 1010))
                                   (Pay "investor" (Party "investor" ) bcc (Constant 1010) Close)]
                               (Slot 40)
                               Close))]
                       (Slot 30)
                       Close))]
               (Slot 20)
               Close))]
       (Slot 10)
       Close

..

   **Exercise**

   Give a variant of the ``zcouponBondFor3Month12Percent`` contract
   which also includes a ``guarantor`` who puts up the full payment up
   before the bond is issued, and who will pay that counterparty if the
   issuer defaults; if the issuer does make the payment in time, then
   the guarantor should recover their money.

The Blockchain Co. plans to implement the full ACTUS standard using Simeon and Zerepoch
over the coming year.
