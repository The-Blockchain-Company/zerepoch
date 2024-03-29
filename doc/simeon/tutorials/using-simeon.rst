.. _using-simeon:

Using Simeon from the ghci command line
========================================

This tutorial shows you how to use Simeon from within Haskell, and in
particular shows how to exercise a contract using the semantics given
earlier.

Simeon in Haskell
------------------

This tutorial works in version 3.0 of Simeon which can be found in the
``master`` branch of the repository:

.. code:: bash

   git clone https://github.com/The-Blockchain-Company/simeon.git
   cd simeon

Stepping through contracts
--------------------------

As we saw earlier the semantics of a single transaction are defined by
the function

.. code:: haskell

   computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput

where the types are defined like this:

.. code:: haskell

   data TransactionInput = TransactionInput
       { txInterval :: SlotInterval
       , txInputs   :: [Input] }

   data TransactionOutput =
       TransactionOutput
           { txOutWarnings :: [ReduceWarning]
           , txOutPayments :: [Payment]
           , txOutState    :: State
           , txOutContract :: Contract }
       | Error TransactionError

and ``States`` are defined like this, with a helper function to define
an initially empty state:

.. code:: haskell

   data State = State { accounts :: Map Party Money
                      , choices  :: Map ChoiceId ChosenNum
                      , boundValues :: Map ValueId Integer
                      , minSlot :: Slot }

   emptyState :: Slot -> State
   emptyState sn = State { accounts = Map.empty
                         , choices = Map.empty
                         , boundValues = Map.empty
                         , minSlot = sn }

We can use the facilities of ``ghci`` to step through a contract one
transaction at a time, and, here, we will do that with the embedded
escrow contract contained in
```EscrowSimmpleV2.hs`` <https://github.com/The-Blockchain-Company/simeon/blob/master/semantics-3.0/src/Language/Simeon/Examples/EscrowSimpleV2.hs>`_.

To single step, you can work in ``ghci`` like this, using the facility
to make local bindings:

.. code:: haskell

   Prelude> :set -XOverloadedStrings
   Prelude> :l Language/Simeon/Examples/EscrowSimpleV2.hs
    ...
   *Lang...V2> let (TransactionOutput txWarn1 txPay1 state1 con1) = computeTransaction (TransactionInput (0, 0) [IDeposit "alice" "alice" bcc 450]) (emptyState 0) contract

In doing this we have pattern matched the output of an application of
``computeTransaction``, which takes three inputs: the second is an
initial state (at slot number 0) and the third is the initial escrow
contract. The first is a ``TransactionInput`` which contains a
``SlotInterval`` – here ``SlotInterval 0 0`` – and a deposit of 450
Entropic from ``"alice"`` into her account ``"alice"`` namely
``IDeposit "alice" "alice" bcc 450``.

.. note::

   If you want to try this for yourself in ghci, you can copy and paste
   from the code examples: they are in horizontally scrolling windows.

The output is matched with
``TransactionOutput txWarn1 txPay1 state1 con1`` so that we can examine
the various components separately:

.. code:: haskell

   *Lang...V2> txWarn1
   []
   *Lang...V2> txPay1
   []
   *Lang...V2> state1
   State {accounts = fromList [("alice", bcc), 450)], choices = fromList [], boundValues = fromList [], minSlot = 0}
   *Lang...V2> con1
   When [Case (Choice (ChoiceId "choice" "alice") [Bound 0 1])
    ...

This shows that the transaction generates no warnings or payments, but
updates the state to show the balance in the account ``"alice"``, and
updates the contract, ready to receive a choice from Alice or Bob.

In the next state the contract is waiting for input, and if both Alice
and Bob agree to make a payment to Bob by choosing ``0``, then a payment
to Bob is generated. This is verified through this interaction in GHCI:

.. code:: haskell

   *Lang...V2> let (TransactionOutput txWarn2 txPay2 state2 con2) = computeTransaction (TransactionInput (SlotInterval 0 0) [IChoice (ChoiceId "choice" "alice") 0, IChoice (ChoiceId "choice" "bob") 0]) state1 con1
   *Lang...V2> txPay2
   [Payment "bob" bcc 450]
   *Lang...V2> con2
   Close
   *Lang...V2> state2
   State {accounts = fromList [], choices = fromList [(ChoiceId "choice" "alice",0),(ChoiceId "choice" "bob",0)], boundValues = fromList [], minSlot = 0}

An alternative way of doing this is to add these definitions to a
working file, e.g. ``Build.hs``, where these definitions will be
preserved. Indeed, it would be very sensible to include some of the
definitions used above in such a file.

Alternative routes through the contract
---------------------------------------

The local bindings are lost each time a ``:load`` or ``:l`` command is
performed, so doing that allows us to re-use some earlier commands. An
alternative execution of the contract is given by

-  First step: Alice deposits money as in the earlier example.

-  Second step: Alice and Bob select different options. This can be done
   like this:

.. code:: haskell

   *Lang...V2> let (TransactionOutput txWarn2 txPay2 state2 con2) = computeTransaction (TransactionInput (SlotInterval 0 0) [IChoice (ChoiceId "choice" "alice") 0, IChoice (ChoiceId "choice" "bob") 1]) state1 con1
   *Lang...V2> con2
   When [Case (Choice (ChoiceId "choice" "carol") [Bound 1 1]) Close, Case (Choice (ChoiceId "choice" "carol") [Bound 0 0]) (Pay "alice" (Party "bob") bcc (Constant 450) Close)] 100 Close
   *Lang...V2> state2
   State {accounts = fromList [("alice", bcc), 450)], choices = fromList [(ChoiceId "choice" "alice",0),(ChoiceId "choice" "bob",1)], boundValues = fromList [] , minSlot = 0}

This shows that we’re now in a contract where the choice is up to Carol,
and that there is still the 450 Entropic in the ``"alice"`` account.

-  Third step: Carol makes a choice. If she chooses 0, payment to Bob is
   made. If she chooses 1, Alice is refunded. Let’s do that now:

.. code:: haskell

   *Lang...V2> let (TransactionOutput txWarn3 txPay3 state3 con3) = computeTransaction  (TransactionInput (SlotInterval 0 0) [IChoice (ChoiceId "choice" "carol") 1]) state2 con2
   *Lang...V2> txPay3
   [Payment "alice" bcc 450]
   *Lang...V2> con3
   Close
   *Lang...V2> state3
   State {accounts = fromList [], choices = fromList [(ChoiceId "choice" "alice",0), (ChoiceId "choice" "bob",1),(ChoiceId "choice" "carol",1)], boundValues = fromList [], minSlot = 0}

So now the contract is ready to ``Close``, and so to refund any
remaining money, but it is clear from ``state3`` that there are no
accounts containing non-zero balances, and so the contract is
terminated.

Why is single stepping useful? It is the equivalent of debugging, and we
are able to see the internal state of the contract at each stage, the
contract continuation, i.e. what remains to be executed, and the actions
produced at each step.

   **Exercise**

   Explore some other ways of engaging with the contract - What happens
   when Bob and Alice choose to refund the money to Alice? - What
   happens if Bob and Alice disagree, but Carol sides with Bob?
