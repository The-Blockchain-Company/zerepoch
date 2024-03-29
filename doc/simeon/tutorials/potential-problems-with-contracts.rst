.. _potential-problems-with-contracts:

Potential problems with contracts
=================================

The Simeon language is designed to have as few as possible pitfalls and
gotchas, so that contracts can be written intuitively, avoiding any
surprises. Nevertheless, it is impossible by design to exclude all
contracts that should not be written, without making Simeon much harder
to use. Moreover, even when a contract is well written, it is still
possible for its users to interact with it in invalid ways, by issuing
invalid transactions.

In all cases, when these unintended effects happen, Simeon is designed
to behave in the most intuitive and conservative way possible. However,
it is worth being aware of these potential problems, and review how
Simeon behaves in these situations. That is the subject of this
tutorial.

Warnings
--------

Simeon warnings are indications that a contract is written wrongly. A
well-written contract should never issue a warning, no matter how the
users interact with it. Ideally, we would like to forbid contracts that
can issue warnings from being ever written, but that would require
Simeon contracts to be dependently-typed, and writing expressions that
are dependently-typed is much more cumbersome.

Instead, Simeon allows contracts that issue warning to be written, and
we provide :ref:`static analysis
tools <static-analysis>` that let contract
developers check whether a particular contract can possibly issue
warnings. Additionally, we provide fall-back behaviours for when a
contract produces a warning, despite our advice. We provide fall-back
behaviours because we acknowledge that analysing big contracts can be
very computationally expensive, and because mistakes can be made. We
want badly written contracts to fail in the most harmless way possible,
that is conservatively.

Non-positive payments
~~~~~~~~~~~~~~~~~~~~~

When a contract is supposed to pay an amount of money that is less than
one unit of a currency or token, it will issue a ``NonPositivePay``
warning, and it will not transfer any money.

Negative payments should be implemented as either positive deposits
(when paying a participant), or positive payments in the opposite
direction (when paying between accounts).

Non-positive deposits
~~~~~~~~~~~~~~~~~~~~~

When a contract is supposed to expect an amount of money that is less
than one unit of a currency of token, it will still wait for a
``IDeposit`` transaction, but that transaction does not need to transfer
any money into the contract and no money is transferred to the
participant that issues the transaction. Once this ‘fake’ deposit is
successful, the contract will issue a ``NonPositiveDeposit`` warning.

Negative deposits should always be implemented as positive payments.

Partial payment
~~~~~~~~~~~~~~~

When a contract is supposed to pay an amount of money that is larger
than the amount of money that there is in the source account, it will
just transfer whatever is available in that account, even if there is
enough money in all the accounts of the contract, and it will issue a
``PartialPay`` warning.

Partial payments should be avoided because a contract that never
produces a partial payment is an explicit contract. Explicit contracts
reassure their users that they will be enforceable, and that wherever in
the contract it says a payment is going to happen it will indeed happen.

``Let`` shadowing (not covered by static analysis)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a contract reaches a ``Let`` construct that re-defines a value with
an identifier that was already defined by an outer ``Let``, the contract
will issue a ``Shadowing`` warning, and it will override the previous
definition.

Shadowing is a bad programming practice because it leads to confusion.
Using the same identifier for more than one thing can mislead developers
or users into thinking that one usage of ``Use`` is going to be
evaluated to one amount while it is actually going to be evaluated to
some other different amount.

Bad smells
----------

There are some other ‘bad smells’ that indicate that a contract has
probably been poorly designed.

These contracts are valid, in the sense that they will not necessarily
cause any warnings, and they do what they say that they do, but they
have characteristics that suggest that either the contract developer was
not fully aware of the consequences of the contract, or that the
developer purposefully wrote the contract in a way that was confusing
for the reader.

Undefined ``Let`` usage (should be a warning)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a ``Use`` construct uses an identifier that has not been defined
yet, it will evaluate to the default value of ``0``. No warning will be
issued but, again, this is a bad practice because it can be misleading.
``(Constant 0)`` should be used instead since it makes explicit the
amount in question.

Unreachable parts of a contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the main bad smell in Simeon contracts. If part of the contract
is unreachable, why would it have been included in the first place?

This bad smell takes a number of shapes.

Sub-``Contract`` is not reachable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For example:

.. code:: haskell

       If FalseObs contract1 contract2

The previous contract is equivalent to ``contract2``. In general you
should never use ``FalseObs``, and you should only use ``TrueObs`` as
the root observation of a ``Case`` construct.

``Observation`` is always short-cut
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For example:

.. code:: haskell

       OrObs TrueObs observation1

The previous observation is equivalent to ``observation1``. Again, you
should only use ``TrueObs`` as the root observation of a ``Case``
construct.

``When`` branch is unreachable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For example:

.. code:: haskell

       When [ Case (Notify TrueObs) contract1
            , Case (Notify TrueObs) contract2 ]
            10
            contract3

``contract2`` is unreachable, the whole ``Case`` could be removed from
the contract and the behaviour would be the same.

Nested non-increasing timeouts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For example:

.. code:: haskell

       When []
            10
            When [ Case (Notify TrueObs)
                        contract1 ]
                 10
                 contract2

``contract1`` is unreachable: after block ``10``, the contract will
directly evolve into ``contract2``. The inner ``When`` does not make any
difference to the contract.

Usability issues
----------------

Even if a contract avoids warnings, and has no unreachable code, it may
still allow malicious users to force other users into undesirable
situations that were not originally intended by developer of the
contract.

Bad timing of ``When`` constructs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the following contract:

.. code:: haskell

       When [Case (Choice (ChoiceId "choice1" (Role "alice")) [Bound 0 10])
                  (When [Case (Choice (ChoiceId "choice2" (Role "bob")) [Bound 0 10])
                              Close
                        ]
                   10
                   (Pay (Role "bob") (Party (Role "alice"))
                        bcc
                        (Constant 10)
                        Close
                   )
               )
            ]
            10
            Close

There is nothing wrong in principle with this contract, but if
``(Role "alice")`` makes her choice on block ``9``, it will be virtually
impossible for ``bob`` to make his choice on time and get the refund of
the money in his account ``(Role "bob")``. Unless, this is part of a
game and that is an intended effect, this is likely an unfair contract
for ``(Role "bob")``.

In general, it is a good practice to ensure that ``When`` constructs
have increasing timeouts, and that the increase between timeouts is
reasonable for the different parties to issue and get their transactions
accepted by the blockchain. There are many reasons why the participation
of a party may be delayed: an energy supply failure, a sudden peak in
the number of pending transactions in the blockchain, network attacks,
etc. So it is important to allow plenty of time, and to be generous with
timeouts and with increases in timeouts.

Errors
------

Finally, even if a contract is perfectly written. Users may use it
incorrectly, and we call those incorrect usages errors.

In all cases, whenever a transaction causes an error, the transaction
will have no effect on the ``Contract`` or on its ``State``. In fact,
the wallet of a user will know in advance whether a transaction is going
to produce an error, because transactions are deterministic, so users
should never need to send an erroneous transaction to the blockchain.

Ambiguous interval
~~~~~~~~~~~~~~~~~~

When a transaction reaches a timeout, its slot interval must be
unambiguous about whether the timeout has passed or not. For example, if
the top-most ``When`` of a contract has timeout ``10`` and a transaction
with slot interval ``[6, 14]`` is issued, the transaction will cause an
``AmbiguousSlotIntervalError`` error, because it is impossible to know
whether the timeout has passed just by looking at the transaction. To
avoid this, the transaction must be split into two separate
transactions:

1. One with slot interval ``[6, 9]``.

2. Another one with slot interval ``[10, 14]``.

Apply no-match
~~~~~~~~~~~~~~

If a transaction does not provide the inputs that are expected by the
``Contract``, then the contract will issue a ``NoMatchError`` error, and
the whole transaction will be discarded.

Useless transaction
~~~~~~~~~~~~~~~~~~~

If a transaction does not have any effect on the ``Contract`` or
``State``, it will result on a ``UselessTransaction`` error, and the
whole transaction will be discarded. The reason why we discard useless
transactions is that they open the door to Denial of Service (DoS)
attacks, because a potential attacker could flood the contract with
unnecessary transactions and prevent necessary transactions to make it
into the blockchain.
