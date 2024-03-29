.. _migrating:

Migrating from earlier versions of Simeon
==========================================

This tutorial explains how the latest version of Simeon differs from
earlier versions of the language.

Remove ``Both``
---------------

We do not include a ``Both`` construct in the latest version of Simeon,
which makes all contracts sequential.

Since in none of the versions of Simeon was there communication among
the branches of ``Both``, the only extra functionality provided by
``Both`` in practice was the ability to wait for several money deposits
at the same time.

We take care of this functionality by updating the ``When`` construct.
Instead of having different branches wait for different inputs, we move
to a completely sequential and synchronous model, where we can wait for
one of several possible inputs at the same time (as in ``select``).

The reason we removed this construct is that sequential programs are
easier to analyse and easier to reason about, since there is no need for
synchronisation and no opportunity for race conditions.

Include accounts
----------------

In earlier versions of Simeon each commitment has its own timeout. This
means that money deposited in a contract is *not fungible*, because it
can be distinguished by its timeout. To achieve fungibility we have
removed timeouts from individual constructs, and have a single timeout
for all commitments. This *contract lifetime* is straightforward to
infer from the timeouts in the contract.

We do, however, include *accounts* to organise the money deposited into
the contract. This makes it more transparent how money flows within the
contract, and in particular identifies to whom money is *refunded* when
the contract terminates.

Each account is identified by a participant; the participant indicates
who will get the money in the account by default when ``Close`` is
reached.

The reason we chose to include accounts is that without them we found
that we were essentially keeping track of the accounts manually.
Additionally, in every leaf of the AST, we found ourselves calculating
how much we must return to every participant, cluttering the tree with
repetitious “boilerplate”. Thus, having money organised in accounts can
make contracts easier to reason about and less prone to error.

Note that we can provide full fungibility by using a single account. We
only need to write the appropriate ``Pay`` commands in the leaves of the
contract. If all the money is paid to participants, then ``Close`` has
no effect. [1]_

Discussion: Implicit vs Explicit Accounts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many of the use cases for accounts – and all those that we can identify
for ACTUS contracts – have one account per participant, and one
participant per account (the “1-1 model”). This raises the question of
whether we should give an implicit treatment of accounts, with each
participant owning one account.

On the other hand, there are a variety of plausible scenarios for
accounts which do not conform to the 1-1 model.

Examples where multiple participants use an account.

-  Alice owns an account to which she commits money for Bob to spend
   (think of Alice as Bob’s employer). Bob is able to spend up to the
   limit in the account, but after the commitment times out, Alice
   recovers anything remaining.

-  Alice owns an account to which she commits money for Bob and Carol to
   spend (think of Alice as Bob’s and Carol’s employer). They are able
   to spend (jointly) up to the limit in the account, but after the
   commitment times out, Alice recovers anything remaining.

-  On the other hand, they could each be given a separate account from
   which to spend: that would enforce individual limits as well as an
   aggregate limit.

-  If Bob [and Carol] want to spend money they can also add money to the
   account, but they should realise that anything unused will be
   refunded to Alice.

Examples of multiple accounts for one person:

-  Examples of underwriting would fit here. A person underwrites
   first-level risk, and second-level risk using different accounts.
   Only when the first level underwriting of all participants is spent
   will second level spending occur.

``Close`` replaces ``Null`` / ``Pay``
-------------------------------------

Since all contracts are sequential now, we can easily tell when a
contract terminates, i.e: when only ``Null`` is left. We use this
opportunity to *close* the contract and to refund any money remaining in
the accounts; for this reason we have renamed ``Null`` to ``Close``
(aiding comprehensibility).

As noted earlier, there is no longer any explicit timeout in the
accounts, since all contracts will eventually reduce to ``Close``. In
fact, we can statically and efficiently calculate an upper bound for
when this will happen, making this aspect of Simeon analysable.

Pay
---

``Pay`` is now immediate, and it has a single continuation, and fewer
parameters. [2]_ It allows payments from an account to a participant or
to another account. We discarded ``PayAll``, since it can be emulated as
a finite series of ``Pay``. In fact, we can define ``payAll`` as a
Haskell function (see ``zeroCouponBond`` example).

It is a consequence of removing the ``Both`` construct, that it is now
unequivocal which ``Pay`` goes first: they are all sequential, thus
aiding analysis. With the ``Both`` construct we could potentially have
``Pays`` happen in any order (since both sides of ``Both`` are supposed
to run concurrently).

Multi-clause When
-----------------

We have modified ``When`` to include a set of possible actions that can
be input while ``When`` waits. We call this approach “One of Many”,
because it accepts one action out of potentially many allowed actions.
``When`` remains as follows:

.. code:: haskell

   When [Case] Timeout Contract

where ``When`` will either wait for ``Timeout`` and continue as
``Contract``, or continue as specified in one of the ``Cases``,
whichever happens first. ``Case`` is defined as:

.. code:: haskell

   data Case = Case Action Contract

and ``Action`` as:

.. code:: haskell

   data Action = Deposit Party Party Token Value
               | Choice ChoiceId [Bound]
               | Notify Observation

A ``Case`` clause will be activated only if the corresponding ``Action``
is produced, and it will continue as ``Contract``. In case of two
``Actions`` matching, the first one in the list will be executed.

Three kinds of actions are supported:

-  ``Deposit`` represents a deposit of money into an account; this was
   originally called ``Commit``.

-  ``Choice`` represents a choice made by a participant from within a
   set of ``Integer`` values (specified by the list of ``Bounds``).

-  ``Notify`` will wait for a ``Notify`` action issued when the
   ``Observation`` is true. We call it ``Notify`` in order to make it
   clear that we cannot just wait for ``Observations``, but that someone
   must trigger the contract in a moment when an ``Observation`` is
   true.

We have discarded adding observations to ``Deposit`` and ``Choice``
since it would not be obvious whether the ``Observation`` would be
evaluated before or after applying the action.

In addition to explicit cases in ``When``, we must remember that the
*timeout* branch is also a case, and it also needs to be triggered
(similarly to ``Notify``). [3]_  [4]_

Observations and Values
-----------------------

We have discarded ``Observations`` and ``Values`` that can be expressed
by combining others: like the general ``AvailableMoney`` (for the whole
contract), or like ``DepositedMoneyBy``, that remembers the amount of
money deposited by a participant, since the contract can be restructured
to observe that, and supporting would require additional information in
the state (simplicity).

We have retained the ``ChoseSomething`` observation, even though, in the
proposed semantics, every occurrence of ``ChoseSomething`` can be
evaluated statically and efficiently by examining its context.

For example, in the following contract we can see that the first
occurrence of ``ChoseSomething`` will evaluate to ``True``, and the
second one to ``False``:

.. code:: haskell

   When [ Case (Choice (ChoiceId 1 Alice) [(1,1)])
               (If (ChoseSomething (ChoiceId 1 Alice))
                   Close
                   Close)
        , Case (Choice (ChoiceId 2 Bob) [(2,2)])
               (If (ChoseSomething (ChoiceId 1 Alice))
                   Close
                   Close)]
        0
        Close

Nevertheless, we have chosen to keep the construct for two reasons:

-  It allows for code reusability (convenience). For example, in the
   previous contract, we could define ``chosen1``:

::

     let chosen1 = If (ChoseSomething (ChoiceId 1 1))
                      Close
                      Close
     in
     When [ Case (Choice (ChoiceId 1 1) [(1,1)])
                 chosen1
          , Case (Choice (ChoiceId 2 2) [(2,2)])
                 chosen1]
          0
          Close

But this would not be possible if we did not have the construct
``ChoseSomething``, since the value to which it reduces depends on the
context.

-  It may no longer be the case that occurrences of the construct can be
   evaluated statically if we extend the ``When`` construct to support
   “many of many” inputs.

Inclusion of SlotIntervals
--------------------------

The EUTxO specification provides validation scripts with slot-intervals
instead of with slot numbers. This is to promote determinism in
validation scripts. Nevertheless, we have kept the timeout of ``When``
(the only timeout) as a slot number. The way we deal with slot-intervals
is by requiring that the interval of a transaction does not include any
timeout over which the semantics has to make a choice. For example: if a
timeout is 10, a transaction with interval 5-15 will fail with
``AmbiguousSlotInterval``. Participants would have to issue a
transaction with interval 5-9 or 10-15 (or both).

Nevertheless, for ``Values``, we provide the two constructs
``SlotIntervalStart`` and ``SlotIntervalEnd``. An alternative to
consider would be to modify the semantics so that Values are
non-deterministic, that way we could include a ``CurrentSlot`` construct
and just invalidate transactions that are ambiguous, but this would
complicate the semantics and make them less predictable.

.. [1]
   We can potentially provide a way of statically analysing the contract
   to check whether there can possibly be any money left in any account
   when ``Close`` is reached.

.. [2]
   This means that payments now obey a “push” model rather than a “pull”
   model.

.. [3]
   Nevertheless, triggering the contract for processing timeouts is not
   urgent as it is with ``Notify``, because while ``Observations`` can
   alternate between ``True`` and ``False``, timeouts can only happen
   once and, independently of whether they have been observed by the
   contract or not, they cannot be reversed.

.. [4]
   Indeed, an explicit ``Case`` can no longer be issued after the
   timeout, even if the timeout has not been observed by the contract,
   since the timeout is checked before the ``Inputs``. However, a
   participant may want to trigger a timeout in cases where no other
   ``Inputs`` are needed, in order to trigger one or more payments, for
   example. In the current implementation of the semantics that would be
   done by issuing a transaction with an empty list of ``Inputs``.
