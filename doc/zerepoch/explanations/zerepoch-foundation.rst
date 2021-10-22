.. _what_is_zerepoch_foundation:

What is Zerepoch Foundation?
==========================

In order for an application to run its :ref:`trusted kernel<what_is_the_zerepoch_platform>` of logic as a script on a :ref:`ledger<what_is_a_ledger>`, the ledger needs a way of specifying and executing scripts.
Scripts are simply programs, so this means we need a *programming language*.

Zerepoch Core
-----------

In the Zerepoch Platform, this language is *Zerepoch Core*.
Zerepoch Core is a variant of the lambda calculus, a well-studied formalism for computing.

.. note::
    Zerepoch Core is our "assembly language".
    Trust me, you don't want to see any!
    Dealing with that is the compiler's job.

Zerepoch Core is designed for simplicity, determinism, and to allow careful cost control of program execution.
Using the lambda calculus makes it an easy compilation target for functional programming languages, and allows us to have a simple, formally verified evaluator.

Zerepoch Tx
---------

Writing Zerepoch Core by hand is not a job for a human!
It is designed to be written by a compiler, and the Platform provides a compiler from a subset of Haskell to Zerepoch Core.
This allows you to seamlessly write applications in Haskell, while compiling part of the code to on-chain Zerepoch Core, and part into an off-chain application.

Supporting "mixed" code in this way enables libraries written with the Zerepoch Haskell SDK to share logic and datatypes across both parts of the application, reducing the risk of errors significantly.

Further reading
---------------

The formal details of Zerepoch Core are in its specification :cite:p:`zerepoch-core-spec`.
The design is discussed in :cite:t:`zerepoch-report`.

For more about Zerepoch Tx, see the :ref:`tutorial<zerepoch_tx_tutorial>`.
