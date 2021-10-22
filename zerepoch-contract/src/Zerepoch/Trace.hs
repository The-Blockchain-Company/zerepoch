{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-

A trace is a sequence of actions on some Zerepoch contracts. This module defines
the types and functions for constructing and running traces.

Some examples of actions that can be performed during a trace:

* Starting a new instance of a contract
* Calling an endpoint on a contract instance
* Inspecting the state of a contract instance
* Waiting for blockchain events to happen
* Modifying the state of the blockchain emulator

The syntax of traces is given as a number of @freer-simple@ effects under
"Zerepoch.Trace.Effects". There is a concrete list of trace effects to be used in
a testing environment ('Zerepoch.Trace.Emulator.EmulatorTrace'), and a similar
type for use in the Playground ('Zerepoch.Trace.Playground.PlaygroundTrace').

** Running Traces

Running a trace means handling its effects.
'Zerepoch.Trace.Emulator.runEmulatorStream' interprets the
'Zerepoch.Trace.Emulator.EmulatorTrace' type as a stream of emulator events. This
stream includes all events that happen in the emulated environment while the
trace is running: Transactions validated or rejected, endpoint calls, slot
changes, and more. The module "Wallet.Emulator.Folds" defines a number of folds
that extract interesting data from emulator event streams.

-}
module Zerepoch.Trace(
    module X
    , module Extract
    , Scheduler.SchedulerLog
    , Scheduler.ThreadEvent
    ) where

import           Zerepoch.Trace.Emulator         as X
import           Zerepoch.Trace.Emulator.Extract as Extract
import           Zerepoch.Trace.Scheduler        as Scheduler

{- Note [Trace]

Traces describe systems in which many things may happen at the same time.
Imagine a trace that starts two contract instances and then waits for both of
them to finish some initialisation procedure, while the node keeps adding new
blocks to the chain in the background.

To deal with this concurrency, traces are interpreted using a scheduler that
runs some concurrent threads. Each contract instance runs in its own thread,
and (in the emulated environment) there is a system thread that takes care of
producing new blocks periodically.

The scheduler defined in "Zerepoch.Trace.Scheduler"
is pure and deterministic. It runs in a single GHC thread, and it performs the
same actions in the same order every time the same trace is run. So a thread
here is a coroutine that runs for a little while before yielding control
back to the scheduler, which then selects the next coroutine to run.

-}
