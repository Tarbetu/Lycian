# Lycian Programming Language

## Overview
Lycian is an **immutable, dataflow-oriented** programming language designed for computational tasks. Unlike languages focused on communication (async/await) or imperative control flow, Lycian treats programs as **dependency graphs** where the compiler automatically extracts parallelism and applies memoization.

Key features:
- **Pure functional** with automatic memoization
- **Parallel by default** - independent operations execute concurrently
- **Emergent state** - state arises from relations, not commands

*Currently in early development - see [Status](#status) below.*

## Quick Look
```lycian
# Functions with literal return types
GiveFive() -> 5 = 5
# Or avoid unnecessary paranthesises and type declaration like:
# GiveFive = 5

# Automatic parallelization of pure functions
ProcessData(items: List(Integer)) -> List(Int32) =
    items.Map(|x| x * 2)  # Parallel by default

# Bang notation will stop the parallel execution and the results of will not memorized.
# product_of_two is another function here will be computed at call.
product_of_two = ProcessData([GetTime!(), GetTime!()]);
```

*See [Overview](/doc/overview.md) for detailed examples.*

## Status

Lycian is in **early development**. Current progress:

- Parsing and AST generation (Done)
- Scope analysis and binding resolution (Done)
- Type inference (constraint-based, in progress)

The compiler isn't functional yet, but the core architecture is taking shape. You can:
- Track development progress on GitHub
- Discuss design ideas in Issues
- Contribute if interested (see Contributing below)

## Motivation

### The Three Kinds of Programming Tasks

Modern programming serves three distinct purposes:

1. **Communicational tasks**: Responding to external events (web servers, UI, message queues)
2. **Routine tasks**: Triggered by conditions like time or hardware signals (cronjobs, GPIO)
3. **Computational tasks**: Pure transformation of data, independent of external triggers

Most languages today optimize for communication (async/await, event loops) or routines (schedulers). **Lycian focuses on computation** - the domain lambda calculus was designed for.

### The Shifting Nature of Computation

Since the early stages of von Neumann computers, computation has evolved:
- **CPUs** became directors of CPU cores
- **GPUs** transformed from graphics processors to essential computational units
- **Clusters** act as single logical machines

Yet our languages haven't caught up. We still:
- Write sequential code, manually managing parallelism
- Mix computational logic with infrastructure concerns
- Choose between abstraction (Haskell) or control (C), sacrificing one for the other

### Lycian's Approach

Lycian aims to:
- **Distinguish** computational tasks from communication/routine tasks
- **Enable** natural parallelism through dataflow semantics
- **Provide** deterministic behavior without verbose memory management

How?
- Everything is expressed as **computational states** (immutable transformations)
- **Parallel execution is default** - the compiler extracts dependency graphs
- **Automatic memorization of function results** replaces manual memory management

## Philosophy

Lycian also has a "Zen" text! It might be vague to declare a "Zen" in early stages, but my main goal is determine the motivation and remind myself the fundamental ideas of Lycian.
[The Way of Lycian](/doc/the_way_of_lycian.md)

## Contributing

Lycian's development follows a trial-and-error approach as I learn compiler design on the go.

Ways to help:
- Review code and suggest improvements
- Discuss language design in Issues
- Maybe a little chat would work! Don't hesitate to say hi if you just want to speak about Lycian.
