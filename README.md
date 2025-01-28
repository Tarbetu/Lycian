# Lycian Programming Language

## Overview

Lycian is a modern programming language that combines functional programming principles with object-oriented features. It introduces a unique approach to state management and concurrent programming, making it particularly suitable for building reliable, maintainable, and scalable software systems.

The language emphasizes compile-time guarantees through its powerful type system, where types are essentially callable functions representing their possible return values. This includes the ability to use literal values as types at compile-time, enabling sophisticated type-level programming and compile-time verification.

## Key Features

### Type and Safety System
- **Types as Callable Functions**: Every type represents the set of its possible values
- **Compile-time Literal Types**: Literal values can be used as types for compile-time verification
- **Pure/Impure Distinction**: Compile-time verification of function purity
- **Class-State System**: Type-safe state transitions
- **Pattern Matching**: Comprehensive pattern matching with type enforcement

### Concurrency and Performance
- **Default Async Behavior**: Built-in support for concurrent programming
- **Automatic Parallelization**: Parallel execution of independent impure operations
- **Compile-time Memory Management**: Deterministic memory handling without GC overhead
- **Zero-cost Abstractions**: Pattern matching and async operations compile to efficient code

### Development Experience
- **Clear Syntax**: Minimal ceremony, maximum readability
- **Convention-based Development**: Consistent naming patterns indicate visibility and behavior
- **First-class Documentation**: Integrated documentation system (planned)

## Language Details

### Functions and Types

In Lycian, types are essentially callable functions representing their possible return values. Additionally, compile-time constant values can be used as types:

```lycian
# Integer is a type that represents all possible integer values
x: Integer = 5  # x is bound to one of Integer's possible values

# Literal values can be used as types
give_five -> 5 = 5
computed_five -> (2 + 3) = 5

# Function return types create distinct subtypes
Counter(x: Integer) -> Increment = Increment(x)
Increment(x: Integer) -> Integer = x + 1

count = Counter(5)     # Type is Increment
value = Increment(5)   # Type is Integer

sum = count + value    # Type error: Increment and Integer are different types

# Type-safe calculations with literal types
Add(x: 5, y: 3) -> 8 = x + y
result = Add(5, 3)  # Type is literally 8

# Pattern matching with literal types
match value:
    18 -> "Adult"            # Matches literal type 18
    x when x < 18 -> "Minor" # Matches any Integer less than 18
    _ -> "Unknown age"
```

### Class and State System

Classes in Lycian define type constructors with distinct state types:

```lycian
Connection:
    Connected(socket: Socket) -> SendData
    Disconnected -> Reconnect
    Failed(error: Error) -> Reconnect

    # Each method returns its own type
    SendData(data: Data) -> Result = ...
    Reconnect() -> Connection = ...

# Type safety through state transitions
connection = Connected(new_socket)  # Type is SendData
result = connection.SendData(data)  # Type is Result
```

### Pure and Impure Functions

```lycian
# Pure function with compile-time type checking
Calculate(x: Integer) -> 42 = 
    match x:
        21 -> 42
        _ -> error("Only 21 is accepted")

# Impure operations must implement Mutation
DatabaseClient:
    implementing Mutation

    # Return type indicates possible states
    Save(record: Record) -> Result =
        match validate(record):
            Ok(valid) -> self.write(valid)
            Err(e) -> Err(e)

    # Multiple impure operations are automatically parallel
    SaveMultiple(records: List) -> Result =
        results = records.Map(self.Save)
        CollectResults(results)
```

### Pattern Matching with Types

```lycian
Optional:
    Some(value)
    None

    # Pattern matching on types
    Match(self) -> String =
        match self:
            Some(5) -> "Exactly five"          # Literal type match
            Some(Integer) -> "Some number"     # Type match
            None -> "Nothing"

# Type composition through pattern matching
Parser(input: String) -> ValidateResult = ...
Validator(input: ValidateResult) -> ProcessResult = ...

result = input
    .Parser()      # Type: ValidateResult
    .Validator()   # Type: ProcessResult
```

## Project Structure

The Lycian project consists of several components:

- **Compiler**: Written in Rust, handles parsing, analysis, and code generation
- **Standard Library**: Core types and utilities (MIT Licensed)
- **Language Server**: IDE integration support (planned)
- **Package Manager**: Dependency management system (planned)

## Getting Started

### Building from Source

[Coming soon]

### First Steps

[Coming soon]

## Contributing

We welcome contributions! Areas where help is particularly appreciated:

- Compiler development
- Standard library implementation
- Documentation improvements
- Test suite expansion
- IDE integration

### Development Setup

[Coming soon]

## License

This project uses dual licensing:
- **Compiler**: GNU Lesser General Public License (LGPL)
- **Standard Library**: MIT License

## Current Status

Lycian is currently in early development. The following features are being worked on:

- [ ] Type system
- [ ] Pattern matching
- [ ] Compile-time memory management
- [ ] Standard library foundations
- [ ] Documentation system

## Community

[Coming soon]

---

This README will be updated as the project evolves.
