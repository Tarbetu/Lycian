# Lycian Programming Language

## Key Features

### Type and Safety System
- **Compile-time Literal Types**: Literal values can be used as types for compile-time verification
- **Pure/Impure Distinction**: Compile-time verification of function purity
- **Class-State System**: Type-safe state transitions
- **Pattern Matching**: Patterns are the types in here, we are matching with variants of a class or inheritance

### Concurrency and Performance
- **Default Async Behavior**: Built-in support for concurrent programming
- **Automatic Parallelization**: Parallel execution of independent pure operations
- **Automatic Async**: Async execution of independent impure operations
- **Compile-time Memory Management**: Deterministic memory handling without GC overhead
- **Zero-cost Abstractions**: Pattern matching and async operations compile to efficient code

### Development Experience
- **Clear Syntax**: Minimal ceremony, maximum readability
- **Convention-based Development**: Consistent naming patterns indicate visibility and behavior

## Language Details

### Functions and Types

In Lycian, types are essentially callable functions representing their possible return values. Additionally, compile-time constant values can be used as types:

```lycian
x -> Int32 = 5  # x is bound to one of Int32's possible values

# Literal values can be used as types
give_five -> 5 = 5
computed_five -> 5 = 2 + 3 // The compiler will check that whether it is true

# Type-safe calculations with literal types
Add(x: 5, y: 3) -> 8 = x + y
result = Add(5, 3)  # Type is literally 8

# Pattern matching with literal types
match value:
    18 -> "Adult"            # Matches literal type 18
    x when x < 18 -> "Minor" # Matches any Int32 less than 18
    _ -> "Unknown age"
```

### Class and State System

Classes in Lycian define type constructors with distinct state types:

```lycian
Connection:
    Connected(socket: Socket)
    Result(code: Int32)
    Disconnected
    Failed(error: Error)

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
Calculate(numbers: List(Int32)) -> List(Int32) =
    # Multiple Pure computations are computed parallelly
    numbers.Map: |x| x * 2

# Impure operations may implement Mutation to enforce bang notation
DatabaseClient:
    implementing Mutation

    # Return type indicates possible states
    Save(record: Record) -> Result =
        match validate(record):
            Ok(valid) -> self.write(valid)
            Err(e) -> Err(e)

    # Multiple impure operations are automatically async
    SaveMultiple(records: List) -> Result =
        results = records.Map(self.Save)
        CollectResults(results)
```

### Pattern Matching with Types

```lycian
Option:
    Some(value)
    None

    # Pattern matching on types
    Match(self) -> String =
        match self:
            Some(5) -> "Exactly five"          # Literal type match
            Some(Int32) -> "Some number"       # Type match
            None -> "Nothing"

# Type composition through pattern matching
Parser(input: String) -> ValidateResult = ...
Validator(input: ValidateResult) -> ProcessResult = ...

result = input
    .Parser()      # Type: ValidateResult
    .Validator()   # Type: ProcessResult
```
