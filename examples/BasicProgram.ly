# A module is akin to classes in other languages. Can be used as namespaces
module Program:
	# Function declaration
	add_two(a: Integer) -> Integer = a + 2

	# Function overloading!
	add_two(a: Float) -> Float = a + 0.2

	# Pattern matching!
	add_two(42) = 42

	# Function without type annonations
	give_five = 5

	# Literals can be used as types, the literal types can be used as compile time values
	# Well, you don't need to specify them
	give_the_answer -> 42 = 42

	# All functions need to declare `main` function
	# Main need to take an program state, and this is implictly given by the compiler
	# Also, states are implicitly declared. They are "Running" and "Exit".
	# Users can declare their states to define program rules.
	main(Program) =
		# Nested functions are allowed and defined in function scope
		# Also, you can call functions without paranthesis
		output -> IO = IO.print give_the_answer

		IO is a monad, which can matched
		match output:
			# You can easily call monads if you need trigger them
			IO.ready 	-> output()
			# Recursively calls main until IO is released.
			# This don't blow the stack because of tail call
			IO.busy  	-> main(Program.Running)
			# Exit program by calling main with Exiting state
			IO.consumed -> main(Program.Exit(0))
