# A class is a essansial part of Lycian. They can be inherited, can
# Modules does not need any keyword since they are at top-level
Program:
	# Method declaration.
    # Like modules, they need not any keywords and they have to defined lower-level of modules
	add_two(a: Int32) -> Integer = a + 2

	# Method can be overloadable.
    # Can be more spesific to a type, like a literal type, or can be more general like a superclass.
    # However, they have to be in a similar ancestorship line.
	add_two(42) = 44

	# Literals can be used as types, the literal types can be used as compile time values
    # And you can escape the parantheses if you don't need parameters.
	give_the_answer -> 42 = add_two(40)

	# All functions need to declare `main` function
	# Main need to take an program state, and this is implictly given by the compiler
	# Also, states are implicitly declared. They are "Running" and "Exit".
	# Users can declare their states to define program rules.
	main(Program) =
		# Functions are allowed and defined in methods, and they can be nested
        # Unlike method, you don't have to give a return type for them
		output = IO.print(give_the_answer)

        # Below is just a concept, but might to note down here
		# IO is a ADT, which can matched
        # However, you can wait the IO with strict call like `IO.print!(give_the_answer)`
		match output:
			# You can easily call ADTs if you need trigger them
			IO.ready 	-> output()
			# Recursively calls main until IO is released.
			# This don't blow the stack because of tail call
			IO.busy  	-> main(Program.Running)
			# Exit program by calling main with Exiting state
			IO.consumed -> main(Program.Exit(0))
