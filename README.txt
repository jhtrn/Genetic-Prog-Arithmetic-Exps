Project Name: 
Genetic Programming Arithmetic Expressions

Introduction: 
This Genetic Program project involves a population of critters, each of which is a Lisp arithmetic expression. An initial population of random Lisp arithmetic expressions will be generated. After that, a purge will happen which will calculate the fitness of each expression in the population and will get rid of the lower half of the less fit expressions. Following that, crossover will occur between random pairs of all expressions within the population. The random pairs will produce children which will repopulated the population size back to normal. There is a 1% chance that a mutation will occur somewhere in the population. This pattern will repeat again for 100 generations. After 100 generations, the final population, the best and worst expressions, and the best, worst, and average fitness will be displayed.

External Requirements:
None

Build, Installation, and Setup:
Install common lisp.
Open terminal and run common lisp using the clisp command.
	- $ clisp
Load the lisp file with the command.
	- (load "path_name_of_file/GPArithmeticExps.lisp")
Run the function by calling 
	- (genetic-prog).

Usage:
Genetic Programming is used to solve simple problems which are computationally intensive.

Extra Features:
Mutation occurs 2% of the time instead of 1% for more variety.

Bugs:
None
