
import AST._ 

/*
Things that add static constraints. These are things that are explicitly
provided or could be inferred by walking the AST once. Adds initial constraints to
symbolic execution engine:
* args - ranges, subranges
* return type for function - might have a range
* operations on ints - add, minus, mult, div 
* ntos - max # of possible digits = max length of string - might be taken care of by solver
* ston - max length of string = max # of possible digits - might be taken care of by solver
*/

// Target language is rosette