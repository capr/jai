
local re = require'lpeg.re'
local pp = require'pp'

pp(re.compile([[

	main <- expr

	name        <- [a-z]+
	factor      <- {name}
	factor_op   <- {[*/]}
	factor_expr <- {| factor (factor_op factor)* |}
	term        <- factor_expr
	arith_op    <- {[+-]}
	arith_expr  <- {| term (arith_op term)* |}

	expr <- arith_expr

]], defs):match'a+b*c-d')

pp(re.compile[[

	main <- expr !.

	s    <- %s*
	n    <- s ({%d} -> '[%1]')
	p    <- n (s '+' n)*
	expr <- {~ p ~}

]]:match'1 + 2 + 3')

