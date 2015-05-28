local lpeg = require'lpeg'
local re = require'lpeg.re'
local pp = require'pp'

local patt = re.compile[[

	prog <- s dect                             --program: must be first!

	lcom <- '//' [^%nl]* %nl                   --line comment
	ncom <- '/*' (ncom / (!'*/')) '*/'         --nested comment
	s    <- (%s / lcom / ncom)*                --whitespace

	dec  <- %d+                                --decimal
	sdec <- '-'? dec                           --signed decimal
	exp  <- [eE] [+-]? dec                     --exponent part
	df1  <- sdec '.'                           --d. float
	df2  <- sdec? '.' dec                      --.d or d.d float
	dint <- { sdec }                           --decimal int
	df   <- { (df2 / df1) exp? }               --decimal float
	hex  <- { '0x' [0-9a-fA-F]+ }              --hex literal
	num  <- hex / df / dint                    --number literal

	str  <- '"' { [^"]* } '"'                  --string literal

	name <- { [a-z_] [a-z0-9_]* }              --name literal

	darr <- '[..]'                             --dynamic array
	xarr <- '[' num ']'                        --fixed array
	ptr  <- '^'                                --pointer
	tmod <- ptr / darr / xarr                  --type modifier
	tmex <- tmod (s tmod)*                     --type modifier expression
	type <- (tmex s)? name                     --type declaration

	sind <- '.' s name                         --struct indexing operator
	aind <- '[' s texp s ']'                   --array indexing operator
	ind  <- sind / aind                        --indexing operator
	nnd  <- grp / num / name                   --numeric operand
	nexp <- nnd (s ind)*                       --numeric expression
	aop  <- { '+' }                            --addition op
	sop  <- { '-' }                            --substraction op
	top  <- aop / sop                          --term operators
	mop  <- { '*' }                            --multiplication op
	dop  <- { '/' }                            --division op
	fop  <- mop / dop                          --factor operators
	fnd  <- nexp                               --factor operand
	fexp <- fnd (s fop s fnd)*                 --factor expression
	tnd  <- fexp                               --term operand
	texp <- tnd (s top s tnd)*                 --term expression
	grp  <- {| '(' s texp s ')' |}             --eval group

	asig <- name s '=' s expr

	blks  <- blk / stmt

	if   <- 'if' s expr s blks
	for  <- 'for' s name s blks
	ret  <- 'return' s expr
	carg <- expr
	cags <- carg (s ',' s carg)*
	call <- name s '(' cags ')'
	stmt <- { ret / if / for / call / asig / decl }

	farg <- name s ':' s type
	fags <- '(' (s farg)? (s ',' s farg)* s ')'
	fret <- type
	stml <- (stmt s ';' s)*
	blk  <- '{' s stml s '}'
	func <- {| fags s ('->' s fret)? s blk |}

	sbod <- '{' s dect s '}'
	strt <- 'struct' s sbod

	expr <- {| strt / func / texp / str |}     --expression
	decl <- {|                                 --declaration
		{:name: name :} s
		(':' s ({:type: type :} s)? )?
		('=' s {:expr: expr :} s)?
	|}
	dect <- {| (decl s ';' s)* |}              --declaration list

]]

local program = [[
i = 1;
i: int;

i: int = -0;
i: int = -1;
i: int = 0x0aF1;

f: float = 0.1;
f: float = 0.1e2;
f: float = 0.1e+2;
f: float = 0.1e-2;

f: float = .1;
f: float = .1e2;
f: float = .1e+2;
f: float = .1e-2;

f: float = 1.;
f: float = 1.e2;
f: float = 1.e+2;
f: float = 1.e-2;

s: string = "Jon";

e := 0.5 * (x+y);

c := 0;              // an int
s := "Jon";          // a string
a := 0.5 * (x+y);    // a float

counter: int;
name: string;
average: float;

// A function that accepts 3 floats as parameters and returns a float
sum := (x: float, y: float, z: float) {
	return x + y + z;
};

Vector3 := struct {
	x: float;
	y: float;
	z: float;
};

x := 111111111;
]]

pp(patt:match(program))

--pp(re.match('1234', '{| {:s: . :} |}*'))

