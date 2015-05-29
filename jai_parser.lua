
if not ... then return require'jai_parser_test' end

local lpeg = require'lpeg'
local re = require'lpeg.re'
local pp = require'pp'
local glue = require'glue'
local assert = glue.assert

local jai = {}

local defs = {}

--literal parsers ------------------------------------------------------------

function defs.d2f(s)
	return (assert(tonumber(s), 'invalid number %s', s))
end

function defs.d2i(s)
	return (assert(tonumber(s), 'invalid number %s', s))
end

function defs.h2i(s)
	return (tonumber(s, 16))
end

defs.e2s = {['\\'] = '\\', n = '\n', r = '\r', t = '\t', b = '\b'}

function defs.h2s(s)
	return (string.char(tonumber(s, 16)))
end

function defs.d2s(s)
	local n = tonumber(s)
	assert(n <= 255, 'decimal escape out of range %s', s)
	return (string.char(n))
end

function defs.cat(t)
	return (table.concat(t))
end

--parser ---------------------------------------------------------------------

function defs.err(s)
	if #s > 0 then
		error(s)
	end
end

local builtin_types = {int = {}}
local types = {} --{sig = def}
jai.types = types

glue.update(types, builtin_types)

local function type_signature(s)
	return s
end

local function type_def(s)
	return {s = s}
end

local function find_type(sig)
	return types[sig]
end

function defs.add_type(s, ...)
	if s == 'struct' then
		pp('>>>', s, ...)
	end
	local sig = type_signature(s)
	local def = find_type(sig)
	if not def then
		def = type_def(s)
		types[sig] = def
	end
	return sig
end

local decls = {} --{name = type_id}
jai.decls = decls

function defs.add_decl(name, type_id)
	decls[name] = type_id
end

function defs.error(s)
	error(s)
end

local structs = {}
local struct

function defs.begin_struct(type)
	struct = {type = type}
end

function defs.end_struct()
	return struct
end

local grammar = re.compile([[

	grammar <- program

	--comments ----------------------------------------------------------------

	lcom    <- '//' [^%nl]* %nl                   --line comment
	ncom    <- '/*' (ncom / (!'*/')) '*/'         --nested comment
	s       <- (%s / lcom / ncom)*                --whitespace

	--number literals ---------------------------------------------------------

	dec     <- %d+                                --decimal
	sdec    <- '-'? dec                           --signed decimal
	dint    <- sdec -> d2i                        --decimal int
	df1     <- sdec '.'                           --d. float
	df2     <- sdec? '.' dec                      --.d or d.d float
	exp     <- [eE] [+-]? dec                     --exponent part
	df      <- ((df2 / df1) exp?) -> d2f          --decimal float
	hexd    <- [0-9a-fA-F]                        --hex digit
	hex     <- '0x' hexd+ -> h2i                  --hex int
	num     <- hex / df / dint                    --number literal

	--string literals ---------------------------------------------------------

	escs    <- [\nrtb] -> e2s                     --literal escape
	esch    <- 'x' (hexd^2 -> h2s)+               --hex escape
	escd    <- (%d %d^-2) -> d2s                  --decimal escape
	esc     <- '\' (escs / esch / escd)           --escape sequence
	strl    <- '"' (esc / {[^"]})* '"'            --string literal
	str     <- {| strl |} -> cat                  --concatenate captures

	--identifiers -------------------------------------------------------------

	name    <- [a-zA-Z_][a-zA-Z_0-9]*

	--expressions -------------------------------------------------------------

	group    <- '(' s expr s ')'
	term     <- group / str / num / name
	farg     <- term
	fargs    <- farg? (s ',' s farg)*
	fcall    <- '(' s fargs s ')'
	a_access <- fcall s '[' s expr s ']'
	s_access <- '.' s a_access
	unarypm  <- [+-] s s_access
	negation <- [!~] s unarypm
	deref    <- '*' s negation
	muldiv   <- deref (s [*/%] s deref)*
	arith    <- muldiv (s [+-] s muldiv)*
	shift    <- arith (s ('<<' / '>>') s arith)*
	comp     <- shift (s ('<' / '>' / '<=' / '>=') s shift)*
	equality <- comp (s ('==' / '!=') s comp)*
	bitand   <- equality (s '&' s equality)*
	bitor    <- bitand (s '|' s bitand)*
	logicand <- bitor (s '&&' s bitor)*
	logicor  <- logicand (s '||' s logicand)*
	ternary  <- logicor s '?' logicor s ':' s logicor

	expr <- arith / func_val / type / str / name / num

	--enums -------------------------------------------------------------------

	enum_elem <- name s ('=' s expr s)?

	enum_body <- enum_elem (s [;,] s enum_elem)* (s [;,])?

	enum <- 'enum' (s type)? s '{' s enum_body s '}'

	--structs -----------------------------------------------------------------

	struct_field <- name s ':' s type s ';'

	struct_body <- (struct_field s)*

	struct <-
		('struct' / 'union') s
		'{' s struct_body s '}'

	type <- (enum / struct / func_sig / name) -> add_type

	--function types ----------------------------------------------------------

	func_retval <- type
	func_retvals <- func_retval (s ',' s func_retval)*
	func_arg <- name s ':' s type
	func_args <- func_arg? (s ',' s func_arg)*
	func_sig <- '(' s func_args s ')' (s '->' s func_retvals)?
	func_val <- func_sig s stmt

	--statements --------------------------------------------------------------

	assign <- name s '=' s expr

	return <- 'return' s expr
	if <- 'if' s expr s stmt
	elif <- 'elif' s expr s stmt
	else <- 'else' s stmt
	defer <- 'defer' s stmt
	new <- 'new' s type
	delete <- 'delete' s name

	stmt <-
		block /
		return /
		if / elif / else /
		defer / new / delete /
		decl / assign

	stmt_list <- (stmt s ';' s)*

	block <- '{' s stmt_list s '}'

	--declarations ------------------------------------------------------------

	decl <- (
			{name}
			s ':' s type?
			(s '=' s expr)?
			s ';'
		) -> add_decl

	decls <- (decl s)*

	program <- (s decls s !.) / (.* -> error)

]], defs)

function jai.parse(s)
	return grammar:match(s)
end

return jai
