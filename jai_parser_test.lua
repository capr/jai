local pp = require'pp'
local jai = require'jai_parser'

local tests = {}
local function add(s)
	tests[#tests+1] = s
end

local function run_all()
	for i=1,tests do
		pp(jai.parse(tests[i]))
	end
end

local function run(s)
	pp(jai.parse(s))
end

add[[
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
]]

add[[
i: = 1;
i: int;
]]

add[[
s: string = "Jon";
]]

add[[
e := 0.5 * (x+y);
]]

add[[
c := 0;              // an int
s := "Jon";          // a string
a := 0.5 * (x+y);    // a float
]]

add[[
counter: int;
name: string;
average: float;
]]

add[[
f := (x: float, y: float) -> int, float {
	if x + 2
		for e
			return 2;
};
]]

run[[
f : ();
f : () -> int;
f : () -> int, float;
f : (x: int);
f : (x: int, y: float);
f := () {};
f := (x: int) -> int {};
]]

add[[
Vector3 := struct {
	x: float;
	y: float;
	z: float;
};
]]

add[[
c := struct {
	x: int;
	y: int;
};

]]

add[[
Value := enum u16 {
	VALUE_ZERO = 0;
	VALUE_ONE;
	VALUE_TWO;
	VALUE_THREE = VALUE_TWO;

	VALUE_EIGHT = MIDDLE_VALUE;
	VALUE_NINE;
	VALUE_TEN = 500;
	VALUE_HIGH;
};

MIDDLE_VALUE := 8;
]]

pp('types', jai.types)
pp('decls', jai.decls)
