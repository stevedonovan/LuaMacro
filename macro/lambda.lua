--- Short anonymous functions (lambdas).
-- This syntax was inspired by a suggestion by lhf, and is suited
-- to any naive token-processor because the payload is always inside parens.
-- It is an example of a macro associated with a 'operator' character.
-- @module macro.lambda
local M = require 'macro'

--- An anonymous function macro.  `\x(x+10)` is short for
-- `function(x) return x+10 end`. There may be a number of formal argumets,
-- e.g. `\x,y(x+y) or there may be none, e.g. `\(somefun())`.
-- @macro \
M.define ('\\',function(get,put)
	local args, body = get:names('('), get:list()
	return put:keyword 'function' '(' : names(args) ')' :
        keyword 'return' : list(body) : space() : keyword 'end'
end)

