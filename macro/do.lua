--- An intelligent 'loop-unrolling' macro.
-- `do_` defines a named scoped macro var which is the loop iterator.
--
-- `tuple` is an example of how the expansion of a macro can be
-- controlled by its context
-- @module macro.do
local M = require 'macro'

--- Expand a loop inline.
-- @p var the loop variable
-- @p start initial value of `var`
-- @p finish final value of `var`
-- @p stat the statement containing `var`
-- @macro do_
M.define('do_(v,s,f,stat)',function(var,start,finish,statements)
    -- macros with specified formal args have to make their own putter,
    -- and convert the actual arguments to the type they expect.
    local put = M.Putter()
    var,start,finish = var:get_iden(),start:get_number(),finish:get_number()
    M.push_macro_stack('do_',var)
    -- 'do_' works by setting the variable macro for each value
    for i = start, finish do
        put:name 'set_':name(var):number(i):space()
        put:tokens(statements)
    end
    put:name 'undef_':name(var)
    put:name '_DROP_':string 'do_':space()
    return put
end)

-- an example of conditional expansion.
-- `tuple` takes a list of variable names, like a declaration list except that it
-- must end with a line end.
-- @macro tuple
M.define('tuple',function(get)
    get:expecting '('
    local N = get:number()
    get:expecting ')'
    local names = get:names '\n'
    for _,name in ipairs(names) do
        M.define(name,function(get,put)
            local loop_var = M.value_of_macro_stack 'do_'
            if loop_var then
                local loop_idx = tonumber(M.get_macro_value(loop_var))
                return put:name (name..'_'..loop_idx)
            else
                local out = {}
                for i = 1,N do
                    out[i] = name..'_'..i
                end
                return put:names(out)
            end
        end)
    end
end)
