local M = require 'macro'

M.define('with',function(get,put)
  M.set_scoped_macro('.',function()
    local lt,lv = get:peek(-1,true) --  peek before the period...
    if lt ~= 'iden' and lt ~= ']' then
      return '_var.'
    else
      return nil,true -- pass through
    end
  end)
  local expr = get:upto 'do'
  return 'do local _var = '..tostring(expr)..'; '
end)
