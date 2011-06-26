----
-- a basic class mechanism.
-- Used for some of the demonstrations.

return function (base)
  -- OOP with single inheritance
  local klass,cmt = {},{}
  if base then -- 'fat metatable' inheritance
    for k,v in pairs(base) do klass[k] = v end
  end
  klass.__index = klass
  -- provide a callable constructor that invokes user-supplied ctor
  function cmt:__call(...)
    local obj = setmetatable({},klass)
    if klass._init then klass._init(obj,...)
    elseif base and base._init then base._init(base,...) end
    return obj
  end
  setmetatable(klass,cmt)
  return klass
end
