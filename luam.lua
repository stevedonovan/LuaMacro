--- Front end for LuaMacro, a Lua macro preprocessor.
-- @program luam

-- adjust the path so that this script can see the macro package
local path = arg[0]:gsub('[^/\\]+$','')
package.path = path .. '?.lua;' .. package.path
local macro = require 'macro'
require 'macro.builtin'

--- Using luam.
-- @usage follows
local usage = [[
LuaMacro 2.1, a Lua macro preprocessor and runner
    -l  require a library
    -e  statement to be executed
    -c  error context to be shown (default 2)
    -d  dump preprocessed output to stdout
    -i  interactive prompt
    <input>    Lua source file
]]

-- parsing the args, the hard way:
local takes_value = {l = '', e = '', c = 2}

local args = {}
local idx,i = 1,1
while i <= #arg do
    local a = arg[i]
    local flag = a:match '^%-(.+)'
    local val
    if flag then
        if #flag > 1 then -- allow for -lmod, like Lua
            val = flag:sub(2)
            flag = flag:sub(1,1)
        end
        -- grab the next argument if we need a value
        if takes_value[flag] and not val then
            i = i + 1
            val = arg[i]
        end
        -- convert the argument, if required
        local def = takes_value[flag]
        if type(def) == 'number' then
            val = tonumber(val)
        end
        args[flag] = val or true
    else
        args[idx] = a
        idx = idx + 1
    end
    i = i + 1
end

if not args[1] and not args.i then
    print(usage)
    os.exit()
elseif args[1] then
    args.input_name = args[1]
    args.input,err = io.open(args[1],'r')
    if err then return print(err) end
    table.remove(args,1)
end
-- set defaults, if flags not specified
for k,v in pairs(takes_value) do
    if not args[k] then
        args[k] = v
    end
end


local function runstring (code,name,...)
    local res,err = loadstring(code,name)
    if not res then
        local lno = err:match(':(%d+):')
        lno = tonumber(lno)
        local l1,l2 = lno-args.c,lno+args.c
        local l = 1
        for line in code:gmatch '[^\n]+' do
            if l >= l1 and l <= l2 then
                if l == lno then io.write('*') end
                print(l,line)
            end
            l = l + 1
        end
        io.stderr:write(err,'\n')
        os.exit(1)
    end
    return res(...)
end

local function subst (ins,name)
    local buf,i = {},1
    local outf = {write = function(self,v)
        buf[i] = v
        i = i + 1
    end}
    macro.substitute(ins,outf,name)
    return table.concat(buf)
end

local function subst_runstring (ins,name,...)
    local buf = subst(ins,name)
    if args.d then
        print(buf)
    else
        return runstring(buf,name,...)
    end
end

-- Lua 5.1/5.2 compatibility
local pack = table.pack
if not pack then
    function pack(...)
        return {n=select('#',...),...}
    end
end
if not unpack then unpack = table.unpack end

local function eval(code)
    local status,val,f,err,rcnt
    code,rcnt = code:gsub('^%s*=','return ')
    f,err = loadstring(code,'TMP')
    if f then
        res = pack(pcall(f))
        if not res[1] then err = res[2]
        else
            return res
        end
    end
    if err then
        err = tostring(err):gsub('^%[string "TMP"%]:1:','')
        return {nil,err}
    end
end

local function interactive_loop ()
    os.execute(arg[-1]..' -v') -- for the Lua copyright
    print 'Lua Macro 2.1 Copyright (C) 2007-2011 Steve Donovan'

    local function readline()
        io.write(_PROMPT or '> ')
        return io.read()
    end

    require 'macro.all'
    _G.macro = macro
    macro.define 'quit os.exit()'

    local line = readline()
    while line do
        local ok,s = pcall(subst,line..'\n')
        if not ok then
            s = s:gsub('.-:%d+:','')
            print('macro error: '..s)
        else
            if args.d then print(s) end
            local res = eval(s)
            if not res[1] then
                print('expanded: '..s)
                print('error: '..res[2])
            elseif res[2] ~= nil then
                print(unpack(res,2))
           end
        end
        line = readline()
    end
end

macro.set_package_loader()

if args.l ~= '' then require(args.l) end

if args.e ~= '' then
    subst_runstring(args.e,"<temp>")
else
    if args.input then
        arg = args
        subst_runstring(args.input,args.input_name,unpack(args))
    elseif args.i then
        interactive_loop()
    end
end
