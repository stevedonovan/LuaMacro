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
luam, a Lua macro preprocessor and runner
    -l  library
    -e  statement to be executed
    -c  error context to be shown (default 2)
    -d  dump preprocessed output to stdout
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

if not args[1] then
    print(usage)
    os.exit()
else
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
    res(...)
end

local function subst_runstring (ins,name,...)
    local buf,i = {},1
    local outf = {write = function(self,v)
        buf[i] = v
        i = i + 1
    end}
    macro.substitute(ins,outf,name)
    buf = table.concat(buf)
    if args.d then
        print(buf)
    else
        runstring(buf,name,...)
    end
end

if args.l ~= '' then require(args.l) end

if args.e ~= '' then
    subst_runstring(args.e,"<temp>")
else
    arg = args
    subst_runstring(args.input,args.input_name,unpack(args))
end
