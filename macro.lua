----------------------------------------------
-- LuaMacro 2, a macro-preprocessor for Lua.
-- Unlike LuaMacro 1.x, it does not depend on the token-filter patch and generates
-- Lua code which can be printed out or compiled directly. C-style macros are easy, but LM2
-- allows for macros that can read their own input and generate output using Lua code.
-- New in this release are lexically-scoped macros.
-- The Lua Lpeg Lexer is by Peter Odding.
-- Steve Donovan, 2011
-- @module macro
-- @alias M

local macro = {}
local M = macro
local lexer = require 'macro.lexer'
local scan_code = lexer.scan_lua
local append = table.insert
local setmetatable = setmetatable


local TokenList = {}
TokenList.__index = TokenList

local function TL (tl)
    return setmetatable(tl or {},TokenList)
end

local TokenListList = {}

local function LTL (ltl)
    return setmetatable(ltl or {},TokenListList)
end

TokenListList.__index = function(self,key)
    local m = TokenList[key]
    return function(self,...)
        local res = {}
        for i = 1,#self do res[i] = m(self[i],...) end
        return LTL(res)
    end
end

-- token-getting helpers

--- get a delimited list of token lists.
-- Typically used for grabbing argument lists like ('hello',a+1,fred(c,d)); will count parens
-- so that the delimiter (usually a comma) is ignored inside sub-expressions. You must have
-- already read the start token of the list, e.g. open parentheses. It will eat the end token
-- and return the list of TLs, plus the end token. Based on similar code in Penlight's
-- `pl.lexer` module.
-- @param tok the token stream
-- @param endt the end token (default ')')
-- @param delim the delimiter (default ',')
-- @return list of token lists
-- @return end token in form {type,value}
function M.get_list(tok,endtoken,delim)
    endtoken = endtoken or ')'
    delim = delim or ','
    local parm_values = LTL()
    local level = 1 -- used to count ( and )
    local tl = TL()
    local function tappend (tl,t,val)
        val = val or t
        append(tl,{t,val})
    end
    local is_end
    if type(endtoken) == 'function' then
        is_end = endtoken
    elseif endtoken == '\n' then
        is_end = function(t,val)
            return t == 'space' and val:find '\n'
        end
    else
        is_end = function (t)
            return t == endtoken
        end
    end
    local token,value = tok()
    if is_end(token,value) then return parm_values end
    if token == 'space' then
        token,value = tok()
    end
    while true do
        if not token then return nil,'unexpected end of list' end -- end of stream is an error!
        if is_end(token,value) and level == 1 then
            append(parm_values,tl)
            break
        elseif token == '(' then
            level = level + 1
            tappend(tl,'(')
        elseif token == ')' then
            level = level - 1
            if level == 0 then -- finished with parm list
                append(parm_values,tl)
                break
            else
                tappend(tl,')')
            end
        elseif token == '{' then
            level = level + 1
            tappend(tl,'{')
        elseif token == '}' then
            level = level - 1
            tappend(tl,'}')
        elseif token == delim and level == 1 then
            append(parm_values,tl) -- a new parm
            tl = TL()
        else
            tappend(tl,token,value)
        end
        token,value=tok()
    end
    return parm_values,{token,value}
end

function M.upto_keywords (k1,k2)
    return function(t,v)
        return t == 'keyword' and (v == k1 or v == k2)
    end,''
end

-- create a token iterator out of a token list
local function scan_iter (tlist)
    local i,n = 1,#tlist
    return function()
        local tv = tlist[i]
        if tv == nil then return nil end
        i = i + 1
        return tv[1],tv[2]
    end
end

--- Getter class
-- @type Getter

local Getter = {
    __call = function(self)
        return self.fun()
    end
}
Getter.__index = Getter;

local function make_getter (get)
    return setmetatable({fun=get},Getter)
end

function M.Getter(tl)
    return make_getter(scan_iter(tl))
end

Getter.list = M.get_list

function Getter.upto(tok,k1,k2)
    local endt = k1
    if type(k1) == 'string' and k1:match '^%a+$' then
        endt = M.upto_keywords(k1,k2)
    end
    local ltl,tok = tok:list(endt,'')
    M.assert(ltl ~= nil and #ltl > 0,'failed to grab tokens')
    return ltl[1],tok
end

local function tnext(get)
    local t,v = get()
    while t == 'space' or t == 'comment' do
        t,v = get()
    end
    return t,v
end

Getter.next = tnext

function Getter.name(tok)
    local t,v = tnext(tok)
    M.assert(t == 'iden','expecting name')
    return v
end

function Getter.number(tok)
    local t,v = tnext(tok)
    M.assert(t == 'number','expecting number')
    return tonumber(v)
end

--- get a delimited list of names.
-- works like get_list.
-- @param tok the token stream
-- @param endt the end token (default ')')
-- @param delim the delimiter (default ',')
-- @see get_list
function Getter.names(tok,endt,delim)
    local ltl,err = tok:list(endt,delim)
    if not ltl then error('get_names: '..err) end
    local names = {}
    -- get_list() will return {{}} for an empty list of tlists
    for i,tl in ipairs(ltl) do
        local tv = tl[1]
        if tv then names[i] = tv[2] end
    end
    return names
end

--- get the next string from the token stream.
-- Will skip space.
function Getter.string(tok)
    local t,v = tok:expecting("string")
    return v:sub(2,-2)
end

--- assert that the next token has the given type.
-- @param type a token type ('iden','string',etc)
function Getter.expecting (tok,type,value)
    local t,v = tnext(tok)
    if t ~= type then M.error ("expected "..type.." got "..t) end
    if value then
        if v ~= value then M.error("expected "..value.." got "..v) end
    end
    return t,v
end


local function extract (tl)
    local tk = tl[1]
    if tk[1] == 'space' then
        tk = tl[2]
    end
    return tk
end

function TokenList.get_iden (tl)
    local tk = extract(tl)
    M.assert(tk[1]=='iden','expecting identifier')
    return tk[2]
end

function TokenList.get_number(tl)
    local tk = extract(tl)
    M.assert(tk[1]=='number','expecting number')
    return tonumber(tk[2])
end

function TokenList.get_string(tl)
    local tk = extract(tl)
    M.assert(tk[1]=='string')
    return tk[2]:sub(2,-2) -- watch out! what about long string literals??
end

--- takes a token list and strips spaces and comments.
function TokenList.strip_spaces (tl)
    local out = TL()
    for _,t in ipairs(tl) do
        if t[1] ~= 'comment' and t[1] ~= 'space' then
            append(out,t)
        end
    end
    return out
end

function TokenList:pick (n)
    local t = self[n]
    return t[2],t[1]
end


-- token-putting helpers
local comma,space = {',',','},{'space',' '}

function M.put_name(res,name,no_space)
    append(res,{'iden',name})
    if not no_space then
        append(res,space)
    end
    return res
end

function M.put_string(res,name)
    append(res,{'string','"'..name..'"'})
    return res
end

function M.put_number(res,val)
    append(res,{'number',val})
    return res
end

--- put out a list of names, separated by commas.
-- @param res output token list
-- @param names a list of strings
function M.put_names(res,names)
    for i = 1,#names do
        M.put_name(res,names[i],true)
        if i ~= #names then append(res,comma) end
    end
    return res
end

--- put out a token list.
-- @param res output token list
-- @param names a token list
function M.put_tokens(res,tl)
    for j = 1,#tl do
        append(res,tl[j])
    end
    return res
end

function TokenList.__tostring(tl)
    local res = {}
    for j = 1,#tl do
        append(res,tl[j][2])
    end
    return table.concat(res)
end

--- put out a list of token lists, separated by commas.
-- @param res output token list
-- @param names a list of strings
function M.put_list(res,ltl)
    for i = 1,#ltl do
        M.put_tokens(res,ltl[i])
        if i ~= #ltl then append(res,comma) end
    end
    return res
end

--- put out a space token.
-- @param res output token list
-- @param space a string containing only whitespace (default ' ')
function M.put_space(res,space)
    append(res,{'space',space or ' '})
    return res
end

--- put out a keyword token.
-- @param res output token list
-- @param keyw a Lua keyword
function M.put_keyword(res,keyw)
    append(res,{'keyword',keyw})
    append(res,space)
    return res
end

--- put out a operator token.
-- @param res output token list
-- @param keyw an operator string
function M.put(res,t,v)
    append(res,{t,v or t})
    return res
end

TokenList.__call = function(obj,...)
    return M.put(obj,...)
end
TokenList.keyword = M.put_keyword
TokenList.space = M.put_space
TokenList.list = M.put_list
TokenList.names = M.put_names
TokenList.tokens = M.put_tokens
TokenList.name = M.put_name
TokenList.string = M.put_string
TokenList.number = M.put_number

local make_putter = TL

M.Putter = make_putter

-- given a token list, a set of formal arguments and the actual arguments,
-- return a new token list where the formal arguments have been replaced
-- by the actual arguments
local function substitute_tokenlist (tl,parms,args)
    local append,put_tokens = table.insert,M.put_tokens
    local parm_map = {}
    for i,name in ipairs(parms) do
        parm_map[name] = args[i]
    end
    local res = {}
    for _,tv in ipairs(tl) do
        local t,v = tv[1],tv[2]
        if t == 'iden' then
            local pval = parm_map[v]
            if pval then
                put_tokens(res,pval)
            else
                append(res,tv)
            end
        else
            append(res,tv)
        end
    end
    return res
end

function M.copy_tokens(tok,pred)
    local res = {}
    local t,v = tok()
    while t and not (pred and pred(t,v)) do
        append(res,{t,v})
        t,v = tok()
    end
    return res
end

function M.define_tokens(extra)
    lexer.add_extra_tokens(extra)
end

local imacros,smacros = {},{}

M.macro_table = imacros

--- define a macro using a specification string and optional function.
-- The specification looks very much like a C preprocessor macro: the name,
-- followed by an optional formal argument list (_no_ space after name!) and
-- the substitution. e.g 'answer 42' or 'sqr(x) ((x)*(x))'
--
-- If there is no substitution, then the second argument must be a function which
-- will be evaluated for the actual substitution.
-- @param macstr
-- @param subst_fn the optional substitution function
function M.define(macstr,subst_fn)
    local tok,t,macname,parms,parm_map
    local mtbl
    tok = scan_code(macstr)
    t,macname = tok()
    if t == 'iden' then mtbl = imacros
    elseif t ~= 'string' and t ~= 'number' and t ~= 'keyword' then
        mtbl = smacros
    else
        error("a macro cannot be of type "..t)
    end
    t = tok()
    if t == '(' then
        parms = make_getter(tok):names()
    end
    mtbl[macname] = {
        name = macname,
        subst = subst_fn or M.copy_tokens(tok),
        parms = parms
    }
end

--- define a macro using a function and a parameter list.
-- @param name either an identifier or an operator.
-- @param subst a function
-- @param parms a list of parameter names
-- @return the existing value of this macro, if any
function M.set_macro(name,subst,parms)
    local macros
    if name:match '^[_%a][_%w]*$' then
        macros = imacros
    else
        macros = smacros
    end
    if subst == nil then
        macros[name] = nil
        return
    end
    local last = macros[name]
    if type(subst) ~= 'table' or not subst.name then
        subst = {
            name = name,
            subst = subst,
            parms = parms
        }
    end
    macros[name] = subst
    return last
end

--- defined a scoped macro. Like define except this macro will not
-- be visible outside the current scope.
-- @param name either an identifier or an operator.
-- @param subst a function
-- @param parms a list of parameter names
-- @see set_macro
function M.define_scoped (name,subst,parms)
    local old_value = M.set_macro(name,subst,parms)
    M.block_handler(-1,function()
        M.set_macro(name,old_value)
    end)
end

--- get the value of a macro. The macro substitution must either be a
-- a string or a single token.
-- @param name existing macro name
-- @return a string value, or nil if the macro does not exist.
function M.get_macro_value(name)
    local mac = imacros[name]
    if not mac then return nil end
    if type(mac.subst) == 'table' then
        return mac.subst[1][2]
    else
        return mac.subst
    end
end

local function get_macro (mac, no_error)
    local macro = imacros[mac]
    if not macro and not no_error then
        M.error("macro "..mac.." is not defined")
    end
    return macro
end

local push,pop = table.insert,table.remove

function M.push_macro_stack (name,value)
    local macro = get_macro(name)
    macro.stack = macro.stack or {}
    push(macro.stack,value)
end

function M.pop_macro_stack (name)
    local macro = get_macro(name)
    if macro.stack and #macro.stack > 0 then
        return pop(macro.stack)
    end
end

function M.value_of_macro_stack (name)
    local macro = get_macro(name,true)
    if not macro then return nil end
    if macro.stack and #macro.stack > 0 then
        return macro.stack[#macro.stack]
    end
end

local lua_keywords = {
    ['do'] = 'open', ['then'] = 'open', ['else'] = 'open', ['function'] = 'open',
    ['repeat'] = 'open';
    ['end'] = 'close', ['until'] = 'close',['elseif'] = 'close'
}

local c_keywords = {}
local keywords = lua_keywords

local block_handlers,keyword_handlers = {},{}
local level = 1

--- specify a block handler at a given level.
function M.block_handler (lev,action)
    lev = lev + level
    if not block_handlers[lev] then
        block_handlers[lev] = {}
    end
    append(block_handlers[lev],action)
end

local function process_block_handlers(level,get,v)
    local persist,result
    -- a block handler may indicate with an extra true return
    -- that it wants to persist; the keyword is passed to them
    -- so we can get more specific end of block handlers.
    for _,bh in pairs(block_handlers[level]) do
        local res,keep = bh(get,v)
        if not keep then
            if res then result = res end
        else
            persist = persist or {}
            append(persist,bh)
        end
    end
    block_handlers[level] = persist
    return result
end


--- set a keyword handler. Unlike macros, the keyword itself is always
-- passed through, but the handler may add some output afterwards.
-- If the action is nil, then the handler for that keyword is removed.
-- @param word keyword
-- @param action function to be called when keyword is encountered
-- @return previous handler associated with this keyword
function M.keyword_handler (word,action)
    if word == 'BEGIN' or word == 'END' then
        keyword_handlers[word] = action
        return
    end
    if action then
        local last = keyword_handlers[word]
        keyword_handlers[word] = action
        return last
    else
        keyword_handlers[word] = nil
    end
end

--- set a scoped keyword handler. Like keyword_handler, except
-- it restores the original keyword handler (if any) at the end
-- of the current block.
-- @param word keyword
-- @param action to be called when keyword is encountered
-- @see keyword_handler
function M.scoped_keyword_handler (keyword, action)
    local last = M.keyword_handler(keyword,action)
    M.block_handler(-1,function()
        M.keyword_handler(keyword,last)
    end)
end

-- a convenient way to use keyword handlers. This sets a handler and restores
-- the old handler at the end of the current block.
function M.make_scoped_handler(keyword,handler)
    return function() M.scoped_keyword_handler(keyword, action) end
end

M.please_throw = false

function M.error(msg)
    M.please_throw = true
    msg = M.filename..':'..lexer.line..' '..msg
    if M.please_throw then
        error(msg,2)
    else
        io.stderr:write(msg,'\n')
        os.exit(1)
    end
end

M.define ('debug_',function()
    M.DEBUG = true
end)

function M.assert(expr,msg)
    if not expr then M.error(msg or 'internal error') end
end

local line_updater, line_table

local function lua_line_updater (iline,oline)
    if not line_table then line_table = {} end
    append(line_table,{il=iline,ol=oline})
end

local function c_line_updater (iline,oline,last_t,last_v)
    local endt = last_t == 'space' and last_v or '\n'
    return '#line '..iline..' "'..M.filename..'"'..endt
end


--- Do a macro substitution on Lua source.
-- @param src Lua source (either string or file-like reader)
-- @param out output (a file-like writer)
-- @param name input file name
-- @param use_c nil for Lua; if 'line', then output #line directives; if true, then don't
-- @return the result as table of strings
-- @return line number information
function M.substitute(src,name, use_c)
    local out, ii = {}, 1
    if use_c then
        lexer = require 'macro.clexer'
        scan_code = lexer.scan_c
        keywords = c_keywords
        if use_c == 'line' then
            line_updater = c_line_updater
        else
            line_updater = function() end
        end
    else
        lexer = require 'macro.lexer'
        scan_code = lexer.scan_lua
        keywords = lua_keywords
        line_updater = lua_line_updater
    end
    local tok,tokn = scan_code(src,name)
    local iline,iline_changed = 0
    local last_t,last_v = 'space','\n'
    local do_action

    M.filename = name or '(tmp)'

    local t,v = tok()

    -- this function get() is always used, so that we can handle end-of-stream properly.
    -- The substitution mechanism pushes a new stream on the tstack, which is popped
    -- when empty.
    local tstack = {}
    local push,pop = table.insert,table.remove

    local function get ()
        last_t,last_v = t,v
        local t,v = tok()
        while not t do
            tok = pop(tstack)
            if tok == nil then
                if keyword_handlers.END then
                    do_action(keyword_handlers.END)
                    keyword_handlers.END = nil
                else
                    return nil
                end
            end -- finally finished
            t,v = tok()
        end
        if name == lexer.name and iline ~= lexer.line  then
            iline = lexer.line -- input line has changed
            iline_changed = last_v
        end
        return t,v
    end

    local getter = make_getter(get)

    --- peek ahead or before in the token stream.
    -- @param k positive delta for looking ahead, negative for looking behind.
    -- @param dont_skip true if you want to check for whitespace
    -- @return the token type
    -- @return the token value
    -- @return the token offset
    -- @member Getter
    function getter:peek (k,dont_skip)
        k = k - 1
        local tok = tokn(k)
        local t,v = tok[1], tok[2]
        if not dont_skip then
            local skip = k < 0 and -1 or 1
            while t == 'space' do
                k = k + skip
                tok = tokn(k)
                t,v = tok[1], tok[2]
            end
        end
        return t,v,k+1
    end

    --- peek ahead two tokens.
    -- @return first token type
    -- @return first token value
    -- @return second token type
    -- @return second token value
    -- @member Getter
    function getter:peek2 ()
        local t1,v1,k1 = self:peek(1)
        local t2,v2 = self:peek(k1+1)
        return t1,v1,t2,v2
    end

    --- patch the token stream at the end.
    -- @param idx index in output table
    -- @param text to replace value at that index
    -- @member Getter
    function getter:patch (idx,text)
        out[idx] = text
    end

    --- put out a placeholder for later patching.
    -- @param put a putter object
    -- @return an index into the output table
    -- @member Getter
    function getter:placeholder (put)
        put:name '/MARK?/'
        return ii
    end

    -- this feeds the results of a substitution into the token stream.
    -- substitutions may be token lists, Lua strings or nil, in which case
    -- the substitution is ignored. The result is to push a new token stream
    -- onto the tstack, so it can be fetched using get() above
    local function push_substitution (subst)
        if subst == nil then return end
        local st = type(subst)
        push(tstack,tok)
        if st == 'table' then
            subst = scan_iter(subst)
        elseif st == 'string' then
            subst = scan_code(subst)
        end
        tok = subst
    end
    M.push_substitution = push_substitution

    -- a macro object consists of a subst object and (optional) parameters.
    -- If there are parms, then a macro argument list must follow.
    -- The subst object is either a token list or a function; if a token list we
    -- substitute the actual parameters for the formal parameters; if a function
    -- then we call it with the actual parameters.
    -- Without parameters, it may be a simple substitution (TL or Lua string) or
    -- may be a function. In the latter case we call it passing the token getter,
    -- assuming that it will grab anything it needs from the token stream.
    local function expand_macro(get,mac)
        local pass_through
        local subst = mac.subst
        local fun = type(subst)=='function'
        if mac.parms then
            t = tnext(get);
            if t ~= '(' then
                M.error('macro '..mac.name..' expects parameters')
            end
            local args,err = M.get_list(get)
            M.assert(args,'no end of argument list')
            if fun then
                subst = subst(unpack(args))
            else
                if #mac.parms ~= #args then
                    M.error(mac.name.." takes "..#mac.parms.." arguments")
                end
                subst = substitute_tokenlist(subst,mac.parms,args)
            end
        elseif fun then
            subst,pass_through = subst(getter,make_putter())
        end
        push_substitution(subst)
        return pass_through
    end


    local multiline_tokens,sync = lexer.multiline_tokens,lexer.sync
    local line,last_diff = 0,0



    function do_action (action)
        push_substitution(action(getter,make_putter()))
    end

    if keyword_handlers.BEGIN then
        do_action(keyword_handlers.BEGIN)
    end

    while t do
        local dump = true
        if t == 'iden' then -- classic name macro
            local mac = imacros[v]
            if mac then
                dump = expand_macro(get,mac)
            end
        elseif t == 'keyword' then
            -- important to track block level for lexical scoping and block handlers
            local class = keywords[v]
            if class == 'open' then
                if v ~= 'else' then level = level + 1 end
            elseif class == 'close' then
                level = level - 1
                if block_handlers[level] then
                    local res = process_block_handlers(level,get,v)
                    if res then push_substitution(res) end
                end
            --* elseif class == 'hook' then
            end
            local action = keyword_handlers[v]
            if action then do_action(action) end
        else -- any unused 'operator' token (like @, \, #) can be used as a macro
            if use_c then
                if v == '{' then
                    level = level + 1
                elseif v == '}' then
                    level = level - 1
                    if block_handlers[level] then
                        local res = process_block_handlers(level,get,v)
                        if res then push_substitution(res) end
                    end
                end
            end
            local mac = smacros[v]
            if mac then
                dump = expand_macro(get,mac)
            end
        end
        if dump then
            if multiline_tokens[t] then -- track output line
                line = sync(line, v)
            end
            if iline_changed then
                local diff = line - iline
                if diff ~= last_diff then
                    local ldir = line_updater(iline,line,last_t,last_v)
                    if ldir then out[ii] = ldir; ii=ii+1 end
                    last_diff = diff
                end
                iline_changed = nil
            end
            out[ii] = v
            ii = ii + 1
        end
        t,v = get()
    end

    return out,line_table
end

--- take some Lua source and return the result of the substitution.
-- Does not raise any errors.
-- @param src either a string or a readable file object
-- @param name optional name for the chunk
-- @return the result or nil
-- @return the error, if error
function M.substitute_tostring(src,name,use_c)
    M.please_throw = true
    local ok,out,li = pcall(M.substitute,src,name,use_c)
    if type(src) ~= 'string' and src.close then src:close() end
    if not ok then return nil, out
    else
        return table.concat(out), li
    end
end

local old_loadin = loadin
local loadin

if not old_loadin then -- Lua 5.1
    function loadin (env,src,name)
        local chunk,err = loadstring(src,name)
        if chunk and env then
            setfenv(chunk,env)
        end
        return chunk,err
    end
else -- Lua 5.2
    function loadin(env,src,name)
        local chunk,err
        if env then
            chunk,err = old_loadin(env,src,name)
        else
            chunk,err = load(src,name)
        end
        return chunk,err
    end
end

--- load Lua code in a given envrionment after passing
-- through the macro preprocessor.
-- @param env the environment (may be nil)
-- @param src either a string or a readable file object
-- @param name optional name for the chunk
-- @return the cnunk, or nil
-- @return the error, if no chunk
function M.loadin(env,src,name)
    local res,err = M.substitute_tostring(src)
    if not res then return nil,err end
    return loadin(env,res,name)
end

--- evaluate Lua macro code in a given environment.
-- @param src either a string or a readable file object
-- @param env the environment (can be nil)
-- @return true if succeeded
-- @return result(s)
function M.eval(src,env)
    local chunk,err = M.loadin(env,src,'(tmp)')
    if not chunk then return nil,err end
    return pcall(chunk)
end

function M.set_package_loader(ext)
    ext = ext or 'm.lua'
    -- directly inspired by https://github.com/bartbes/Meta/blob/master/meta.lua#L32,
    -- after a suggestion by Alexander Gladysh
    table.insert(package.loaders, function(name)
        local lname = name:gsub("%.", "/") .. '.'..ext
        local f,err = io.open(lname)
        if not f then return nil,err end
        return M.loadin(nil,f,lname)
    end)
end

return macro
