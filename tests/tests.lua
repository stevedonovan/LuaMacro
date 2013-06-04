require_ 'macro.forall'
require_ 'qw'
local function execute (name)
   local file = 'test-'..name..'.lua'
   print('executing '..file)
   os.execute('luam '..file)
end
forall name in qw(dollar lambda try block scope do const rawhash include test) do
    execute (name)
end

if pcall(require,'pl') then
    execute 'list'
end

os.execute 'luam -lcskin test-cskin.lua'

