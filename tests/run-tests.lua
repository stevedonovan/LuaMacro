-- similar syntax to tests.bat, but more portable and aware of errors.
require_ 'forall'
require_ 'qw'
def_ put io.stderr:write

function run (f)
  put(f,': ')
  if os.execute('luam '..f) ~= 0 then
    put 'failed!\n'
    os.exit(1)
  else
    put 'ok\n'
  end
end

forall f in qw(dollar,lambda,try,block,forall,scope,do,const,with,case,mod,test) do
  f = 'test-'..f..'.lua'
  run(f)
end

run '-lcskin test-cskin.lua'

