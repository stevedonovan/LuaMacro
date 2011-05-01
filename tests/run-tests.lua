-- similar syntax to tests.bat, but more portable and aware of errors.
require_ 'macro.forall'
require_ 'qw'
def_ put io.stderr:write

forall f in qw(dollar,lambda,try,block,forall,scope,do,const) do
  f = 'test-'..f..'.lua'
  put(f,': ')
  if os.execute('luam '..f) ~= 0 then
    put 'failed!\n'
    os.exit(1)
  else
    put 'ok\n'
  end
end
