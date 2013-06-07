require_ 'atm'
@eval env = os.getenv
@if env 'P'
print 'gotcha'
@if A
print 'A was true'
@end
@else
print 'nada'
@end


