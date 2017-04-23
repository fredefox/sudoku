-module(test).
-compile(export_all).
-import(par,[parMap/2]).

%%fib :: Nat -> Nat
fib(0)->
    1;
fib(1) ->
    1;
fib(N) when is_number(N) ->
    fib(N-1) + fib(N-2).

prop_map(Nss) ->
    A = parMap(fun (Ns) ->
		   parMap(fun fib/1,Ns) 
	   end, Nss),
    B = [[fib(N)||N<-Ns]||Ns<-Nss],
    A == B.
test() ->
    prop_map([[rand:uniform(40) || _ <- lists:seq(1,10)] || _ <- lists:seq(1,5)]).
