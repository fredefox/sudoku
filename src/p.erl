-module(p).
-compile(export_all).

chan() ->
  R = make_ref(),
  S = self(),
  Reply = fun(X) -> S ! {R, X} end,
  Receive = fun() -> receive {R, X} -> X end end,
  {Reply, Receive}.

% Call a function in parallel, return a promise for that value.
spawnAsync(F) ->
  {Reply, Receive} = chan(),
  spawn_link(fun() -> Reply(F()) end),
  Receive.

parMap(F, Xs) ->
  As = [spawnAsync(fun() -> F(X) end) || X <- Xs],
  [A() || A <- As].

init() -> parMap(fun(_) -> rand:uniform(100) end, lists:seq(0,10)).
