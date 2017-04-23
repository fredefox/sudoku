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
  spawn_link(fun() -> Reply(catch({ok, F()})) end),
  Receive.

parMap(F, Xs) ->
  As = [spawnAsync(fun() -> F(X) end) || X <- Xs],
  [case R() of
    {ok, A} -> A;
    {'EXIT', E} -> exit(E);
    E -> exit(E)
  end || R <- As].

split_at_most(N, L) when is_integer(N), N >= 0, is_list(L) ->
    split_at_most(N, L, []).

split_at_most(N, [H|T], PR) when N > 0 ->
  split_at_most(N-1, T, [H|PR]);
split_at_most(_, S, PR) ->
  {lists:reverse(PR), S}.

chunksOf(_, []) -> [];
chunksOf(N, Xs) ->
  {A, B} = split_at_most(N, Xs),
  Bs = chunksOf(N, B),
  [A | Bs].

granularParMap(F, Xs, N) ->
  Cs = chunksOf(N, Xs),
  Xss = parMap(fun(A) -> lists:map(F, A) end, Cs),
  lists:concat(Xss).
