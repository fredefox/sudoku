-module(sudoku).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).
-import(par, [poolMap/3]).
-import(p, [parMap/2]).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
        refine_rows(
          transpose(
            refine_rows(
              transpose(
                unblocks(
                  refine_rows(
                    blocks(M)
                   )
                 )
               )
             )
           )
         ),
    %% F = par:usePipeline(par:pipeline(
    %%   [ fun blocks/1
    %%   , fun refine_rows/1
    %%   , fun unblocks/1
    %%   , fun transpose/1
    %%   , fun refine_rows/1
    %%   , fun transpose/1
    %%   , fun refine_rows/1
    %%   ])),
    %% NewM = F(M),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

f() ->
  A = fun(X) -> X + 2 end,
  B = fun(X) -> X * 2 end,
  Pipe = par:usePipeline(par:pipeline([A, B])),
  Pipe(4).

refine_rows(M) ->
    % poolMap(fun refine_row/1, M, Workers).
    lists:map(fun refine_row/1, M).
    % p:parMap(fun refine_row/1, M).
    % p:granularParMap(fun refine_row/1, M, 2).

poolMap(F, Xs) ->
    Cores = 4, Workers = Cores - 1,
    par:poolMap(F, Xs, Workers).

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] -> exit(no_solution);
		     [Y] -> Y;
		     NewX -> NewX
		 end;
	   true -> X
	 end
	 || X <- Row],
    NewEntries = entries(NewRow),
    %% check we didn't create a duplicate entry
    case length(lists:usort(NewEntries)) == length(NewEntries) of
	true ->
	    NewRow;
	false ->
	    exit(no_solution)
    end.

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

-define(NUM_WORKERS,4).

solve(Sud) -> 
  SudokuPuzzle = refine(fill(Sud)),
  H = heaps:from_list([{hard(SudokuPuzzle), SudokuPuzzle}]),
  S = self(),
  Pids = [spawn_link(fun() -> worker(S) end) || _ <- lists:seq(1, ?NUM_WORKERS)],
  format("starting workers~n"),
  Result = heap(H,?NUM_WORKERS),
  format("got: ~p~n", [Result]),
  format("killing workers~n"),
  % process_flag(trap_exit,true),
  [exit(P, 'normal') || P <- Pids],
  Result.

dbg(_S) -> ok. % io:format("[~p]: ~p~n", [self(), S]).
format(_A) -> ok.
format(_A, _B) -> ok.

worker(S) ->
  S ! {pop, self()},
  receive
    Puzzle ->
      dbg("solving puzzle"),
      case solved(Puzzle) of
        true  -> dbg("was solved"), S ! {solved, Puzzle};
        false -> dbg("wasnt solved"),
                 Puzzles = [{hard(P), P} || P <- guesses(Puzzle)],
                 S ! {merge, {prioq,_,_} = heaps:from_list(Puzzles)},
                 worker(S)
      end
  end.

heap(H,T) ->
 format("heap size: ~p~n",[heaps:size(H)]),
 receive {merge,H2} -> dbg("heap merge"), heap(heaps:merge(H, H2),T);
         {pop,Worker} ->
                format("heap pop: ~p~n", [Worker]),
		case pop(H) of
                  {{_Diff, SP}, H2} -> 
                    Worker ! SP,
                    case heaps:empty(H2) of
                      true -> heap2([],T);
                      false -> heap(H2,T)
                    end;
                  empty -> heap2([Worker],T)
                end;
         {solved,Sol} -> dbg("heap solved"), dbg("returning from heap"), {ok, Sol}
 end.

pop(H) -> case heaps:empty(H) of
    true ->  empty;
    false -> {heaps:min(H), heaps:delete_min(H)}
  end.

heap2(Ws,T) ->
 format("workers: ~p~n",[Ws]),
 receive {merge,H} -> format("heap2 merge~n"),
                      {Elems,H2} = pops(length(Ws),H),
                      Ws2 = send_puzzles(Elems, Ws),
                      case Ws2 of
                           [] -> heap(H2,T);
                           _  -> heap2(Ws2,T)
                      end;
         {pop,Worker} -> format("heap2 pop: ~p~n", [Worker]),
                         Workers = [Worker|Ws],
                         case length(Workers) == T of
                           true ->  no_solution;
                           false -> heap2(Workers,T)
                         end;
         {solved,Sol} -> format("returning from heap2~n"), {ok, Sol}
 end.

% for Elem in Elems send to Ws[0], Ws[1] etc, Ws2 is remainder
send_puzzles([], Workers) -> Workers;
send_puzzles([{_Diff, Puzzle}|Puzzles],[Worker|Workers]) ->
  Worker ! Puzzle,
  send_puzzles(Puzzles,Workers).

% Try to pop length Ws Elems from H, H2 is remainder:
%     pops N Heap:
%     while N-- && Heap not empty: output Heap.pop()
%     done
pops(0,Heap) ->
   {[], Heap};
pops(N,Heap) ->
  case pop(Heap) of
     {E,Heap2} -> {Es,Heap3} = pops(N-1,Heap2),
                         {[E|Es],Heap3};
     empty -> {[], Heap}
  end.

%% benchmarks

-define(EXECUTIONS,100).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

bench(Fun) ->
    {ok,Puzzles} = file:consult("problems.txt"),
    timer:tc(?MODULE,Fun,[Puzzles]).

benchmarks() ->
    bench(benchmarks).

%% We assume that the task of solving a sudoku already has a fairly
%% decent granularity.
pbenchmarks(Puzzles) ->
    par:parMap(fun({Name, M}) -> {Name, bm(fun() -> solve(M) end)} end, Puzzles).

pbenchmarks() -> bench(pbenchmarks).

poolbenchmarks(Puzzles) ->
    Cores   = 4,
    Workers = Cores - 1,
    par:poolMap(fun({Name, M}) -> {Name, bm(fun() -> solve(M) end)} end, Puzzles, Workers).

poolbenchmarks() -> bench(poolbenchmarks).

%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).
