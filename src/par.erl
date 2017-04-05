-module(par).
-compile(export_all).

%%creates an async value only accessible by the current process
spawnAsync(F) ->
    R = make_ref(),
    S = self(),
    spawn_link(fun () ->
		       S ! {R, F()}
	       end),
    R.
awaitAsync(R) ->
    receive {R, X} ->
	    X
    end.

parMap(F,Xs) ->
    As = [spawnAsync(fun () -> F(X) end) || X <- Xs],
    [awaitAsync(A) || A <- As].

%%creates a single pipeline object
%% newtype Pipeline a = Pipeline [Pid]
%% pipeline :: [a->a] -> Pipeline a
%% usePipeline :: Pipeline a -> a -> a
%% killPipeline :: Pipeline a -> IO ()
pipeline([F]) ->
    P = spawn(fun () -> pipelineNil(F) end),
    [P];
pipeline([F|Fs]) ->
    [P|Ps] = pipeline(Fs),
    P2 = spawn(fun () -> pipelineCons(F,P) end),
    [P2,P|Ps].
pipelineNil(F) ->
    receive {Call,Input} ->
	    Output = F(Input),
	    return(Call,Output),
	    pipelineNil(F)
    end.
pipelineCons(F,Next) ->
    receive {Call,Input} ->
	    Output = F(Input),
	    Next ! {Call,Output},
	    pipelineCons(F,Next)
    end.
usePipeline([P|_]) ->
    fun (X) ->
	    P ! {Call = call(), X},
	    awaitCall(Call)
    end.
killPipeline(Ps) ->
    [exit(P,kill) || P <- Ps],
    ok.

%%Worker pool interface
%%pool :: IO ()
%%work :: (IO a,Pool) -> IO a
%%addWorker :: (Worker,Pool) -> IO ()
%%worker :: Pool -> IO ()
%%spawnWorker :: Pool -> IO ()
%%pool either does work for you and sends back result {ok,a} or (pool,'busy')
pool(NumWorkers,Name) ->
    Pool = self(),
    [spawn_link(fun () -> worker(Pool) end) || _ <- lists:seq(1,NumWorkers)], 
    true = register(Name,Pool),
    pool([]).
pool(Idle) ->
    receive {addWorker,Pid} ->
	    pool([Pid|Idle]);
	    {work, Call, F} ->
	    case Idle of
          [] ->
              return(Call, busy);
          [Worker|Idle2] ->
              Worker ! {Call, F},
              pool(Idle2)
	    end
    end.
work(F,Pool) ->
    Pool ! {work, Call = call(), F},
    case awaitCall(Call) of
        busy ->
            F();
        {ok,X} -> X
    end.
worker(Pool) ->
    Pool ! {addWorker, self()},
    receive {Call, F} ->
	    return(Call,{ok,F()}),
	    worker(Pool)
    end.

poolMap(F, Xs, N) ->
    Pool = self(),
    spawn_link(fun() -> pool(N, Pool) end),
    parMap(fun (X) -> work(fun() -> F(X) end, Pool) end, Xs).

call() ->
    {self(), make_ref()}.
return({Pid,Ref},X) ->
    Pid ! {Ref,X}.
awaitCall({_,Ref}) ->
    receive {Ref, X} ->
	    X
    end.
