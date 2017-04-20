-module(par).
-compile(export_all).

%%creates an async value only accessible by the current process
spawnAsync(F) ->
    {Send,Receive} = chan(),
    spawn_link(fun () -> sendChan(Send,F()) end),
    Receive.

%%removed awaitAsync: use receiveChan instead
awaitAsync(_) ->
    io:format("ERROR: use receiveChan/1 instead!"),
    exit(self(),kill).

parMap(F,Xs) ->
    As = [spawnAsync(fun () -> F(X) end) || X <- Xs],
    [receiveChan(A) || A <- As].

myChan() ->
  R = make_ref(),
  S = self(),
  Reply = fun(F) -> S ! {R, F()} end,
  Receive = fun() -> receive {R, X} -> X end end,
  {Reply, Receive}.

mySpawnAsync(F) ->
  {Reply, Receive} = myChan(),
  spawn_link(fun() -> Reply(F) end),
  Receive.

myParMap(F, Xs) ->
  As = [mySpawnAsync(fun() -> F(X) end) || X <- Xs],
  [A() || A <- As].

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
	    sendChan(Call,Output),
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
	    {S,R} = chan(),
	    P ! {S,X},
	    receiveChan(R)
    end.
killPipeline(Ps) ->
    [exit(P,kill) || P <- Ps],
    ok.

%%This pool is inefficient: tasks get sent via pool and you can't reserve
%%a worker
newPool(NumWorkers) ->
    spawn(fun () ->
		  Pool = self(),
		  [spawn_link(fun () -> worker(Pool) end)
		   || _ <- lists:seq(1,NumWorkers)],
		  pool([])
	  end).
%%change: let customer know immediately that you're working on it
pool(Idle) ->
    receive {addWorker,Pid} ->
	    pool([Pid|Idle]);
	    {work, Send, F} ->
	    case Idle of
		[] ->
		    sendChan(Send,busy);
		[Worker|Idle2] ->
		    Worker ! {Send, F},
		    sendChan(Send,working),
		    pool(Idle2)
	    end
    end.

worker(Pool) ->
    Pool ! {addWorker, self()},
    receive {Send, F} ->
	    sendChan(Send,{done,F()}),
	    worker(Pool)
    end.

%%Also functions as a metaphor for capitalism
poolMap(F,Xs,N) ->
    S = self(),
    R = make_ref(),
    [spawn_link(fun() -> poolMapWorker(F,S,R) end) || _ <- lists:seq(1,N)], 
    LenXs = employWorkers(F,Xs,R,_Index = 0),
    killTheUnemployed(N,R),
    collectWork(R,LenXs).
employWorkers(_,[],_R,Index) ->
    Index;
employWorkers(F,[X|Xs],R,Index) ->
    receive {job_application,R,Worker} ->
	    Worker ! {R,Index,X}
    after 0 -> %%Dang, guess I'll have to do some work myself!
	    self() ! {R,Index,F(X)}
    end,
    employWorkers(F,Xs,R,Index+1).
killTheUnemployed(0,_) ->
    ok;
killTheUnemployed(N,R) ->
    receive {job_application,R,Worker} ->
	    Worker ! die,
	    killTheUnemployed(N-1,R)
    end.
collectWork(R,LenXs) ->
    [receive {R,Index,FX} -> FX end || Index <- lists:seq(0,LenXs-1)].
poolMapWorker(F,Employer,R) ->
    Employer ! {job_application,R,self()},
    receive {R,Index,X} ->
	    Employer ! {R,Index,F(X)},
	    poolMapWorker(F,Employer,R);
	    die -> ok
    end.

chan() ->
    S = self(),
    R = make_ref(),
    {_Send = {S,R}, _Receive = R}.
sendChan({Pid,Ref},X) ->
    Pid ! {Ref,X}.
receiveChan(Ref) ->
    receive {Ref, X} ->
	    X
    end.

chanF() ->
    {Send,Receive} = chan(),
    {fun (X) -> sendChan(Send,X) end,
     fun () -> receiveChan(Receive) end}.

%%while I'm at it
takeMVar(MVar) ->
    MVar ! {take,R = make_ref(), self()},
    receive {R,X} -> X end.
putMVar(MVar,X) ->
    MVar ! {put,R = make_ref(), self(), X},
    receive R -> ok end.
mvar(nothing) ->
    receive {put,R,Pid,X} ->
	    Pid ! R,
	    mvar({just,X})
    end;
mvar({just,X}) ->
    receive {take,R,Pid} ->
	    Pid ! {R,X},
	    mvar(nothing)
    end.
