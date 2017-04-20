* Problem in `refine_rows/1`

  `refine_rows/1` which was defined like this:

    refine_rows(M) ->
        A = lists:map(fun refine_row/1,M),
        B = par:parMap(fun refine_row/1,M),
        A = B,
        A.

  Which tries to do more work than intended and runs very, very slowly.
  For just 5 repititions (`EXECUTIONS = 5`) it takes 10 seconds to
  execute.

  If I redefine it to:

    refine_rows(M) ->
        par:parMap(fun refine_row/1,M).

  I get the exception:

    ** exception exit: no_solution

  What gives?

* Speculative paralellism

  As Anton Ekblad told us about at the lecture before easter
  we can use speculative parallelism when performing guesses
  on the sudoku.

  The hand-out mentions that we should have a go at parallelizing
  the solver but does not say we need to do it in multiple ways.
  If we solve the above problem we might not need to implement
  other approaches - but this is one viable method.

* Explain our results

  After we've implemented the approaches we wanna use we should
  document them in the README and explain how to reproduce them.

* Using `par:poolMap/3` in `refine_rows/1`

  We could use `par:poolMap/3` in place of `par:parMap/2` in
  `refine_rows/1`. However, if I do that, then I get this error:

    Error in process <0.23337.48> with exit value:
    {badarg,[{erlang,register,[<0.57.0>,<0.23337.48>],[]},
             {par,pool,2,[{file,"par.erl"},{line,64}]}]}
    ** exception exit: badarg
         in function  register/2
            called as register(<0.57.0>,<0.23337.48>)
         in call from par:pool/2 (par.erl, line 64)

  And again I have no idea what the problem is.

* Problem with `poolMap/3`

  If I run `par:poolMap(fun(X) -> X * 2 end, lists:seq(1,1000000), 3).`
  and look at my cpu usage (e.g. with `top`) it looks like only one
  core is being used for the computation.
