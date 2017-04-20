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
