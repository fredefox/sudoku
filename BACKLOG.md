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
