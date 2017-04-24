Intro
=====
Source-files are located in `src/`

Benchmark results
=================
Output of sequential benchmark:

    > sudoku:benchmarks().
    {52691981,
     [{wildcat,0.31281},
      {diabolical,43.789019999999994},
      {vegard_hanssen,93.14809},
      {challenge,6.6046000000000005},
      {challenge1,343.72214},
      {extreme,8.39593},
      {seventeen,30.94689}]}

Output of parallelized benchmark:

    > sudoku:pbenchmarks().
    {38856233,
     [{wildcat,2.29569},
      {diabolical,85.9101},
      {vegard_hanssen,139.75928},
      {challenge,10.92692},
      {challenge1,388.55411},
      {extreme,30.87589},
      {seventeen,58.32384}]}

We see that the overall time has gone down, but the time for solving each
individual puzzle has gone up possibly due to contention.

With pooling the results look like this:

    > sudoku:poolbenchmarks().
    {37801694,
     [{wildcat,1.76515},
      {diabolical,84.54417},
      {vegard_hanssen,135.17884},
      {challenge,21.3835},
      {challenge1,378.01133000000004},
      {extreme,30.67472},
      {seventeen,56.00633}]}

The results are not drastically different from the previous result. This is
likely due to the small size of the list. We would expect the improvement to
be more pronounced if we were solving - say - a thousand sudokus in parallel.

Parallelizing the solver
========================
* Problem with error propagation across ps.
* tried to use
  - parMap/2 in refine_rows/1 (data-parallelism)
  - a pipeline in refine/1 (pipeline parallelism)
    We decided against this because it increases the amount of data that
    gets send; and because the complexity of this function appears to be
    relatively small
  - speculative parallelism in solve_one
