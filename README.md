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
