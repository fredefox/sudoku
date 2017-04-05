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
    {36855382,
    [{wildcat,1.98806},
      {diabolical,76.61157},
      {vegard_hanssen,132.22502},
      {challenge,11.24081},
      {challenge1,368.53671},
      {extreme,20.65632},
      {seventeen,54.95603}]}

We see that the overall time has gone down, but the time for solving each
individual puzzle has gone up due to contention.

In the last version we spawn the tasks in a different order:

    {37415740,
     [{wildcat,374.15009999999995},
      {diabolical,128.73331},
      {vegard_hanssen,80.27239999999999},
      {challenge,54.99237},
      {challenge1,21.5966},
      {extreme,11.02835},
      {seventeen,2.17739}]}
