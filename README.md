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

    2> sudoku:pbenchmarks().
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
