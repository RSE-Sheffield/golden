## R CMD check results v0.0.2

0 errors | 0 warnings | 2 notes

* This release improves documentation, and consistency of argument names.

The valgrind report ([from the Oxford memtests](https://www.stats.ox.ac.uk/pub/bdr/memtests/valgrind/golden/tests/testthat.Rout)) shows 15 "definitely lost" or "indirectly lost" blocks, all originating from paths that throw exceptions in Rcpp code.

These are known false positives caused by the interaction between C++ exceptions and R's longjmp-based error handling. The reported allocations (often the strings holding the exception's messages) are likely cleaned up by R's garbage collector, but Valgrind cannot see this cleanup due to the non-local jump skipping normal C++ destructors.

This pattern is considered harmless. No memory leaks were detected in the success paths or in non-exception-throwing tests.

## R CMD check results v0.0.1

0 errors | 0 warnings | 2 notes

* This is the first release.
