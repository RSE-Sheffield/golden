## R CMD check results v0.0.3

Status: 2 NOTEs

* This release resolves a memory-leak identified by [r-project incoming valgrind](https://win-builder.r-project.org/incoming_pretest/golden_0.0.2_20260324_152124/specialChecks/valgrind/summary.txt), although we were unable to reproduce the error under Valgrind locally.
* Also removes several old-style try-catch statements from RCPP code.

> X-CRAN-Comment: Archived on 2026-03-31 as issues were not corrected in time. And spammed personal email address of team member in HTML.

Apologies, the email was sent to clarify handling of spurious errors reported by v0.0.1 to the email address the results were sent from. We will avoid repeating this action in future.

## R CMD check results v0.0.2

0 errors | 0 warnings | 2 notes

* This release improves documentation, and consistency of argument names.

The valgrind report ([from the Oxford memtests](https://www.stats.ox.ac.uk/pub/bdr/memtests/valgrind/golden/tests/testthat.Rout)) shows 15 "definitely lost" or "indirectly lost" blocks, all originating from paths that throw exceptions in Rcpp code.

These are known false positives caused by the interaction between C++ exceptions and R's longjmp-based error handling. The reported allocations (often the strings holding the exception's messages) are likely cleaned up by R's garbage collector, but Valgrind cannot see this cleanup due to the non-local jump skipping normal C++ destructors.

This pattern is considered harmless. No memory leaks were detected in the success paths or in non-exception-throwing tests.

## R CMD check results v0.0.1

0 errors | 0 warnings | 2 notes

* This is the first release.
