# golden <img src="man/figures/logo.png" align="right" height="138" alt="" /> #

A minimal best practice (ci, docs, test) rcpp package to be used as a template.

## Development ##

If making changes to the package locally, you will need both rebuild and reinstall it.

```r
library(Rcpp)
library(devtools)
Rcpp::compileAttributes() # Only required if new exported functions have been added
devtools::load_all()
```

*If this fails, it may be necessary to close all R sessions, they appear to share an installed package index.*

Tests can then be executed using `testthat`.

```R
library(testthat)
test_file(test_path("test-example.R"))
```

## Creation From Scratch ##

*Uses of "rcpp.testpkg" in the below example are the name of the package. Package names may only contain alphanumeric characters and periods.*

First the package must be created, `Rcpp` provides a function for this.
Open an `RTerm` window in the parent directory to where the package will be created.

```R
library(Rcpp)
Rcpp.package.skeleton("rcpp.testpkg")
```

*This creates the directory `rcpp.testpkg/` with the initial files required for an example Rcpp package.*

The `Rterm` working directory can now be moved to the package.

```R
setwd("rcpp.testpkg")
```

Next we replace `rcpp_hello_world.cpp` with our own test file `rcpp_interface.cpp`.

```cpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool is_odd(int d){
    return d % 2 != 0;
}
```

It is now necessary to regenerate the RCppexport files for the changed interface.

```R
Rcpp::compileAttributes()
```

*This updates `R/RCppExports.R` and `src/RCppExports.cpp`, which should not be manually updated.*

For testing, now the package can be installed.

```R
library(devtools)
devtools::install()
```

It should now be possible to load and test the package.

```R
library(rcpp.testpkg)
is_odd(3) # TRUE
is_odd(4) # FALSE
```

You may wish to delete these additional files from the example

* `man/rcpp_hello_world.Rd` (An example documentation page for the file we removed earlier)
* `Read-and-delete-me` (A simple guide for what to update)

Testing infrastructure can be added automatically, below "example" is the name of the test created.

```R
library(usethis)
usethis::use_test("example")
```
