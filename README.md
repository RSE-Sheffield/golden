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

Tests can then be executed.

```R
devtools::test()
```


## Creation From Scratch ##

See the vignette: `vignette("getting-started", package="eldoradosim")`


## Acknowledgements

todo which grant/project funded development?
