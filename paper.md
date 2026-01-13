---
title: 'Golden: An R package for patient-level microsimulation'
tags:
  - R
  - Health
  - Microsimuation
authors:
  - name: Pete Dodd
    orcid: 0000-0001-5825-9347
    affiliation: 1
    equal-contrib: true
    corresponding: true 
  - name: Robert Chisholm
    orcid: 0000-0003-3379-9042
    affiliation: 2
    equal-contrib: true
affiliations:
 - name: Sheffield Centre for Health and Related Research (SCHARR), University of Sheffield, United Kingdom
   index: 1
 - name: Research Software Engineering, School of Computer Science, University of Sheffield, United Kingdom
   index: 2
date: 30 January 2026
bibliography: paper.bib
---
<!-- DELETE ME: Based on https://joss.readthedocs.io/en/latest/example_paper.html -->
# Summary

Fast, flexible, patient-level microsimulation. Time-stepped simulation with a C++ back-end from user-supplied initial population, trajectories, hazards, and corresponding event transitions. User-defined aggregate time series histories are returned together with the final population. Designed for simulation of chronic diseases with continuous and evolving risk factors, but could easily be applied more generally.

# Statement of need

`Golden` is an R package for patient-level microsimulation. Most R users are statisticians rather than software engineers, as such they are typically comfortable implementing statistical algorithms, but less comfortable with more traditional programming concepts. `Golden` is a light-weight framework, allowing R users to specify their statistical functions (whether implemented by hand, or via a 3rd party R library) and how they interact without needing the consider the underlying simulation logic. Attempts have been made to minimise overhead of `Golden`, by implementing simulation logic in RCPP, although practically the larger overhead is likely to come from a user's selected statistical functions.

In particular, `Golden` was designed to be used by population health researchers and students in population health that often use a time-stepped hazard and event transition modelling approach <!-- cite something with [@reference] here and flesh out further regarding the style of modelling golden is designed for-->.

# Software design

`Golden`'s design philosophy is based on three principles: (1) to provide an API friendly to statisticians, (2) to ensure thorough validation to catch user errors early and (3) to minimally impact performance through the use of RCPP.

Users create `Golden` models in a linear fashion, defining individual function objects represented either  hazards and their corresponding state transitions or state trajectories. Additionally, users can also define aggregation functions, termed "columns" referring to a column within the output history data-table, to be collected at regular intervals during simulation.

Building on of R's ability to pass functions as arguments which can be dynamically called, `Golden` allows users to focus their attention on applying the most effective statistical models. Whether they are self implemented in R, or come from high-performance R packages, they can be attached to a `Golden` function object (e.g. a hazard) and the population columns which should be passed as arguments named.

`Golden` is a fully independent simulation framework, built on the author's experience of working with simulation packages designed by developers rather than users.

# Research impact statement

<!-- todo-->

# AI usage disclosure

No generative AI tools were used in the development of this software, the writing
of this manuscript, or the preparation of supporting materials.

# Acknowledgements

Development of `Golden` was carried out as part of the ELDORADO project, funded under Horizon Europe via EDCTP3.

# References