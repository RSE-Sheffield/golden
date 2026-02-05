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

Microsimulation is a popular approach to modelling outcomes in health economics and related fields, especially where individual heterogeneity is important or where there are variable series of events that drive health or cost consequences [@Krijkamp2018-ac]. Often such simulations may be based on statistical analyses of longitudinal data that inform trajectories of risk factors and their relationship with event risks [@Breeze2016-ia]. Increasingly-available modern statistical approaches such as joint modelling allow for multivariate modelling of multiple trajectories and events in a way that captures correlations between them, and can improve accuracy by linking event rates to concurrent trajectory values [@Henderson2000-lw]. 

However, many simulations are developed with bespoke code on a case-by-case basis, which duplicates work, increases the probability of error, and makes it difficult to compare or reuse aspects of simulation logic. Moreover, implementations are often parametrized by extracting parameters from statistical models and using them as simulation inputs, which has several disadvantages compared to direct use of fitted model objects in defining the simulation.

R is widely used for statistical analyses, including analyses of the sort of data often used to parametrize microsimulation models, and is increasingly used for population health and health economic modelling, see e.g. [DARTH](https://www.darthworkgroup.com/), [R for HTA](https://r-hta.org/). As of 2/2026, the Comprehensive R Archive Network ([CRAN](https://cran.r-project.org/) has only 4 packages for microsimulation, only two of which could be used in this domain: `MicSim`[@ZinnUnknown-wl] and `microsimulation`[@Karlsson2016-jm].

`MicSim` implements a continuous-time discrete-event simulation framework in pure R for continuous-time multi-state (semi-Markov) models, and was developed for demographic and life-course simulation. Model specification is therefore via transition matrices and therefore cannot accommodate continuous state variables. Microsimulation is closer in purpose and design to golden in being initially developed for a prostate cancer cost-effectiveness analysis, and using Rcpp to link to utilize a simulation engine written in C++ ([SSim](https://www.inf.usi.ch/carzaniga/ssim/index.html)). As with `MicSim`, `microsimulation` also uses discrete-event simulation, and includes several functions which are specific to cost-effectiveness analysis. However, it requires users to code in a lower-level C++ API to specify models [CHECK]. Both packages require user post-processing of outputs and need a user to work with a larger vocabulary of functions and concepts than golden to specify simulations.

TODO get hold of paper for latter

TODO check heemod hesim & include

TODO comment on time-updating in DES formulations


# Features and approach

`Golden` is an R package for patient-level microsimulation designed to address these needs. `Golden` is a light-weight framework, allowing R users to specify their statistical functions (whether implemented by hand, or via a 3rd party R library) and how they interact without needing to consider the underlying simulation logic. A single function is used to run a simulation once the necessary inputs have been assembled (i.e. a parameter object, and an initial population).

The intent has been to use concepts that will be familiar and easy to work with for the majority of users, and to maintain flexibility while enforcing a particular structure in defining a model. The initial population is provided to the simulation engine, and the final population returned from it, in the form of a data.frame or data.table with one row per patient and columns representing variables - the type of data structure commonly used to store population data. Thus detailed associations can be assessed on outputs, and this mechanism can be leveraged to capture results as additional variables as new columns. ‘Histories’ can be defined to return time series of user-defined summary statistics for population subsets (i.e. rows matching some user-defined selection criterion). The time-stepped nature of the simulation means that no additional processing of results is required to align event timings. ‘Trajectories’ can specify rules for updating risk factor variables’ evolution that can include stochasticity and multivariate models of evolution (e.g. from joint models). ‘Hazards’ for events can depend on arbitrary patient data, and can trigger associated sets of ‘transitions’, which themselves can simultaneously affect changes on multiple columns. The ability to represent multivariate trajectories can be exploited to simplify or logically group functions defining evolution, even when the risk factors involved are not correlated. Similarly, transitions acting on multiple columns may be necessary, e.g. to allow for random, correlated outcome events, but even when not necessary may have advantages over lists of separately-defined transitions associated with an event in terms of reducing lines of code or conveniently grouping related outcomes. Golden can also handle deterministic events via infinite hazards. For further details and examples, please see the package vignette.
 

`Golden` was developed as part of the ELDORADO project in order to provide a basis on which to create a microsimulation model of long-term health outcomes for people living with HIV. However, it has been designed as a completely general tool. Although the focus is on modelling trajectories of continuous risk factors and associated events, `Golden` can equally well model discrete risk factors, e.g. as in Markov or multistate models, which are commonly-used in cost-effectiveness modelling [@Putter2007-lo, @Alarid-Escudero2023-mh].

# Software design

`Golden`'s design philosophy is based on three principles: (1) to provide an API friendly to statisticians, (2) to ensure thorough validation to catch user errors early and (3) to minimally impact performance through the use of Rcpp [@Rcpp]. While the implementation of `Golden`'s  simulation logic in Rcpp will allow highly-performant simulation, in practice the larger overhead is likely to come from a user's input functions defining trajectories and hazards.

Users create `Golden` models in a linear fashion, defining individual function objects representing either  hazards and their corresponding state transitions or state trajectories. Additionally, users can also define aggregation functions, termed "columns" referring to a column within the output history data-table, to be collected at regular intervals during simulation.

Building on R's ability to pass functions as arguments which can be dynamically called, `Golden` allows users to focus their attention on applying the most effective statistical models. Whether they are self implemented in R, or come from high-performance R packages, they can be attached to a `Golden` function object (e.g. a hazard) and the population columns which should be passed as arguments named.

`Golden` is a fully independent simulation framework, built on the author's experience of working with simulation packages designed by developers rather than users.

The simulation configuration is composed of a set of S3 objects. This design represents a deliberate compromise, providing an idiomatic R interface while avoiding the additional complexity of more advanced object-oriented systems. To reduce the risk of incorrect configuration, dedicated constructor functions are provided that perform comprehensive input validation. This validation is repeated when the configuration is passed to the main simulation executor, allowing the system to detect errors related to the input data table, such as missing columns, as well as inconsistencies introduced by manual modification of the underlying S3 objects.

Additional optional validation can be enabled within the simulation loop itself. When active, this raises informative errors if user-supplied functions return invalid values, such as NaN, which can otherwise be difficult to diagnose. The package, including all validation mechanisms, is supported by an extensive `testthat` test suite comprising more than 300 individual checks.

# AI usage disclosure

No generative AI tools were used in the development of this software, the writing
of this manuscript, or the preparation of supporting materials.

# Acknowledgements

Development of `Golden` was carried out as part of the ELDORADO project, funded under Horizon Europe via EDCTP3.

# References
