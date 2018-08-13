
<!-- README.md is generated from README.Rmd. Please edit that file -->

sticRs: the [STICS](https://www6.paca.inra.fr/stics_eng/) model R package for developers <img src="man/figures/logo.jpg" alt="logo" width="300" align="right" />
================================================================================================================================================================

<!-- <img src="man/figures/logo.png" alt="logo" style="width:30%;height:auto;" align="right" /> -->
<!-- [![Travis build status](https://travis-ci.com/VEZY/DynACof.svg?branch=master)](https://travis-ci.org/VEZY/DynACof)   -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![Travis Build Status](https://travis-ci.com/VEZY/sticRs.svg?branch=master)](https://travis-ci.com/VEZY/sticRs) [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/cu1nyxrhc6nmpt5i/branch/master?svg=true)](https://ci.appveyor.com/project/VEZY/sticrs/branch/master) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Coverage status](https://codecov.io/gh/VEZY/sticRs/branch/master/graph/badge.svg)](https://codecov.io/github/VEZY/sticRs?branch=master)

Overview
--------

This package allows the user to programmatically:

-   read ([`read_param`](R/read_param.R)) or set ([`set_param`](R/read_param.R)) parameters

-   set ([`set_usm`](R/set_usm.R)) one or more simulations (USM)

-   call STICS to run them ([`run_stics`](R/run_stics.R))

-   import the results for analyzes ([`read_output`](R/read_output.R)), or the observations available in the USM ([`read_obs`](R/read_obs.R))

-   compare observations and simulations ([`eval_output`](R/eval_output.R))

-   make all previous at once in parallel to evaluate and compare the output of STICS versions or parameter values effect ([`stics_eval`](R/stics_eval.R))

-   generate automatic reports (only available for one experiment, development terminated but still under evaluation)

-   and run sensitivity analyzes ([`sensitive_stics`](R/sensitive_stics.R)) on STICS for one or more input parameters (and their possible interactions) on one or more output variables. Note that [`stics_eval`](R/stics_eval.R) can evaluate parameter change effect also, but doesn't run full sensitivity analyzes.

The package is under intensive development, so you can fill an issue or request me a feature [here](https://github.com/VEZY/sticRs/issues) at any time.

Installation
------------

The development version from [GitHub](https://github.com/) can be installed with:

``` r
devtools::install_github("VEZY/sticRs")
```

Or using the lightweight [remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("VEZY/sticRs")
```

The package is tested routinely to pass all [CRAN](https://CRAN.R-project.org) tests using Travis-CI (linux) and AppVeyor (Windows), but it is not released to the CRAN servers because we believe sticRs users are not widespread enough to bother CRAN people and server time.

Examples
--------

### Setting a parameter, running the model and compare with observations

#### Manually

This is a basic example using the default dummy simulation (parameters and meteorology) for a mixed crop of wheat-wheat (not a real mixed crop, for testing the model behavior) :

``` r
library("sticRs")
# Path to a preconfigured USM:
path_origin_USM= "Your_path_goes_here"
# Path where the simulation will be made:
path_simulation= "Your_path_goes_here_again"
# Path to the STICS model executable:
path_STICS= "Your_path_goes_here_again_and_again"
# Importing the preconfigured USM into a new folder:
import_usm(dir.orig = path_origin_USM, 
           dir.targ = path_simulation,
           stics = path_STICS, 
           usm_name = "test")
# Reading the interrang parameter for both plants (= interrow):
read_param(dirpath = file.path(path_simulation,"test"),
           param='interrang')
# Setting the interrang parameter to 0.30m for both plants:
set_param(dirpath = file.path(path_simulation,"test"),
          param= "interrang", value= 0.3,plant = c(1,2))
# Setting the outputs needed from STICS:
set_out_var(filepath = file.path(path_simulation,"test","var.mod"),
            vars = c("hauteur","lai(n)",'masec(n)'))
# Running the model:
run_stics(dirpath= file.path(path_simulation,"test"))
# Reading the model outputs:
out= read_output(dirpath= file.path(path_simulation,"test"))
# Plotting automatically the outputs along the observations: 
plot_output(file.path(path_simulation,"test"),
            obs_name = c("wheat.obs","pea.obs"))
```

To use your own data, simply use the folder of your simulation as the reference path; like you would do with javaSTICS.

#### Automatically

You can run all previous code using the simple, standardized `stics_eval` function:

``` r
library("sticRs")
out= 
  stics_eval(dir.orig = path_origin_USM, 
             dir.targ = path_simulation,
             stics = path_STICS,
             Parameter = list(interrang= 0.3),Plant = c(1,2),
             obs_name = c("wheat.obs","pea.obs"),
             Out_var = c("hauteur","lai(n)",'masec(n)'),
             Title = "Wheat-Wheat", plot_it = T)
```

This function will import the USM in a new folder, change the parameter values, run the model, return the outputs (simulation output + ggplot object) and plot it.

### Comparing model simulations

#### Comparing STICS outputs with different parameter values

If you want to compare the effect of different values of one or several parameters values on several STICS outputs, you can give different parameter values to the `stics_eval` function. Here is an exemple with the `P_rapforme` parameter:

``` r
Eval_stics= 
  stics_eval(dir.orig = path_origin_USM, 
             dir.targ = path_simulation,
             stics = path_STICS,
             Parameter = list(P_rapforme= list(1.5,2,4)),Plant = c(1,2),
             obs_name = c("wheat.obs","pea.obs"),
             Out_var = c("hauteur","laisen(n)","lai(n)","eai","largeur",
                         "varrapforme","dfol","dominant"),
             Title = "Wheat-Wheat", plot_it = T)
```

The function will return a list of STICS outputs for each parameter value, and a plot comparing the model simulations.

#### Comparing STICS outputs with different STICS versions

If you want to compare different versions of the model after modifying the code for exemple, you can give different stics values to the `stics_eval` function. Here is an exemple:

``` r
Eval_stics= 
  stics_eval(dir.orig = path_origin_USM, 
             dir.targ = path_simulation,
             stics = list(Original= path_STICS,
                          Modified= "Path_to_modified_stics/stics.exe"),
             obs_name = c("wheat.obs","pea.obs"),
             Out_var = c("hauteur","laisen(n)","lai(n)","eai","largeur",
                         "varrapforme","dfol","dominant"),
             Title = "Wheat-Wheat", plot_it = T)
```

The function will return a list of STICS outputs for each stics executable provided, and a plot comparing the model simulations.

### Making a sensitivity analysis

Make a sensitivity analysis using the `fast99` algorithm for the interrow (`interrang` parameter) for three main variables: the intercepted radiation (`raint`), the leaf area index (`lai(n)`), and the dry mass (`masec(n)`):

``` r
library("sticRs")
sens= sensitive_stics(dir.orig = path_origin_USM,
                      dir.targ = path_simulation,
                      stics = path_STICS,
                      obs_name = "Wheat.obs",Parameters = "interrang",
                      Vars = c("raint", "lai(n)", "masec(n)"),
                      method= "fast99", n= 10,
                      q= "qunif",q.arg = list(list(min=0.05, max=0.25),
                                              list(min=140, max=280)))
```

NB: `n`, `q`, and `q.arg` are parameters from the [`fast99`](https://cran.r-project.org/web/packages/sensitivity/sensitivity.pdf) function.

The output from [`sensitive_stics`](R/sensitive_stics.R) is a list of two:

-   A list of ggplot objects to plot the sensitivity of each variable to the parameter(s) along the rotation

-   A list of the output from the method function, *e.g.* a list of class `fast99` for the `fast99` method.

Example data
------------

Example data are available in the [tests folder](https://github.com/VEZY/sticRs/tree/master/tests/testthat/example_data). It is a dummy USM of wheat in self-intercropping, meaning that the model is run on the same plant planted in intercropping, to test if the model outputs are close to a sole crop simulation.
**Warning**: this example USM is made primarily to test the sticRs package, and is available to the user only for training, not for model validation. These data are dummy data that were entirely fabricated from scratch. It is not reflecting any real observations.

Acknowledgments
---------------

The STICS (Simulateur mulTIdisciplinaire pour les Cultures Standard, or multidisciplinary simulator for standard crops) model is a dynamic, generic and robust model aiming to simulate the soil-crop-atmosphere system. It was first developed in 1996 by INRA -the French National Institute for Agricultural research- by Nadine Brisson and Dominique Ripoche. An overview of the model is available [here](https://www6.paca.inra.fr/stics_eng/About-us/Stics-model-overview).

The sticRs package was developed thanks to the European H2020 funded [ReMIX project](https://www.remix-intercrops.eu/).

![ReMIX logo](man/figures/remix_logo.jpg)
-----------------------------------------
