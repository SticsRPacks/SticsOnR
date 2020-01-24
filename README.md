
# SticsOnR: The R package for the [STICS](https://www6.paca.inrae.fr/stics_eng/) model <img src="man/figures/logo.png" alt="logo" width="150" align="right" />

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Travis build
status](https://travis-ci.org/SticsRPacks/SticsOnR.svg?branch=master)](https://travis-ci.org/SticsRPacks/SticsOnR)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsOnR/branch/master/graph/badge.svg)](https://codecov.io/gh/SticsRPacks/SticsOnR?branch=master)

The goal of SticsOnR is to perform simulations of the Stics model,
downloadable with its graphical user interface from
<https://www6.paca.inra.fr/stics_eng/Download>.

<!--## Development 

Follow up the development [here](sticsOnR.md).-->

## Prerequisites and technical tips

### JavaStics software

JavaStics must be installed and the minimal version is version 1.41.

The last distribution version for Stics 9.1, JavaSTICS-1.41-stics-9.1,
is downloadable [here](https://www6.paca.inrae.fr/stics_eng/Download).

The installation process only constists of unzipping the JavaStics
archive.

### Under linux operating systems

#### Java version

For using the JavaStics software (GUI and command line interface) under
a `linux` operating system, the java version must be at most the Java 8
version.

So for recent distributions on which a higher version is installed some
manipulations can be done.

A description is given
[here](https://sticsrpacks.github.io/SticsOnR/articles/Changing_java_version_linux.html)
on how to fix it for using the R package (i.e. underlying JavaStics
command line interface).

#### System libraries

For the linux operating system, the SticsOnR package may require to
install an `xslt` library.

If the SticsOnR installation fails, and the `xslt` library is missing,
the error message indicates what is the name of the xslt library to be
installed (according to the common linux distributions). For example,
for the Ubuntu or Debian OS `libxslt1-dev` must be installed.

#### Files/directories paths syntax

Using the `~` in files or directories paths may cause errors in SticsOnR
functions. So, it is safer for the moment to use absolute paths. This
will be fixed in the future versions.

## Installation

The development version from [GitHub](https://github.com/) can be
installed with:

``` r
devtools::install_github("SticsRPacks/SticsOnR@*release")
```

Or using the lightweight
[remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOnR@*release")
```

Normaly, all packages dependencies will be installed, either CRAN
packages or SticsRPacks needed packages. In that case SticsRFiles and
CroptimizR will be installed as well.

## Examples

Here are basic examples which show you how to run the model either from
a R model interface or a JavaStics (command line) one. More complete
examples will be detailed in a specific documentation later.

### Running the JavaStics command line interface

The JavaStics installation folder (for example,
JavaSTICS-1.41-stics-9.1) contains an `example` workspace folder with a
set of runnable usms.

For running simulations from it, we can use the `run_javastics`
function.

In this SticsOnR package, for the moment, only one Stics version can be
used with it (i.e. the one delivered with the JavaStics installation
archive). JavaStics GUI, allows to manage several Stics executables, but
in that case one must take care of consistency between executables
version and input files format (advanced use case).

So, at this point, it is at your own risk if you try to use another
version than the JavaStics distribution one.

``` r

# Specifying the JavaStics folder
javastics_path <- "/path/to/JavaSTICS-1.41-stics-9.1"

# Specifying a workspace as a subfolder of JavaStics 
workspace_path <- "example"

# or an absolute path to an external folder
# workspace_path <- "/path/to/javastics/workspace"
```

``` r

# Running specific usms from the workspace
run_javastics(javastics_path, workspace_path, usms_list = c("banana","wheat"))
#> [1] "banana"
#> [1] "wheat"

# Running all usms contained in the workspace 
run_javastics(javastics_path, workspace_path)
#> [1] "SugarCane"
#> [1] "potato"
#> [1] "banana"
#> [1] "sorghum"
#> [1] "barley"
#> [1] "sugarbeet"
#> [1] "wheat"
#> [1] "maize"
#> [1] "soybean"
#> [1] "lettuce"
#> [1] "tomato"
#> [1] "DurumWheat"
#> [1] "rapeseed"
#> [1] "sunflower"
#> [1] "grass"
#> [1] "BareSoil"
#> [1] "demo_Wheat1"
#> [1] "demo_BareSoil2"
#> [1] "demo_maize3"
#> [1] "DurumWheat_4years"
#> [1] "maize_4years"
#> [1] "strawberry"
#> [1] "pea"
#> [1] "vine"
#> [1] "fescue"
#> [1] "flax"
#> [1] "intercrop_pea_barley"
#> [1] "timothy"
#> [1] "DurumWheat_snow"
#> [1] "Turmeric"
#> [1] "cc_BristleOat"
#> [1] "cc_mustard"
#> [1] "cc_ItalianRyegrass"
#> [1] "cc_vetch"
#> [1] "cc_CrimsonClover"
#> [1] "proto_rice"

# Getting returned information about execution 
runs_info <- run_javastics(javastics_path, workspace_path, usms_list = c("banana","wheat"), display = FALSE)

runs_info
#> [[1]]
#> [[1]]$name
#> [1] "banana"
#> 
#> [[1]]$error
#> [1] FALSE
#> 
#> [[1]]$message
#> [1] "[24/01/20]-[16:53:22] INFO - Modulostics files generation..\n[24/01/20]-[16:53:22] INFO - Generating txt files ...\n[24/01/20]-[16:53:23] INFO - Files generated under /home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example\nFiles generated :\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/mod_bbanana.sti\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/modhistory.sti"
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "wheat"
#> 
#> [[2]]$error
#> [1] FALSE
#> 
#> [[2]]$message
#> [1] "[24/01/20]-[16:53:24] INFO - Modulostics files generation..\n[24/01/20]-[16:53:24] INFO - Generating txt files ...\n[24/01/20]-[16:53:24] INFO - Files generated under /home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example\nFiles generated :\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/mod_bwheat.sti\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/modhistory.sti"
```

In the returned information, the error field name gives a list of
messages got from JavaStics commandline interface. If any `Error` key
word appears in a message, the corresponding simulation failed. But, at
the moment it is impossible to identify what is the error’s origin.
Things must be checked manually in the workspace, after running again
the faulty usm (because the model input files are overwritten at each
usms simulation).

<!-- SET eval to TRUE gen_usms_xml2txt has been moved to SticsRFiles -->

### Running the model

We need for that a JavaStics folder and a directory with text input
files for Stics, or a folder containing usms individual sub-directories
.

These directories can be generated using the `gen_usms_xml2txt` function
from the **SticsRFiles** package, by converting automatically XML files
to Stics input text files. See the documentation
[here](https://sticsrpacks.github.io/SticsRFiles/articles/Generating_Stics_text_files.html).

Example of use:

``` r
# Generating files for all the usms contained in the workspace
SticsRFiles::gen_usms_xml2txt(javastics_path,javastics_workspace_path =  workspace_path, target_path = output_path)
```

The `run_stics` function can be used as follows with one folder or
multiple sub-folders.

``` r
# Specifying the Stics executable file path

# for windows
# stics_path <- file.path(javastics_path,"bin","stics_modulo.exe")
# for linux
# stics_path <- file.path(javastics_path,"bin","stics_modulo")
# for Mac
# stics_path <- file.path(javastics_path,"bin","stics_modulo_mac")

# Specifying a directory containing Stics input files
# For example reusing a generated sub-directory in the previous section
# of the document
# Running on usm
files_dir_path <- file.path(output_path,"banana")
run_stics(stics_path, files_dir_path)

# Specifying a root directory containing usms individual directories
# For example reusing a generated directory in the previous section
# of the document
# Running two usms
run_stics(stics_path, output_path, usm_dir_names = c("banana","wheat"))

# Running all usms sub-directories
run_stics(stics_path, output_path, usm_dir_names = "all")

# Getting returned information about stics runs
runs_info <- run_stics(stics_path, output_path, usm_dir_names = c("banana","wheat"))

runs_info
#> [[1]]
#> [[1]]$name
#> [1] "banana"
#> 
#> [[1]]$error
#> [1] FALSE
#> 
#> [[1]]$message
#> [1] " numcult =            1"                           
#> [2] " The execution has been successfully accomplished."
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "wheat"
#> 
#> [[2]]$error
#> [1] FALSE
#> 
#> [[2]]$message
#> [1] " numcult =            1"                           
#> [2] " The execution has been successfully accomplished."
```

### Advanced simulations parameterization

A specific function `stics_wrapper` is dedicated to manage simulations
with a higher level of parameterization than what `run_stics` offers.

This `stics_wrapper` function allows:

  - Forcing the values of a set of parameters (common or specific values
    per USM)
  - Returning simulated daily outputs for each usm with possible dates
    and variables filtering
  - Parallelizing simulations, and displaying execution time

Be aware that for the moment, it **is not possible** to get daily
outputs for an `inter-cropping` use case. This will be implemented in
future developments.

As the `run_stics` function, the `stics_wrapper` operates on directories
containing text stics input files.

#### Defining simulations options

Simulation options can be fixed using the `stics_wrapper_options`
function. Both of them are mandatory: the model executable path and the
directory path containing usms sub-directories with text input files.

Here we reuse the model executable path defined prevously with respect
to the operating system (Windows, Linux or Mac) and the directory where
individual Usms input directories have been generated.

``` r

sim_options <- stics_wrapper_options(stics_path = stics_path, 
                                     data_dir = output_path)
```

Optional fields in the `sim_options` list will be detailed later in this
document.

Default values for these optional fields are set by
`stics_wrapper_options`. They can be displayed by calling the
`stics_wrapper_options` function without any argument as follows:

``` r
stics_wrapper_options()
#> $stics_path
#> [1] "unknown"
#> 
#> $data_dir
#> [1] "unknown"
#> 
#> $parallel
#> [1] FALSE
#> 
#> $cores
#> [1] NA
#> 
#> $time_display
#> [1] FALSE
#> 
#> $warning_display
#> [1] TRUE
```

An existing options list may be updated with new values using named
arguments as follows:

``` r
sim_options <- stics_wrapper_options(in_options = sim_options, parallel = TRUE)
```

#### Simple simulations cases

  - Without filtering usms or outputs

<!-- end list -->

``` r

results <- stics_wrapper(model_options = sim_options)
```

  - Filtering on usms list

<!-- end list -->

``` r

usms_list <- c("wheat", "pea", "maize")

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = usms_list)
```

  - Filtering outputs on usms variables and dates using observations
    data

The argument `sit_var_dates_mask` must contain a named list (with usms
names) containing data.frames organized as observations data or
corresponding to observations data.

``` r
obs_filenames <- paste0(usms_list,".obs")
obs_list <- get_obs(dirpath = workspace_path, obs_filenames = obs_filenames)

# Observations table for wheat
obs_list$wheat
#>          Date lai_n masec_n HR_1 HR_2 HR_3 resmes AZnit_1 AZnit_2 AZnit_3
#> 1  1995-01-30  0.29    0.25   NA   NA   NA     NA      NA      NA      NA
#> 2  1995-02-03    NA      NA 21.1 18.8 12.4 133.26     3.1     2.9       4
#> 3  1995-02-07  0.37    0.31   NA   NA   NA     NA      NA      NA      NA
#> 4  1995-02-16  0.40    0.32   NA   NA   NA     NA      NA      NA      NA
#> 5  1995-02-24  0.45    0.40   NA   NA   NA     NA      NA      NA      NA
#> 6  1995-03-06  0.44    0.38   NA   NA   NA     NA      NA      NA      NA
#> 7  1995-03-16  0.60    0.59   NA   NA   NA     NA      NA      NA      NA
#> 8  1995-03-23  0.85    0.87   NA   NA   NA     NA      NA      NA      NA
#> 9  1995-04-03  1.47    1.28   NA   NA   NA     NA      NA      NA      NA
#> 10 1995-04-11  2.37    2.68   NA   NA   NA     NA      NA      NA      NA
#> 11 1995-04-18  2.65    3.70   NA   NA   NA     NA      NA      NA      NA
#> 12 1995-04-26  4.55    4.67   NA   NA   NA     NA      NA      NA      NA
#> 13 1995-05-02  4.41    5.77   NA   NA   NA     NA      NA      NA      NA
#> 14 1995-05-05    NA    6.01   NA   NA   NA     NA      NA      NA      NA
#> 15 1995-05-09  5.20    7.51   NA   NA   NA     NA      NA      NA      NA
#> 16 1995-05-12    NA    9.73   NA   NA   NA     NA      NA      NA      NA
#> 17 1995-05-15  5.98    9.87   NA   NA   NA     NA      NA      NA      NA
#> 18 1995-05-19    NA   11.08   NA   NA   NA     NA      NA      NA      NA
#> 19 1995-05-29    NA   14.08   NA   NA   NA     NA      NA      NA      NA
#> 20 1995-07-17    NA   21.91   NA   NA   NA     NA      NA      NA      NA
#>    QNplante
#> 1      9.24
#> 2        NA
#> 3     10.85
#> 4     12.61
#> 5     13.32
#> 6     16.65
#> 7     24.59
#> 8     38.86
#> 9     60.99
#> 10    98.98
#> 11   103.36
#> 12   141.99
#> 13   170.03
#> 14       NA
#> 15   188.57
#> 16       NA
#> 17   216.06
#> 18       NA
#> 19   258.38
#> 20   258.45

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = obs_list)
#> Warning: Variable(s) INN, hmax not simulated by the Stics model for USM pea =>
#> try to add it(them) in /home/plecharpent/tmp/tests_SticsOnR/gen_usms_xml2txt/
#> pea/var.mod
#> Warning: Requested date(s) 1996-04-16 is(are) not simulated for USM maize
```

Some warnings may occur, indicating that observed variables and/or
observations dates are missing in simulated data. Concerning the dates,
this may be due to the USMs simulation period that may not include
observed dates. For the variables, this may be due to an incorrect
spelling of the variables in obs\_list or to the list of simulated
variables defined in the var.mod file.

#### Simulations with forcing parameters

  - Applying a single parameter values vector for all the selected usms

Parameters values are prescribed using the `param_values` argument. It
can be a named vector containing the values and names of the parameters
to force. In this case, the same values will be applied for all the
simulated usms.

``` r

param_values <- c(0.002,50)
names(param_values) <- c("dlaimax", "durvieF")

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = usms_list, param_values = param_values)
```

  - Defining different parameters values depending on the usms or
    defining several parameters values per usms

`param_values` can also be a named 3D array containing the value(s) and
names of the parameters to force for each situation to simulate. This
array contains the different parameters values (first dimension) for the
different parameters (second dimension) and for the different usms
(third dimension). By this way, the parameters can take different values
depending on the usms. Also, usms can be simulated several times each
with different values of the parameters.

``` r

# Let's run usm wheat with c(dlaimax=0.001, durvieF=50) and c(dlaimax=0.002, durvieF=50),
# usm pea with c(dlaimax=0.001, durvieF=60) and c(dlaimax=0.002, durvieF=60),
# and usm maize with c(dlaimax=0.001, durvieF=70) and c(dlaimax=0.002, durvieF=70)
param_values <- array( c(0.001, 0.002, 50, 50, 
                         0.001, 0.002, 60, 60, 
                         0.001, 0.002, 70, 70),
                      dim=c(2,2,3),
                      dimnames=list(NULL,c("dlaimax", "durvieF"),c("wheat", "pea", "maize")))
                      
# Let's display it
param_values
#> , , wheat
#> 
#>      dlaimax durvieF
#> [1,]   0.001      50
#> [2,]   0.002      50
#> 
#> , , pea
#> 
#>      dlaimax durvieF
#> [1,]   0.001      60
#> [2,]   0.002      60
#> 
#> , , maize
#> 
#>      dlaimax durvieF
#> [1,]   0.001      70
#> [2,]   0.002      70

# In this case, no need to redefine the usms list in sit_var_dates_mask argument, it is already
# given in param_values
results <- stics_wrapper(model_options = sim_options, param_values = param_values)

# Let's display the results
head(results)
#> $error
#> [1] FALSE
#> 
#> $sim_list
#> $sim_list[[1]]
#> $sim_list[[1]]$wheat
#> # A tibble: 411 x 51
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 1994-10-17 00:00:00  1994    10    17   290     0       0       0  2.53  4.80
#> 2 1994-10-18 00:00:00  1994    10    18   291     0       0       0  2.31  4.66
#> 3 1994-10-19 00:00:00  1994    10    19   292     0       0       0  4.55  4.44
#> 4 1994-10-20 00:00:00  1994    10    20   293     0       0       0  4.49  4.41
#> 5 1994-10-21 00:00:00  1994    10    21   294     0       0       0  5.36  4.35
#> # … with 406 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   HR_5 <dbl>, resmes <dbl>, drain <dbl>, esol <dbl>, et <dbl>, zrac <dbl>,
#> #   tcult <dbl>, AZnit_1 <dbl>, AZnit_2 <dbl>, AZnit_3 <dbl>, AZnit_4 <dbl>,
#> #   AZnit_5 <dbl>, Qles <dbl>, QNplante <dbl>, azomes <dbl>, inn <dbl>,
#> #   chargefruit <dbl>, AZamm_1 <dbl>, AZamm_2 <dbl>, AZamm_3 <dbl>,
#> #   AZamm_4 <dbl>, AZamm_5 <dbl>, CNgrain <dbl>, concNO3les <dbl>, drat <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, Hmax <dbl>, humidite <dbl>, LRACH_1 <dbl>,
#> #   LRACH_2 <dbl>, LRACH_3 <dbl>, LRACH_4 <dbl>, LRACH_5 <dbl>, mafrais <dbl>,
#> #   pdsfruitfrais <dbl>, Qdrain <dbl>, rnet <dbl>, cum_jul <dbl>
#> 
#> $sim_list[[1]]$pea
#> # A tibble: 129 x 51
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 2003-03-11 00:00:00  2003     3    11    70     0       0       0  20.8  25.7
#> 2 2003-03-12 00:00:00  2003     3    12    71     0       0       0  20.3  25.7
#> 3 2003-03-13 00:00:00  2003     3    13    72     0       0       0  19.7  25.6
#> 4 2003-03-14 00:00:00  2003     3    14    73     0       0       0  18.9  25.5
#> 5 2003-03-15 00:00:00  2003     3    15    74     0       0       0  18.1  25.4
#> # … with 124 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   HR_5 <dbl>, resmes <dbl>, drain <dbl>, esol <dbl>, et <dbl>, zrac <dbl>,
#> #   tcult <dbl>, AZnit_1 <dbl>, AZnit_2 <dbl>, AZnit_3 <dbl>, AZnit_4 <dbl>,
#> #   AZnit_5 <dbl>, Qles <dbl>, QNplante <dbl>, azomes <dbl>, inn <dbl>,
#> #   chargefruit <dbl>, AZamm_1 <dbl>, AZamm_2 <dbl>, AZamm_3 <dbl>,
#> #   AZamm_4 <dbl>, AZamm_5 <dbl>, CNgrain <dbl>, concNO3les <dbl>, drat <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, Hmax <dbl>, humidite <dbl>, LRACH_1 <dbl>,
#> #   LRACH_2 <dbl>, LRACH_3 <dbl>, LRACH_4 <dbl>, LRACH_5 <dbl>, mafrais <dbl>,
#> #   pdsfruitfrais <dbl>, Qdrain <dbl>, rnet <dbl>, cum_jul <int>
#> 
#> $sim_list[[1]]$maize
#> # A tibble: 249 x 51
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 1996-04-21 00:00:00  1996     4    21   112     0       0       0  22.4  22.3
#> 2 1996-04-22 00:00:00  1996     4    22   113     0       0       0  22.5  24.5
#> 3 1996-04-23 00:00:00  1996     4    23   114     0       0       0  22.5  24.7
#> 4 1996-04-24 00:00:00  1996     4    24   115     0       0       0  22.2  24.7
#> 5 1996-04-25 00:00:00  1996     4    25   116     0       0       0  22.0  24.7
#> # … with 244 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   HR_5 <dbl>, resmes <dbl>, drain <dbl>, esol <dbl>, et <dbl>, zrac <dbl>,
#> #   tcult <dbl>, AZnit_1 <dbl>, AZnit_2 <dbl>, AZnit_3 <dbl>, AZnit_4 <dbl>,
#> #   AZnit_5 <dbl>, Qles <dbl>, QNplante <dbl>, azomes <dbl>, inn <dbl>,
#> #   chargefruit <dbl>, AZamm_1 <dbl>, AZamm_2 <dbl>, AZamm_3 <dbl>,
#> #   AZamm_4 <dbl>, AZamm_5 <dbl>, CNgrain <dbl>, concNO3les <dbl>, drat <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, Hmax <dbl>, humidite <dbl>, LRACH_1 <dbl>,
#> #   LRACH_2 <dbl>, LRACH_3 <dbl>, LRACH_4 <dbl>, LRACH_5 <dbl>, mafrais <dbl>,
#> #   pdsfruitfrais <dbl>, Qdrain <dbl>, rnet <dbl>, cum_jul <int>
#> 
#> 
#> $sim_list[[2]]
#> $sim_list[[2]]$wheat
#> # A tibble: 411 x 51
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 1994-10-17 00:00:00  1994    10    17   290     0       0       0  2.53  4.80
#> 2 1994-10-18 00:00:00  1994    10    18   291     0       0       0  2.31  4.66
#> 3 1994-10-19 00:00:00  1994    10    19   292     0       0       0  4.55  4.44
#> 4 1994-10-20 00:00:00  1994    10    20   293     0       0       0  4.49  4.41
#> 5 1994-10-21 00:00:00  1994    10    21   294     0       0       0  5.36  4.35
#> # … with 406 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   HR_5 <dbl>, resmes <dbl>, drain <dbl>, esol <dbl>, et <dbl>, zrac <dbl>,
#> #   tcult <dbl>, AZnit_1 <dbl>, AZnit_2 <dbl>, AZnit_3 <dbl>, AZnit_4 <dbl>,
#> #   AZnit_5 <dbl>, Qles <dbl>, QNplante <dbl>, azomes <dbl>, inn <dbl>,
#> #   chargefruit <dbl>, AZamm_1 <dbl>, AZamm_2 <dbl>, AZamm_3 <dbl>,
#> #   AZamm_4 <dbl>, AZamm_5 <dbl>, CNgrain <dbl>, concNO3les <dbl>, drat <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, Hmax <dbl>, humidite <dbl>, LRACH_1 <dbl>,
#> #   LRACH_2 <dbl>, LRACH_3 <dbl>, LRACH_4 <dbl>, LRACH_5 <dbl>, mafrais <dbl>,
#> #   pdsfruitfrais <dbl>, Qdrain <dbl>, rnet <dbl>, cum_jul <dbl>
#> 
#> $sim_list[[2]]$pea
#> # A tibble: 129 x 51
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 2003-03-11 00:00:00  2003     3    11    70     0       0       0  20.8  25.7
#> 2 2003-03-12 00:00:00  2003     3    12    71     0       0       0  20.3  25.7
#> 3 2003-03-13 00:00:00  2003     3    13    72     0       0       0  19.7  25.6
#> 4 2003-03-14 00:00:00  2003     3    14    73     0       0       0  18.9  25.5
#> 5 2003-03-15 00:00:00  2003     3    15    74     0       0       0  18.1  25.4
#> # … with 124 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   HR_5 <dbl>, resmes <dbl>, drain <dbl>, esol <dbl>, et <dbl>, zrac <dbl>,
#> #   tcult <dbl>, AZnit_1 <dbl>, AZnit_2 <dbl>, AZnit_3 <dbl>, AZnit_4 <dbl>,
#> #   AZnit_5 <dbl>, Qles <dbl>, QNplante <dbl>, azomes <dbl>, inn <dbl>,
#> #   chargefruit <dbl>, AZamm_1 <dbl>, AZamm_2 <dbl>, AZamm_3 <dbl>,
#> #   AZamm_4 <dbl>, AZamm_5 <dbl>, CNgrain <dbl>, concNO3les <dbl>, drat <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, Hmax <dbl>, humidite <dbl>, LRACH_1 <dbl>,
#> #   LRACH_2 <dbl>, LRACH_3 <dbl>, LRACH_4 <dbl>, LRACH_5 <dbl>, mafrais <dbl>,
#> #   pdsfruitfrais <dbl>, Qdrain <dbl>, rnet <dbl>, cum_jul <int>
#> 
#> $sim_list[[2]]$maize
#> # A tibble: 249 x 51
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 1996-04-21 00:00:00  1996     4    21   112     0       0       0  22.4  22.3
#> 2 1996-04-22 00:00:00  1996     4    22   113     0       0       0  22.5  24.5
#> 3 1996-04-23 00:00:00  1996     4    23   114     0       0       0  22.5  24.7
#> 4 1996-04-24 00:00:00  1996     4    24   115     0       0       0  22.2  24.7
#> 5 1996-04-25 00:00:00  1996     4    25   116     0       0       0  22.0  24.7
#> # … with 244 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   HR_5 <dbl>, resmes <dbl>, drain <dbl>, esol <dbl>, et <dbl>, zrac <dbl>,
#> #   tcult <dbl>, AZnit_1 <dbl>, AZnit_2 <dbl>, AZnit_3 <dbl>, AZnit_4 <dbl>,
#> #   AZnit_5 <dbl>, Qles <dbl>, QNplante <dbl>, azomes <dbl>, inn <dbl>,
#> #   chargefruit <dbl>, AZamm_1 <dbl>, AZamm_2 <dbl>, AZamm_3 <dbl>,
#> #   AZamm_4 <dbl>, AZamm_5 <dbl>, CNgrain <dbl>, concNO3les <dbl>, drat <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, Hmax <dbl>, humidite <dbl>, LRACH_1 <dbl>,
#> #   LRACH_2 <dbl>, LRACH_3 <dbl>, LRACH_4 <dbl>, LRACH_5 <dbl>, mafrais <dbl>,
#> #   pdsfruitfrais <dbl>, Qdrain <dbl>, rnet <dbl>, cum_jul <int>
```

#### Optional arguments

  - Displaying execution time

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path,
                                     time_display = TRUE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 22.00408 secs
```

  - Activating parallel execution

On may specify the number of cores to use with the cores argument.

``` r
sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE, cores = 2)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 13.30597 secs
```

If cores is not given, parallel execution is performed over machine
total cores number minus 1.

``` r
# Used cores number
detectCores() - 1
#> [1] 3

sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 11.3027 secs
```

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Authors and acknowledgments

The SticsOnR package is developed by Patrice Lecharpentier, Rémi Vezy
and the [SticsOnR
Team](https://github.com/orgs/SticsRPacks/teams/sticsonr).
