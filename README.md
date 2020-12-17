
``` r
library(SticsOnR)
#> Learn SticsOnR at: https://SticsRPacks.github.io/SticsOnR
library(SticsRFiles)
#> Learn SticsRFiles at: https://SticsRPacks.github.io/SticsRFiles
```

# SticsOnR: The R package for the [STICS](https://www6.paca.inrae.fr/stics_eng/) model <img src="man/figures/logo.png" alt="logo" width="150" align="right" />

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/SticsRPacks/SticsOnR/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/SticsOnR/actions)
[![Travis build
status](https://travis-ci.org/SticsRPacks/SticsOnR.svg?branch=master)](https://travis-ci.org/SticsRPacks/SticsOnR)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsOnR/branch/master/graph/badge.svg)](https://codecov.io/gh/SticsRPacks/SticsOnR?branch=master)

The goal of SticsOnR is to perform simulations of the Stics model,
downloadable with its graphical user interface from
<https://www6.paca.inra.fr/stics_eng/Download>.

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

#### Remote installation tools

For installing packages from the Github site an additional package must
be installed. One can use either `devtools` or
[`remotes`](https://github.com/r-lib/remotes#readme)

For `devtools`, installation requires system dependent tools. They must
be installed first.

  - For Windows  
    RTools must be installed using the last installer from
    [here](https://cran.r-project.org/bin/windows/Rtools)
  - For linux  
    Development tools must be installed first, like the
    `build-essentials` package for a Debian like distribution for
    example.

Then the `devtools` package can be installed using:

``` r
install:::packages("devtools")
```

For `remotes`, it can be directly installed using:

``` r
install:::packages("remotes")
```

## Installation

### Recommended installation: `SticsRPacks`

The best way to install the packages from `SticsRPacks`, from which
`SticsOnR` is part of, is by installing the `[SticsRPacks]` package:

``` r
devtools::install_github("SticsRPacks/SticsRPacks")
```

  - With `remotes`

<!-- end list -->

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsRPacks")
```

The package will install the packages for you at the last release
version.

### Other way: install each package independently

#### SticsOnR

The package installation can be remotely done directly from
[GitHub](https://github.com/) using either `devtools` or the lightweight
`remote` one package

The last release version can be installed using:

  - With `devtools`

<!-- end list -->

``` r
devtools::install_github("SticsRPacks/SticsOnR@*release")
```

  - With `remotes`

<!-- end list -->

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOnR@*release")
```

Normaly, all the package dependencies will be installed for CRAN
packages.

#### SticsRFiles

`SticsRFiles` must be installed manually using the above syntax, just
replacing **SticsOnR** with **SticsRFiles**.

## Examples

Here are basic examples which show you how to run the model either from
a R model interface or a JavaStics (command line) one. More complete
examples will be detailed in a specific documentation later.

### Running the model using JavaStics command line interface (recommended)

The JavaStics installation folder (for example,
JavaSTICS-1.41-stics-9.1) contains an `example` workspace folder with a
set of runnable usms.

For running simulations from it, we can use the `run_javastics()`
function.

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
#> v Using stics 'modulostics' (exe: 'stics_modulo.exe')
#> [1] "banana"
#> [1] "wheat"

# Running all usms contained in the workspace 
run_javastics(javastics_path, workspace_path)
#> v Using stics 'modulostics' (exe: 'stics_modulo.exe')
#> [1] "SugarCane"
#> [1] "potato"
#> [1] "banana"
#> [1] "sorghum"
#> [1] "barley"
#> [1] "ccMustard"
#> [1] "ccRyeGrass"
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

# Getting information about execution:
runs_info <- run_javastics(javastics_path, workspace_path, usms_list = c("banana","wheat"))
#> v Using stics 'modulostics' (exe: 'stics_modulo.exe')
#> [1] "banana"
#> [1] "wheat"

runs_info
#> [[1]]
#> [[1]]$name
#> [1] "banana"
#> 
#> [[1]]$error
#> [1] FALSE
#> 
#> [[1]]$message
#> [1] "0"
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
#> [1] "0"
```

In the returned information, the error field name gives a list of
messages from the JavaStics commandline interface. If any `Error` key
word appears in a message, the corresponding simulation failed. But, at
the moment it is impossible to identify what is the error’s origin.
Things must be checked manually in the workspace, after running again
the faulty usm (because the model input files are overwritten at each
usms simulation).

### Running the model using the stics executable directly

We need for that a JavaStics folder and a directory with text input
files for Stics, or a folder containing individual sub-directories for
usms.

These directories can be generated using the
`[SticsRFiles::gen_usms_xml2txt()]` function from the **SticsRFiles**
package, by converting automatically XML files to Stics input text
files. See the documentation
[here](https://sticsrpacks.github.io/SticsRFiles/articles/Generating_Stics_text_files.html).

Example of use:

``` r
# Generating files for all the usms contained in the workspace
SticsRFiles::gen_usms_xml2txt(javastics_path, workspace_path = workspace_path, target_path = output_path, verbose = FALSE)
```

The `run_stics()` function can be used as follows with one folder or
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
#> [2] " The execution has been successfully accomplished"
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
#> [2] " The execution has been successfully accomplished"
```

### Advanced simulations parameterization

A specific function `stics_wrapper()` is dedicated to manage simulations
with a higher level of parameterization than what `run_stics()` offers.

This `stics_wrapper()` function allows:

  - Forcing the values of a set of parameters (common or specific values
    per USM)
  - Returning simulated daily outputs for each usm with possible dates
    and variables filtering
  - Parallelizing simulations, and displaying execution time

As the `run_stics()` function, the `stics_wrapper()` operates on
directories containing text stics input files.

#### Defining simulations options

Simulation options can be fixed using the `stics_wrapper_options()`
function. Both of them are mandatory: the model executable path and the
directory path containing usms sub-directories with text input files.

A template is returned by the function when called with no arguments:

``` r
stics_wrapper_options()
#> $javastics_path
#> [1] "unknown"
#> 
#> $stics_exe
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
#> $verbose
#> [1] TRUE
```

For the example, we will use the default stics model version shipping
with JavaStics and the directory where individual usms input directories
have been generated:

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path, 
                                     data_dir = output_path, verbose = FALSE)
```

By default, `stics_wrapper_options()` checks that `javastics_path`,
`stics_exe` and `data_dir` exists.

There are different solutions if you need to use a custom version of
stics:

1.  if its already listed in the preference (e.g. added in JavaStics),
    simply provide its name (ID):

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path, stics_exe = "stics_custom",
                                     data_dir = output_path, verbose = FALSE)
```

1.  if its located in the bin directory of the JavaStics installation
    directory, provide the executable name:

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path, stics_exe = "stics_custom.exe",
                                     data_dir = output_path, verbose = FALSE)
```

1.  if its located in any other folder, provide the full path to the
    executable name, and no need to use `javastics_path`:

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(stics_exe = "path/to/stics_custom.exe",
                                     data_dir = output_path, verbose = FALSE)
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
obs_list <- get_obs(workspace = workspace_path, usm_name = usms_list, verbose = FALSE)
#> ! plant folder not found in the workspace, please add `javastics_path` to use real plant names from javaStics.

# Observations table for wheat
obs_list$wheat
#>          Date  ian mo jo jul lai_n masec_n HR_1 HR_2 HR_3 resmes AZnit_1
#> 1  1995-01-30 1995  1 30  30  0.29    0.25   NA   NA   NA     NA      NA
#> 2  1995-02-03 1995  2  3  34    NA      NA 21.1 18.8 12.4 133.26     3.1
#> 3  1995-02-07 1995  2  7  38  0.37    0.31   NA   NA   NA     NA      NA
#> 4  1995-02-16 1995  2 16  47  0.40    0.32   NA   NA   NA     NA      NA
#> 5  1995-02-24 1995  2 24  55  0.45    0.40   NA   NA   NA     NA      NA
#> 6  1995-03-06 1995  3  6  65  0.44    0.38   NA   NA   NA     NA      NA
#> 7  1995-03-16 1995  3 16  75  0.60    0.59   NA   NA   NA     NA      NA
#> 8  1995-03-23 1995  3 23  82  0.85    0.87   NA   NA   NA     NA      NA
#> 9  1995-04-03 1995  4  3  93  1.47    1.28   NA   NA   NA     NA      NA
#> 10 1995-04-11 1995  4 11 101  2.37    2.68   NA   NA   NA     NA      NA
#> 11 1995-04-18 1995  4 18 108  2.65    3.70   NA   NA   NA     NA      NA
#> 12 1995-04-26 1995  4 26 116  4.55    4.67   NA   NA   NA     NA      NA
#> 13 1995-05-02 1995  5  2 122  4.41    5.77   NA   NA   NA     NA      NA
#> 14 1995-05-05 1995  5  5 125    NA    6.01   NA   NA   NA     NA      NA
#> 15 1995-05-09 1995  5  9 129  5.20    7.51   NA   NA   NA     NA      NA
#> 16 1995-05-12 1995  5 12 132    NA    9.73   NA   NA   NA     NA      NA
#> 17 1995-05-15 1995  5 15 135  5.98    9.87   NA   NA   NA     NA      NA
#> 18 1995-05-19 1995  5 19 139    NA   11.08   NA   NA   NA     NA      NA
#> 19 1995-05-29 1995  5 29 149    NA   14.08   NA   NA   NA     NA      NA
#> 20 1995-07-17 1995  7 17 198    NA   21.91   NA   NA   NA     NA      NA
#>    AZnit_2 AZnit_3 QNplante         Plant
#> 1       NA      NA     9.24 wheat_plt.xml
#> 2      2.9       4       NA wheat_plt.xml
#> 3       NA      NA    10.85 wheat_plt.xml
#> 4       NA      NA    12.61 wheat_plt.xml
#> 5       NA      NA    13.32 wheat_plt.xml
#> 6       NA      NA    16.65 wheat_plt.xml
#> 7       NA      NA    24.59 wheat_plt.xml
#> 8       NA      NA    38.86 wheat_plt.xml
#> 9       NA      NA    60.99 wheat_plt.xml
#> 10      NA      NA    98.98 wheat_plt.xml
#> 11      NA      NA   103.36 wheat_plt.xml
#> 12      NA      NA   141.99 wheat_plt.xml
#> 13      NA      NA   170.03 wheat_plt.xml
#> 14      NA      NA       NA wheat_plt.xml
#> 15      NA      NA   188.57 wheat_plt.xml
#> 16      NA      NA       NA wheat_plt.xml
#> 17      NA      NA   216.06 wheat_plt.xml
#> 18      NA      NA       NA wheat_plt.xml
#> 19      NA      NA   258.38 wheat_plt.xml
#> 20      NA      NA   258.45 wheat_plt.xml

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = obs_list)
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
#> # ... with 406 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
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
#> # A tibble: 129 x 25
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 2003-03-11 00:00:00  2003     3    11    70     0       0       0     0     0
#> 2 2003-03-12 00:00:00  2003     3    12    71     0       0       0     0     0
#> 3 2003-03-13 00:00:00  2003     3    13    72     0       0       0     0     0
#> 4 2003-03-14 00:00:00  2003     3    14    73     0       0       0     0     0
#> 5 2003-03-15 00:00:00  2003     3    15    74     0       0       0     0     0
#> # ... with 124 more rows, and 15 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   resmes <dbl>, zrac <dbl>, QNplante <dbl>, azomes <dbl>, INN <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, hmax <dbl>, LRACH_1 <dbl>, LRACH_2 <dbl>,
#> #   LRACH_3 <dbl>, LRACH_4 <dbl>, cum_jul <int>
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
#> # ... with 244 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
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
#> # ... with 406 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
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
#> # A tibble: 129 x 25
#>   Date                  ian    mo    jo   jul lai_n masec_n mafruit  HR_1  HR_2
#>   <dttm>              <int> <int> <int> <int> <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1 2003-03-11 00:00:00  2003     3    11    70     0       0       0     0     0
#> 2 2003-03-12 00:00:00  2003     3    12    71     0       0       0     0     0
#> 3 2003-03-13 00:00:00  2003     3    13    72     0       0       0     0     0
#> 4 2003-03-14 00:00:00  2003     3    14    73     0       0       0     0     0
#> 5 2003-03-15 00:00:00  2003     3    15    74     0       0       0     0     0
#> # ... with 124 more rows, and 15 more variables: HR_3 <dbl>, HR_4 <dbl>,
#> #   resmes <dbl>, zrac <dbl>, QNplante <dbl>, azomes <dbl>, INN <dbl>,
#> #   fapar <dbl>, hauteur <dbl>, hmax <dbl>, LRACH_1 <dbl>, LRACH_2 <dbl>,
#> #   LRACH_3 <dbl>, LRACH_4 <dbl>, cum_jul <int>
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
#> # ... with 244 more rows, and 41 more variables: HR_3 <dbl>, HR_4 <dbl>,
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
sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = output_path,
                                     time_display = TRUE, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 18.92622 secs
```

  - Activating parallel execution

On may specify the number of cores to use with the cores argument.

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE, cores = 2, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 10.21463 secs
```

If cores is not given, parallel execution is performed over machine
total cores number minus 1.

``` r
library(parallel)

# Used cores number
detectCores() - 1
#> [1] 11

sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 7.400581 secs
```

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

The package is under intensive development, so you can fill an issue or
request us a feature
[here](https://github.com/SticsRPacks/SticsOnR/issues) at any time.

## Authors and acknowledgments

The SticsOnR package is developed by Patrice Lecharpentier, Rémi Vezy
and the [SticsOnR
Team](https://github.com/orgs/SticsRPacks/teams/sticsonr).
