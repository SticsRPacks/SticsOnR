
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

## Pre-requisities and technical tips

### JavaStics software

JavaStics must be installed and the minimal version is 1.41.

The last distribution version for Stics 9.1, JavaSTICS-1.41-stics-9.1 is
downloadable [here](https://www6.paca.inrae.fr/stics_eng/Download)).

The installation process only constists of unzipping the JavaStics
archive.

### Under linux operating systems

#### Java version

For using the JavaStics software (GUI and command line interface) under
a `linux` operating system, the java version must be at most the java 8
version.

So for recent distributions on which a higher version is installed some
manipulations can be done.

A description is given
[here](https://sticsrpacks.github.io/SticsOnR/articles/Changing_java_version_linux.html).

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
#> [1] "[18/01/20]-[18:52:30] INFO - Modulostics files generation..\n[18/01/20]-[18:52:30] INFO - Generating txt files ...\n[18/01/20]-[18:52:31] INFO - Files generated under /home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example\nFiles generated :\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/mod_bbanana.sti\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/modhistory.sti"
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
#> [1] "[18/01/20]-[18:52:31] INFO - Modulostics files generation..\n[18/01/20]-[18:52:31] INFO - Generating txt files ...\n[18/01/20]-[18:52:32] INFO - Files generated under /home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example\nFiles generated :\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/mod_bwheat.sti\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/modhistory.sti"
```

In the returned information, the error field name gives a list of
messages got from JavaStics commandline interface. If any `Error` key
word appears in a message, the corresponding simulation failed. But, at
the moment it is impossible to identify what is the the error’s origin.
Things must be checked manually in the workspace, after running again
the faulty usm (because the model input files are overwritten at each
usms simulation).

### Converting JavaStics workspace files

For using the model directly neither using the JavaStics graphical
interface nor the `run_javastics` function interface, we provide a
function, `gen_usms_xml2txt`, for converting JavasStics XML files to
Stics text files from a JavaStics workspace.

Observation files may also be copied if they have a standard name as an
usm name and a `.obs` extension. If not, they must be renamed to do so.

``` r
# Specifying the JavaStics folder
javastics_path <- "/path/to/JavaSTICS-1.41-stics-9.1"

# Specifying a workspace as a subfolder of JavaStics 
workspace_path <- "example"
# or an absolute path to an extrenal folder
# workspace_path <- "/path/to/javastics/workspace"

# Specifying an output folder path 
output_path <- "/path/to/output/folder"
```

#### Converting files into separated folders (one per usm)

``` r
# Generating files for all the usms contained in the workspace
# In the workspace
gen_usms_xml2txt(javastics_path, workspace_path)
# In a specific output folderfolder
gen_usms_xml2txt(javastics_path, workspace_path, target_path = output_path)

# Generating files for a subset of usms
# In the workspace
gen_usms_xml2txt(javastics_path, workspace_path, usms_list = c("banana","wheat"))
# In a specific folder
gen_usms_xml2txt(javastics_path, workspace_path, usms_list = c("banana","wheat"), target_path = output_path)

# Getting returned information about files generation
gen_info <- gen_usms_xml2txt(javastics_path, workspace_path, usms_list = c("banana","wheat"), target_path = output_path)

gen_info
#> $usms_paths
#> [1] "banana" "wheat" 
#> 
#> $files_path
#>  [1] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/climat.txt"     
#>  [2] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/param.sol"      
#>  [3] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/ficini.txt"     
#>  [4] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/ficplt1.txt"    
#>  [5] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/fictec1.txt"    
#>  [6] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/station.txt"    
#>  [7] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/new_travail.usm"
#>  [8] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/tempopar.sti"   
#>  [9] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/tempoparv6.sti" 
#> [10] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/ficplt2.txt"    
#> [11] "/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/fictec2.txt"    
#> 
#> $copy_status
#> [1] TRUE TRUE
#> 
#> $obs_copy_status
#> [1] TRUE TRUE
```

#### Converting files into one folder (overwriting case)

``` r
# Generating files directly in the workspace or a specific folder (no usm sub-folder) 
# In this case the model files are overwritten at each gen_usms_xml2txt call !
# In the workspace
gen_usms_xml2txt(javastics_path, workspace_path, usms_list = "banana", dir_per_usm_flag = FALSE)

# In a specific folder
gen_usms_xml2txt(javastics_path, workspace_path, usms_list = "banana", 
                 target_path = output_path, dir_per_usm_flag = FALSE)
```

### Running the model

We need for that a JavaStics folder and a directory, or a folder
containing usms subdirectories with text input files (converted from xml
JavaStics files, see previous section).

The `run_stics` function can be used as follows with one folder or
multiple sub-folders.

``` r
# Spefifying the Stics executable file path

# for windows/linux
stics_path <- file.path(javastics_path,"bin","stics_modulo")

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

### Advanced simulations paramerization

A specific function `stics_wrapper` is dedicated to manage simulations
with a higher level of parameterization than the `run_stics` function
which only executes runs.

This is a transitional version of the function that will be modified in
order to simplify parameter forcing independently from the optimization
goal, so apart from the CroptimizR package context.

This `stics_wrapper` function allows:

  - Configuring simulations run behaviour ( through an options list )
  - Parameters forcing for usms (common or specific values)
  - Returning specific outputs daily data for each usm with possible
    dates and variables filtering
  - Parallelizing simulations runs, and execution time display

Be aware that for the moment, it **is not possible** to get daily
outputs for an `inter-cropping` use case. This will be implemented in
future developments.

As the `run_stics` function, the `stics_wrapper` operates on directories
containing text stics input files.

In the next uses case, the directory in which usms sub-directories with
text input files were generated will be used.

#### Defining simulations options

The mandatory simulation options are fixed using the
`stics_wrapper_options`: the model executable path and the directory
path containing usms sub-directories with text input files.

``` r
stics_path <- file.path(javastics_path, "bin","stics_modulo")
sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path)
```

Optional fields in the `sim_options` list will be set later in this
document, when paralellized executions will be activated.

The content of the options list with default values, can be displayed
using the `stics_wrapper_options` function without any argument as
follows:

``` r
stics_wrapper_options()
#> $stics_path
#> character(0)
#> 
#> $data_dir
#> character(0)
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
observations dates are missing in simulated data. Simulation period and
output variables list may be fixed before restarting simulations.

But, for variables names, perhaps they can be badly written and do not
correspond exactly the model variables names.

#### Simulations with forcing parameters

In this case, we can define variables containing information used for
parameters forcing management. They are transmitted to `stics_wrapper`
through 2 arguments, one containing a named vector of parameters values
`param_values` and the other storing corresponding usms groups to which
each parameter value of `param_values` will be applied before running
the model.

  - Applying a single parameter values vector for all the usms

<!-- end list -->

``` r

# No group list information given
# Paratemers vector with unique values
param_values <- c(0.002,50)
names(param_values) <- c("dlaimax", "durvieF")

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = usms_list, param_values = param_values)
```

  - Using usms specific parameters values vectors

<!-- end list -->

``` r
# Defining usms groups using same parameters values
# (may be used in parameter optimization)
# Parameters vector, with parameters values per usms groups
param_values <- c(0.001, 0.002, 50, 51)
names(param_values) <- c("dlaimax", "dlaimax", "durvieF", "durvieF")

# Groups list
# parameters_data
groups_list <- list(dlaimax=list(sit_list=list(c("maize"),c("wheat", "pea"))),
                   durvieF=list(sit_list=list(c("wheat"), c("maize", "pea"))))


results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = usms_list, param_values = param_values, prior_information = groups_list)
```

For the moment, usms names must be restricted to usms names existing in
groups\_list. So usms names list must be provided as
`sit_var_dates_mask` argument value (in this case `c("wheat", "pea",
"maize")`)

#### Simulations using parallel executions

  - Getting standard simulations execution time

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path,
                                     time_display = TRUE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 21.75202 secs
```

  - Activating parallel execution and execution time display In that
    case, parallel execution is done over cores number minus 1.

<!-- end list -->

``` r
# Used cores number
detectCores() - 1
#> [1] 3

sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 12.66302 secs
```

  - Specifying cores number to use

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE, cores = 2)

results <- stics_wrapper(model_options = sim_options)
#> Time difference of 13.53429 secs
```

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Authors and acknowledgments

The SticsOnR package is developed by Patrice Lecharpentier, Rémi Vezy
and the [SticsOnR
Team](https://github.com/orgs/SticsRPacks/teams/sticsonr).
