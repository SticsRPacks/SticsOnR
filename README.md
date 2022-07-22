SticsOnR
================

The R package for the [STICS](https://www6.paca.inrae.fr/stics_eng/)
model
<img src="man/figures/logo.png" alt="logo" width="150" align="right" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/SticsRPacks/SticsOnR/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/SticsOnR/actions)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsOnR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/SticsRPacks/SticsOnR?branch=master)
[![DOI](https://zenodo.org/badge/166790380.svg)](https://zenodo.org/badge/latestdoi/166790380)
<!-- badges: end -->

The goal of SticsOnR is to perform simulations of the Stics model,
downloadable with its graphical user interface from
<https://www6.paca.inrae.fr/stics_eng/Download>.

If you want to be notified when a new release of this package is made,
you can tick the Releases box in the “Watch / Unwatch => Custom” menu at
the top right of [this page](https://github.com/SticsRPacks/SticsOnR).

## Prerequisites and technical tips

### JavaStics software

JavaStics must be installed and the minimal version is version 1.41.

The latest distribution version for Stics is downloadable
[here](https://www6.paca.inrae.fr/stics_eng/Download).

The installation process only consists of unzipping the JavaStics
archive.

### Under the Windows operating system

Be aware that the java virtual machine does not need to be installed to
use the JavaSTICS software, neither the graphical interface
(`JavaStics.exe`) nor the command line interface (`JavaSticsCmd.exe`).
Because, a Java machine is embedded in the JavaStics archive.

### Under linux operating systems

#### Java version

For using the JavaStics software (GUI and command line interface) under
a `linux` operating system, the java version must be consistent with the
JavaStics version

-   for JavaStics 1.41, java version must be at most a Java 8 version
-   for the latest JavaStics 1.5.0 version, java version must be a Java
    11 version, and only STICS version 10.0.0 can be used

So, for adapting the java version to the JavaStics version some
manipulations must be done either by switching between versions through
system commands [see
here](https://sticsrpacks.github.io/SticsOnR/articles/Changing_java_version_linux.html),
using a specific java executable path ([see running
JavaStics](#runjavasticscmd)).

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

-   For Windows RTools must be installed using the latest installer from
    [here](https://cran.r-project.org/bin/windows/Rtools)
-   For linux Development tools must be installed first, like the
    `build-essentials` package for a Debian like distribution for
    example.

Then the `devtools` package can be installed using:

``` r
install.packages("devtools")
```

For `remotes`, it can be directly installed using:

``` r
install.packages("remotes")
```

## Installation

### Recommended installation: `SticsRPacks`

The best way to install the packages from `SticsRPacks`, from which
`SticsOnR` is part of, is by installing the `[SticsRPacks]` package:

``` r
devtools::install_github("SticsRPacks/SticsRPacks")
```

-   With `remotes`

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsRPacks")
```

The package will install the packages for you at the latest release
version.

### Other way: install each package independently

#### SticsOnR

The package installation can be remotely done directly from
[GitHub](https://github.com/) using either `devtools` or the lightweight
`remote` one package

The latest release version can be installed using:

-   With `devtools`

``` r
devtools::install_github("SticsRPacks/SticsOnR@*release")
```

-   With `remotes`

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOnR@*release")
```

Normally, all the package dependencies will be installed for CRAN
packages.

#### SticsRFiles

`SticsRFiles` must be installed manually using the above syntax, just
replacing **SticsOnR** with **SticsRFiles**.

## Loading the packages library

``` r
library(SticsOnR)
library(SticsRFiles)
```

## Examples

Here are basic examples which show you how to run the model either from
a R model interface or a JavaStics (command line) one. More complete
examples will be detailed in a specific documentation later.

### Running the model using JavaStics command line interface

The JavaStics installation folder (for example,
JavaSTICS-1.5.0-STICS-10.0.0) contains an `example` workspace folder
with a set of runnable usms.

For running simulations from it, we can use the `run_javastics()`
function.

-   Specifying the JavaStics folder  
    <pre>javastics_path <- /path/to/JavaSTICS-1.5.0-STICS-10.0.0</pre>
-   Specifying a workspace
    -   as a subfolder of JavaStics  
        <pre>workspace_path <- "example"</pre>
    -   or an absolute path to an external folder  
        <pre>workspace_path <- "/path/to/javastics/workspace"</pre>

``` r
########## For Windows or linux with a compatible java version ################
# Running specific usms from the workspace
run_javastics(javastics_path, workspace_path, usm = c("banana","wheat"),
              verbose = FALSE)

# Running all usms contained in the workspace
run_javastics(javastics_path, workspace_path,
              verbose = FALSE)

# Getting information about execution:
runs_info <- run_javastics(javastics_path, workspace_path, usm = c("banana","wheat"), verbose = FALSE)

runs_info
```

-   For linux systems, if the java version is not compatible with the
    JavaStics version a compatible java executable path must be used
    <pre>java_cmd <- "/path/to/java/exe"</pre>

``` r
################ Only for linux with a specific java executable################

# Running specific usms from the workspace
run_javastics(javastics_path, workspace_path, usm = c("banana","wheat"),
              verbose = FALSE, java_cmd = java_cmd)
#> [1] "banana"
#> [1] "wheat"

# Running all usms contained in the workspace
run_javastics(javastics_path, workspace_path,
              verbose = FALSE, java_cmd = java_cmd)
#> [1] "SugarCane"
#> [1] "potato"
#> [1] "banana"
#> [1] "sorghum"
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
#> [1] "Miscanthus_2006"
#> [1] "Miscanthus_2007"
#> [1] "Miscanthus_2008"
#> [1] "Miscanthus_2009"
#> [1] "Miscanthus_2010"
#> [1] "Miscanthus_2011"
#> [1] "Miscanthus_2012"
#> [1] "Miscanthus_2013"
#> [1] "Miscanthus_2014"
#> [1] "Miscanthus_2015"

# Getting information about execution:
runs_info <- run_javastics(javastics_path, workspace_path, usm = c("banana","wheat"),
                           verbose = FALSE, java_cmd = java_cmd)
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
#> [1] "[22/07/22]-[13:34:41] INFO - Generating txt files ..."                                                                           
#> [2] "[22/07/22]-[13:34:41] INFO - Files generated under /home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/example"
#> [3] "[22/07/22]-[13:34:41] INFO - Files generated :"                                                                                  
#> [4] "[22/07/22]-[13:34:41] INFO - \t/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/example/mod_bbanana.sti"    
#> [5] "[22/07/22]-[13:34:41] INFO - \t/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/example/modhistory.sti"     
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
#> [1] "[22/07/22]-[13:34:41] INFO - Generating txt files ..."                                                                           
#> [2] "[22/07/22]-[13:34:42] INFO - Files generated under /home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/example"
#> [3] "[22/07/22]-[13:34:42] INFO - Files generated :"                                                                                  
#> [4] "[22/07/22]-[13:34:42] INFO - \t/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/example/mod_bwheat.sti"     
#> [5] "[22/07/22]-[13:34:42] INFO - \t/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/example/modhistory.sti"
```

In the returned information, the error field name gives a list of
messages from the JavaStics command line interface. If any `Error` key
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
# For Windows
# Generating files for all the usms contained in the workspace
SticsRFiles::gen_usms_xml2txt(javastics_path, workspace = workspace_path,
                              out_dir = output_path, verbose = FALSE)
```

``` r
# For linux, using a specific java executable
# Generating files for all the usms contained in the workspace
SticsRFiles::gen_usms_xml2txt(javastics_path, workspace = workspace_path, 
                              out_dir = output_path, verbose = FALSE,
                              java_cmd = java_cmd)
```

The `run_stics()` function can be used as follows with one folder or
multiple sub-folders.

The Stics executable path is set according to each operating system:

-   for windows  
    <pre>stics_path <- file.path(javastics_path,"bin","stics_modulo.exe")</pre>
-   for linux  
    <pre>stics_path <- file.path(javastics_path,"bin","stics_modulo")</pre>
-   for Mac
    <pre>stics_path <- file.path(javastics_path,"bin","stics_modulo_mac")</pre>

``` r
# Specifying a directory containing Stics input files
# For example reusing a generated sub-directory in the previous section
# of the document
# Running one usm
files_dir_path <- file.path(output_path,"banana")
run_stics(stics_path, files_dir_path)

# Specifying a root directory containing usms individual directories
# For example reusing a generated directory in the previous section
# of the document
# Running two usms
run_stics(stics_path, output_path, usm = c("banana","wheat"))

# Running all the usms defined in the sub-directories of output_path
run_stics(stics_path, output_path, usm = "all")
#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout =
#> TRUE): l'exécution de la commande ''/home/plecharpent/Work/tmp/test_SticsOnR/
#> JavaSTICS-1.5.0-STICS-10.0.0/bin/stics_modulo' 2>&1' renvoie un statut 9

# Getting returned information about stics runs
runs_info <- run_stics(stics_path, output_path, usm = c("banana","wheat"))

runs_info
#> [[1]]
#> [[1]]$name
#> [1] "banana"
#> 
#> [[1]]$error
#> [1] FALSE
#> 
#> [[1]]$message
#> [1] " numcult =            1"        " The execution was successful."
#> [3] " Duration = 0.07 s"            
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
#> [1] " numcult =            1"        " The execution was successful."
#> [3] " Duration = 0.11 s"
```

### Advanced simulations parameterization

A specific function `stics_wrapper()` is dedicated to manage simulations
with a higher level of parameterization than what `run_stics()` offers.

This `stics_wrapper()` function allows:

-   Forcing the values of a set of parameters (common or specific values
    per USM)
-   Returning simulated daily outputs for each usm with possible dates
    and variables filtering
-   Parallelizing simulations, and displaying execution time
-   Run Usms in successive mode

As the `run_stics()` function, the `stics_wrapper()` operates on
directories containing text stics input files.

#### Defining simulations options

Simulation options can be fixed using the `stics_wrapper_options()`
function. Both of them are mandatory: the model executable path and the
directory path containing usms sub-directories with text input files.

A template is returned by the function when called with no arguments:

``` r
stics_wrapper_options()
#> $javastics
#> [1] "unknown"
#> 
#> $stics_exe
#> [1] "unknown"
#> 
#> $workspace
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
#> 
#> $force
#> [1] FALSE
```

For the example, we will use the default stics model version shipping
with JavaStics and the directory where individual usms input directories
have been generated:

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path, verbose = FALSE)
```

By default, `stics_wrapper_options()` checks that `javastics`,
`stics_exe` and `workspace` exists.

There are different solutions if you need to use a custom version of
stics:

1.  if it is already listed in the preference (e.g. added in JavaStics),
    simply provide its name (ID):

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path, stics_exe = "stics_custom",
                                     workspace = output_path, verbose = FALSE)
```

2.  if it is located in the bin directory of the JavaStics installation
    directory, provide the executable name:

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path, stics_exe = "stics_custom.exe",
                                     workspace = output_path, verbose = FALSE)
```

3.  if it is located in any other folder, provide the full path to the
    executable name, and no need to use `javastics_path`:

``` r
sim_options <- stics_wrapper_options(stics_exe = "path/to/stics_custom.exe",
                                     workspace = output_path, verbose = FALSE)
```

#### Simple simulations cases

-   Without filtering usms or outputs

``` r
results <- stics_wrapper(model_options = sim_options)
```

-   Filtering on usms list

``` r
usm <- c("wheat", "maize")

results <- stics_wrapper(model_options = sim_options, situation = usm)
```

-   Filtering outputs on variables

``` r
usm <- c("wheat", "maize")

stics_wrapper(model_options = sim_options, situation = usm, var = c("masec_n","mafruit"))
```

-   Filtering outputs on variables and dates for several USMs

The argument `sit_var_dates_mask` must contain a named list (named by
usms names) containing data.frames, as the sim_list element of the list
returned by stics_wrapper (see here-after) or as observations data.

It defines a mask: stics_wrapper will return a result for each USM,
variable and date that contains at least a value (*i.e.* different from
NA) in the mask.

The stics_wrapper function returns a list that contains two elements:

-   error, a boolean indicating if an error occurs during the
    simulations,
-   sim_list, a named list of data.frames containing the simulated
    values for the requested USMs, variables and dates.

``` r
obs_list <- get_obs(workspace = workspace_path, usm = c("wheat", "maize"), verbose = FALSE)

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
#>    QNplante   Plant
#> 1      9.24 plant_1
#> 2        NA plant_1
#> 3     10.85 plant_1
#> 4     12.61 plant_1
#> 5     13.32 plant_1
#> 6     16.65 plant_1
#> 7     24.59 plant_1
#> 8     38.86 plant_1
#> 9     60.99 plant_1
#> 10    98.98 plant_1
#> 11   103.36 plant_1
#> 12   141.99 plant_1
#> 13   170.03 plant_1
#> 14       NA plant_1
#> 15   188.57 plant_1
#> 16       NA plant_1
#> 17   216.06 plant_1
#> 18       NA plant_1
#> 19   258.38 plant_1
#> 20   258.45 plant_1

sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path, verbose = TRUE)
#> ✔ Using stics: "/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/bin/stics_modulo"

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = obs_list)
#> ✔ Using stics: "/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/bin/stics_modulo"
#> Warning: Requested date(s) 1996-04-16 is(are) not simulated for USM maize
head(results)
#> $error
#> [1] TRUE
#> 
#> $sim_list
#> $maize
#>          Date   lai_n  masec_n  mafruit  AZnit_1   AZnit_2  AZnit_3  QNplante
#> 1  1996-05-14 0.00048  0.00007  0.00000 72.08275  87.81963 18.33811   0.06000
#> 2  1996-06-11 0.52827  0.34602  0.00000 41.25510 101.20524 36.68689  11.47832
#> 3  1996-06-19 1.20877  1.47023  0.00000 32.54283  92.90659 36.48689  35.17449
#> 4  1996-06-26 1.77350  2.31687  0.00000 28.37088  81.70898 35.38022  57.85850
#> 5  1996-07-02 2.45930  3.57230  0.00000 23.37861  68.11765 32.88450  82.31313
#> 6  1996-07-05 2.78487  3.95259  0.00000 16.47476  73.86992 31.59179  89.59835
#> 7  1996-07-15 4.04477  6.19013  0.00000  8.27292  54.08649 32.73149 125.06327
#> 8  1996-07-24 5.80729  9.19374  0.00000  7.73798  36.58914 29.51845 163.77878
#> 9  1996-07-25 5.80729  9.48680  0.00000  7.98975  35.55033 29.72264 167.79428
#> 10 1996-08-02 5.80729 11.66597  0.00000  8.22348  27.48982 22.84724 196.25021
#> 11 1996-08-09 5.80729 13.23097  0.00000  7.37852  20.25414 23.36597 215.35928
#> 12 1996-08-14 5.80729 14.20630  0.00000  5.79748  15.77425 20.80311 226.98665
#> 13 1996-08-20 5.80698 15.78885  0.80934  6.55601  10.70737 14.43716 245.12357
#> 14 1996-09-03 5.79139 18.41685  4.01828  4.90096   5.51325  6.09323 274.03085
#> 15 1996-09-12 5.54057 20.32297  6.55792  4.04308   3.14270  3.69530 289.17612
#> 16 1996-10-15 4.31970 23.43361 13.78295  2.21476   0.99677  1.10246 318.40991
#> 17 1996-10-23 3.54666 23.99482 14.85000  2.04174   0.75705  0.80543 322.53864
#>      Plant
#> 1  plant_1
#> 2  plant_1
#> 3  plant_1
#> 4  plant_1
#> 5  plant_1
#> 6  plant_1
#> 7  plant_1
#> 8  plant_1
#> 9  plant_1
#> 10 plant_1
#> 11 plant_1
#> 12 plant_1
#> 13 plant_1
#> 14 plant_1
#> 15 plant_1
#> 16 plant_1
#> 17 plant_1
#> 
#> $wheat
#>          Date   lai_n  masec_n     HR_1     HR_2     HR_3   resmes  AZnit_1
#> 1  1995-01-30 0.31050  0.09176 24.50000 24.50000 21.49999 192.3001  1.63252
#> 2  1995-02-03 0.31673  0.11328 24.50000 24.50000 21.49999 192.3001  1.14203
#> 3  1995-02-07 0.31881  0.12590 23.84525 24.37525 21.49292 190.8277  1.06937
#> 4  1995-02-16 0.34149  0.19068 24.50000 24.50000 21.49999 192.3001  0.67762
#> 5  1995-02-24 0.38695  0.25152 24.50000 24.50000 21.49999 192.3001 26.68379
#> 6  1995-03-06 0.48235  0.36370 24.15096 24.38058 21.49219 191.3858 20.06912
#> 7  1995-03-16 0.71067  0.55557 22.89094 23.59794 21.40839 187.0971  9.56470
#> 8  1995-03-23 0.97289  0.75267 21.60905 24.01386 21.44918 185.8467 83.16945
#> 9  1995-04-03 1.54219  1.55895 19.79812 22.46709 21.22229 178.3178 66.95890
#> 10 1995-04-11 2.10958  2.56904 15.90296 21.08201 20.84062 166.6803 55.28284
#> 11 1995-04-18 2.38348  3.15631 20.64862 19.64727 20.32267 169.8788 50.42439
#> 12 1995-04-26 3.08181  3.99189 24.50000 24.50000 21.49999 192.3001 20.96497
#> 13 1995-05-02 3.96387  4.93967 22.22493 23.73742 21.37766 186.0529 84.38062
#> 14 1995-05-05 4.54930  5.97163 18.71116 22.44751 21.10217 175.7796 74.78400
#> 15 1995-05-09 4.75734  7.29235 14.63605 20.43873 20.41607 161.1385 63.61000
#> 16 1995-05-12 4.72149  7.89513 24.50000 24.50000 21.49999 192.3001 33.59740
#> 17 1995-05-15 4.66091  8.48364 22.93547 23.99541 21.40464 187.9951 22.75400
#> 18 1995-05-19 4.58980  9.36445 23.38203 23.80817 21.37027 188.2510 12.12055
#> 19 1995-05-29 4.04065 11.57556 19.15819 20.88976 20.70820 171.5402  4.89187
#> 20 1995-07-17 0.00000 17.20260 21.79237 24.26819 21.48919 186.8909  8.02394
#>     AZnit_2  AZnit_3  QNplante   Plant
#> 1   1.49254  6.04378   6.52485 plant_1
#> 2   0.94941  5.37864   7.82038 plant_1
#> 3   0.72057  5.10246   8.70777 plant_1
#> 4   0.38353  3.78632  11.58289 plant_1
#> 5   6.86465  3.92920  15.23573 plant_1
#> 6   6.00304  3.73341  23.12093 plant_1
#> 7   4.50117  3.66756  35.95224 plant_1
#> 8   3.22272  3.89414  43.94151 plant_1
#> 9   1.79751  2.84935  64.75423 plant_1
#> 10  1.47930  2.15104  79.13458 plant_1
#> 11  1.34869  1.64058  86.14355 plant_1
#> 12 12.16368  4.01051 104.60231 plant_1
#> 13 14.85130  4.36084 123.63195 plant_1
#> 14 12.58498  4.03790 136.87631 plant_1
#> 15 10.08060  3.62519 152.44554 plant_1
#> 16 23.66376 10.33759 163.47643 plant_1
#> 17 23.05933 13.25554 172.92227 plant_1
#> 18 20.75192 17.27203 183.00761 plant_1
#> 19 16.48583 16.13381 199.50543 plant_1
#> 20 14.95036 19.86865 216.97099 plant_1
#> 
#> attr(,"class")
#> [1] "cropr_simulation"
```

Some warnings may occur, as in this case (that is why `results$error` is
TRUE), indicating that observed variables and/or observations dates are
missing in simulated data. Concerning the dates, this may be due to the
USMs simulation period that may not include observed dates. For the
variables, this may be due to an incorrect spelling of the variables in
obs_list.

#### Simulations with forcing parameters

-   Applying a single parameter values vector for all the selected usms

Parameters values are prescribed using the `param_values` argument. It
can be a named vector containing the values and names of the parameters
to force. In this case, the same values will be applied for all the
simulated usms.

``` r
param_values <- c(0.002,50)
names(param_values) <- c("dlaimax", "durvieF")

results <- stics_wrapper(model_options = sim_options, situation = usm, param_values = param_values)
```

-   Defining different parameters values depending on the usms

`param_values` can also be a data.frame or a tibble having one named
column per parameter and an optional column named `situation` containing
the name of the situations (USMs for Stics) that allows to define
different values of the parameters for different situations.

``` r
# Let's run usm wheat with c(dlaimax=0.001, durvieF=50),
# usm pea with c(dlaimax=0.001, durvieF=60),
# and usm maize with c(dlaimax=0.001, durvieF=70)
param_values <- data.frame(situation=c("wheat", "pea", "maize"),
                           dlaimax=c(0.001,0.001,0.001),
                           durvieF=c(50,60,70))

# Let's display it
param_values
#>   situation dlaimax durvieF
#> 1     wheat   0.001      50
#> 2       pea   0.001      60
#> 3     maize   0.001      70

results <- stics_wrapper(model_options = sim_options, param_values = param_values, situation = c("wheat", "maize"))
#> ✔ Using stics: "/home/plecharpent/Work/tmp/test_SticsOnR/JavaSTICS-1.5.0-STICS-10.0.0/bin/stics_modulo"
```

#### Simulations in successive mode

USMs can be run in successive mode, if they are adequately defined
(*i.e.* if the beginning and end of simulations are consistent), using
the option `successive_usms`.

`successive_usms` is a list of vectors containing the names of the UMSs
to consider as successive
(e.g. `list(c("usm1.1","usm1.2"),c("usm2.1","usm2.2"))` defines 2
successions usm1.1 -> usm1.2 and usm2.1 -> usm2.2).

#### Other Optional arguments

-   Displaying execution time

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path, time_display = TRUE,
                                     verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
```

-   Activating parallel execution

On may specify the number of cores to use with the cores argument.

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path,
                                     parallel =TRUE, time_display = TRUE,
                                     cores = 2, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
```

If cores is not given, parallel execution is performed over machine
total cores number minus 1.

``` r
library(parallel)

# Used cores number
detectCores() - 1

sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path, parallel =TRUE,
                                     time_display = TRUE, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
```

## Getting help

If you have any question or suggestion or if you want to report a bug,
please do it via the GitHub
[issues](https://github.com/SticsRPacks/SticsOnR/issues).

Thanks for that, this would greatly help us to improve this package.

## Citation

If you have used this package for a study that led to a publication or
report, please cite us. You can either use the citation tool from Github
if you used the last version, or use `citation("SticsOnR")` from R
otherwise.

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

The package is under intensive development, so you can fill an issue or
request us a feature
[here](https://github.com/SticsRPacks/SticsOnR/issues) at any time.
