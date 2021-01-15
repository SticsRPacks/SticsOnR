
``` r
library(SticsOnR)
#> Learn SticsOnR at: https://SticsRPacks.github.io/SticsOnR
library(SticsRFiles)
#> Learn SticsRFiles at: https://SticsRPacks.github.io/SticsRFiles
```

# SticsOnR: The R package for the [STICS](https://www6.paca.inrae.fr/stics_eng/) model <img src="man/figures/logo.png" alt="logo" width="150" align="right" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

[![R build
status](https://github.com/SticsRPacks/SticsOnR/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/SticsOnR/actions)

[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsOnR/branch/master/graph/badge.svg)](https://codecov.io/gh/SticsRPacks/SticsOnR?branch=master)
<!-- badges: end -->

[![DOI](https://zenodo.org/badge/166790380.svg)](https://zenodo.org/badge/latestdoi/166790380)
<!-- badges: end -->

The goal of SticsOnR is to perform simulations of the Stics model,
downloadable with its graphical user interface from
<https://www6.paca.inra.fr/stics_eng/Download>.

If you want to be notified when a new release of this package is made,
you can tick the Releases box in the “Watch / Unwatch =\> Custom” menu
at the top right of [this
page](https://github.com/SticsRPacks/SticsOnR).

## Prerequisites and technical tips

### JavaStics software

JavaStics must be installed and the minimal version is version 1.41.

The last distribution version for Stics is downloadable
[here](https://www6.paca.inrae.fr/stics_eng/Download).

The installation process only constists of unzipping the JavaStics
archive.

### Under linux operating systems

#### Java version

For using the JavaStics software (GUI and command line interface) under
a `linux` operating system, the java version must be at most the Java 8
version.

For recent distributions on which a higher version is installed some
manipulations have to be done.

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
#> v All usms ran successfully!

# Running all usms contained in the workspace
run_javastics(javastics_path, workspace_path)
#> v Using stics 'modulostics' (exe: 'stics_modulo.exe')
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
#> v All usms ran successfully!

# Getting information about execution:
runs_info <- run_javastics(javastics_path, workspace_path, usms_list = c("banana","wheat"))
#> v Using stics 'modulostics' (exe: 'stics_modulo.exe')
#> [1] "banana"
#> [1] "wheat"
#> v All usms ran successfully!

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
# Running one usm
files_dir_path <- file.path(output_path,"banana")
run_stics(stics_path, files_dir_path)

# Specifying a root directory containing usms individual directories
# For example reusing a generated directory in the previous section
# of the document
# Running two usms
run_stics(stics_path, output_path, usm_dir_names = c("banana","wheat"))

# Running all the usms defined in the sub-directories of output_path
run_stics(stics_path, output_path, usm_dir_names = "all")
#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

#> Warning in system2(command = command, args = com_args, stderr = TRUE, stdout
#> = TRUE): l'exécution de la commande '"D:/Home/sbuis/Documents/WORK/STICS/
#> JavaSTICS-1.41-stics-9.0/bin/stics_modulo" ' renvoie un statut 9

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

A specific function `stics_wrapper()` is dedicated to manage simulations
with a higher level of parameterization than what `run_stics()` offers.

This `stics_wrapper()` function allows:

  - Forcing the values of a set of parameters (common or specific values
    per USM)
  - Returning simulated daily outputs for each usm with possible dates
    and variables filtering
  - Parallelizing simulations, and displaying execution time
  - Run Usms in successive mode

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

1.  if it is already listed in the preference (e.g. added in JavaStics),
    simply provide its name (ID):

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path, stics_exe = "stics_custom",
                                     data_dir = output_path, verbose = FALSE)
```

1.  if it is located in the bin directory of the JavaStics installation
    directory, provide the executable name:

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path, stics_exe = "stics_custom.exe",
                                     data_dir = output_path, verbose = FALSE)
```

1.  if it is located in any other folder, provide the full path to the
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

results <- stics_wrapper(model_options = sim_options, sit_names = usms_list)
```

  - Filtering outputs on variables

<!-- end list -->

``` r
usms_list <- c("wheat", "pea", "maize")

stics_wrapper(model_options = sim_options, sit_names = usms_list, var_names = c("masec_n","mafruit"))
```

  - Filtering outputs on variables and dates for several USMs

The argument `sit_var_dates_mask` must contain a named list (named by
usms names) containing data.frames, as the sim\_list element of the list
returned by stics\_wrapper (see here-after) or as observations data.

It defines a mask: stics\_wrapper will return a result for each USM,
variable and date that contains at least a value (i.e. different from
NA) in the mask.

The stics\_wrapper function returns a list that contains two elements:
\* error, a boolean indicating if an error occurs during the
simulations, \* sim\_list, a named list of data.frames containing the
simulated values for the requested USMS, variables and dates.

``` r
obs_list <- get_obs(workspace = workspace_path, usm_name = c("wheat", "maize"), verbose = FALSE)

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

sim_options <- stics_wrapper_options(javastics_path = javastics_path,
                                     data_dir = output_path, verbose = TRUE)
#> v Using stics: 'D:/OneDrive - cirad.fr/Travail_Postdoc/SticsRPacks/JavaSTICS-1.41-stics-9.1/bin/stics_modulo.exe'

results <- stics_wrapper(model_options = sim_options, sit_var_dates_mask = obs_list)
#> v Using stics: 'D:/OneDrive - cirad.fr/Travail_Postdoc/SticsRPacks/JavaSTICS-1.41-stics-9.1/bin/stics_modulo.exe'
#> Warning in e$fun(obj, substitute(ex), parent.frame(), e$data): already exporting
#> variable(s): force_param_values
#> Warning: Requested date(s) 1996-04-16 is(are) not simulated for USM maize
head(results)
#> $error
#> [1] TRUE
#>
#> $sim_list
#> $maize
#>          Date   lai_n  masec_n  mafruit  AZnit_1   AZnit_2  AZnit_3  QNplante
#> 1  1996-05-14 0.00048  0.00007  0.00000 69.16374  90.25692 18.71289   0.06000
#> 2  1996-06-11 0.52827  0.34602  0.00000 39.69806 101.53232 37.54969  11.39259
#> 3  1996-06-19 1.20459  1.46155  0.00000 31.19296  93.05993 37.34723  34.81186
#> 4  1996-06-26 1.76653  2.30258  0.00000 27.14680  81.66067 36.21786  57.46828
#> 5  1996-07-02 2.45174  3.55522  0.00000 22.45446  67.96534 33.68311  81.82993
#> 6  1996-07-05 2.77730  3.93519  0.00000 16.32175  72.93639 32.37175  89.12132
#> 7  1996-07-15 4.03727  6.17194  0.00000  8.38111  52.82459 33.41626 124.54063
#> 8  1996-07-24 5.79979  9.17521  0.00000  7.94482  35.38852 29.95074 163.16862
#> 9  1996-07-25 5.79979  9.46826  0.00000  8.20963  34.35831 30.10686 167.18677
#> 10 1996-08-02 5.79979 11.64728  0.00000  8.44133  26.28358 23.05334 195.64365
#> 11 1996-08-09 5.79979 13.21217  0.00000  7.55000  19.25364 23.24305 214.74731
#> 12 1996-08-14 5.79979 14.18743  0.00000  5.91982  14.87446 20.48055 226.37970
#> 13 1996-08-20 5.79940 15.76523  0.80813  6.69296   9.75431 14.13963 244.41234
#> 14 1996-09-03 5.77766 18.39315  4.01311  4.79599   4.50623  5.81929 273.33353
#> 15 1996-09-12 5.46220 20.29901  6.55019  4.05262   2.43576  3.54460 287.78687
#> 16 1996-10-15 4.25747 23.41053 13.76937  1.97948   0.66061  0.96324 316.49945
#> 17 1996-10-23 3.54677 23.97131 14.85000  1.94655   0.58142  0.71932 320.17722
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
#>          Date lai_n masec_n HR_1 HR_2 HR_3 resmes AZnit_1 AZnit_2 AZnit_3
#> 1  1995-01-30 0.705   0.158 24.5 24.5 21.5    192   0.391   0.444   3.250
#> 2  1995-02-03 0.777   0.227 24.5 24.5 21.5    192   0.408   0.378   2.670
#> 3  1995-02-07 0.849   0.270 23.9 24.4 21.5    191   0.389   0.319   2.400
#> 4  1995-02-16 1.080   0.496 24.5 24.5 21.5    192   0.521   0.297   1.540
#> 5  1995-02-24 1.320   0.739 24.5 24.5 21.5    192  23.700   6.400   2.080
#> 6  1995-03-06 1.880   1.340 24.3 24.4 21.5    192   8.800   4.830   1.640
#> 7  1995-03-16 2.370   2.120 23.4 23.2 21.3    187   2.020   3.940   1.580
#> 8  1995-03-23 2.570   2.640 22.1 23.8 21.4    186  75.900   2.940   1.900
#> 9  1995-04-03 2.990   3.750 19.7 21.8 21.0    176  62.300   2.640   1.400
#> 10 1995-04-11 3.250   5.120 14.7 20.2 20.4    161  47.100   2.500   1.100
#> 11 1995-04-18 3.000   5.830 19.7 18.9 19.6    163  42.500   2.440   0.901
#> 12 1995-04-26 2.470   6.660 24.5 24.5 21.3    191  18.800  11.500   2.790
#> 13 1995-05-02 2.060   7.430 21.7 23.8 21.4    185  84.400  14.900   3.320
#> 14 1995-05-05 1.920   8.260 18.3 23.0 21.2    177  75.600  13.000   3.110
#> 15 1995-05-09 1.680   9.240 14.9 21.9 20.8    166  67.300  11.100   2.840
#> 16 1995-05-12 1.450   9.630 24.5 24.5 21.5    192  38.500  25.700  11.300
#> 17 1995-05-15 1.360   9.970 22.4 24.1 21.4    187  29.100  26.500  14.700
#> 18 1995-05-19 1.130  10.500 22.9 23.9 21.4    188  18.700  25.800  18.900
#> 19 1995-05-29 0.512  11.700 18.5 22.7 21.1    176   9.880  21.500  17.700
#> 20 1995-07-17 0.000  11.700 21.8 24.3 21.5    187  11.700  19.500  30.200
#>    QNplante   Plant
#> 1      5.61 plant_1
#> 2      6.26 plant_1
#> 3      6.91 plant_1
#> 4      8.65 plant_1
#> 5     15.20 plant_1
#> 6     33.60 plant_1
#> 7     43.10 plant_1
#> 8     46.70 plant_1
#> 9     63.50 plant_1
#> 10    80.80 plant_1
#> 11    87.10 plant_1
#> 12   102.00 plant_1
#> 13   117.00 plant_1
#> 14   129.00 plant_1
#> 15   141.00 plant_1
#> 16   148.00 plant_1
#> 17   154.00 plant_1
#> 18   162.00 plant_1
#> 19   180.00 plant_1
#> 20   182.00 plant_1
#>
#> attr(,"class")
#> [1] "cropr_simulation"
```

Some warnings may occur, as in this case (that is why `results$error` is
TRUE), indicating that observed variables and/or observations dates are
missing in simulated data. Concerning the dates, this may be due to the
USMs simulation period that may not include observed dates. For the
variables, this may be due to an incorrect spelling of the variables in
obs\_list.

#### Simulations with forcing parameters

  - Applying a single parameter values vector for all the selected usms

Parameters values are prescribed using the `param_values` argument. It
can be a named vector containing the values and names of the parameters
to force. In this case, the same values will be applied for all the
simulated usms.

``` r
param_values <- c(0.002,50)
names(param_values) <- c("dlaimax", "durvieF")

results <- stics_wrapper(model_options = sim_options, sit_names = usms_list, param_values = param_values)
```

  - Defining different parameters values depending on the usms

`param_values` can also be a data.frame or a tibble having one named
column per parameter and an optional column named Situation containing
the name of the situations (USMs for Stics) that allows to define
different values of the parameters for different situations.

``` r

# Let's run usm wheat with c(dlaimax=0.001, durvieF=50),
# usm pea with c(dlaimax=0.001, durvieF=60),
# and usm maize with c(dlaimax=0.001, durvieF=70)
param_values <- data.frame(Situation=c("wheat", "pea", "maize"),
                           dlaimax=c(0.001,0.001,0.001),
                           durvieF=c(50,60,70))

# Let's display it
param_values
#>   Situation dlaimax durvieF
#> 1     wheat   0.001      50
#> 2       pea   0.001      60
#> 3     maize   0.001      70

results <- stics_wrapper(model_options = sim_options, param_values = param_values, sit_names=c("wheat", "pea", "maize"))
#> v Using stics: 'D:/OneDrive - cirad.fr/Travail_Postdoc/SticsRPacks/JavaSTICS-1.41-stics-9.1/bin/stics_modulo.exe'
```

#### Simulations in successive mode

USMs can be run in successive mode, if they are adequately defined
(i.e. if the beginning and end of simulations are consistent), using
the option `successive_usms`.

`successive_usms` is a list of vectors containing the names of the UMSs
to consider as successive
(e.g. `list(c("usm1.1","usm1.2"),c("usm2.1","usm2.2"))` defines 2
successions usm1.1-\>usm1.2 and usm2.1-\>usm2.2).

#### Other Optional arguments

  - Displaying execution time

<!-- end list -->

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path,
                                     data_dir = output_path, time_display = TRUE,
                                     verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
#> Warning in e$fun(obj, substitute(ex), parent.frame(), e$data): already exporting
#> variable(s): force_param_values
#> Time difference of 26.52565 secs
```

  - Activating parallel execution

On may specify the number of cores to use with the cores argument.

``` r
sim_options <- stics_wrapper_options(javastics_path = javastics_path,
                                     data_dir = output_path,
                                     parallel =TRUE, time_display = TRUE,
                                     cores = 2, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
#> Warning in e$fun(obj, substitute(ex), parent.frame(), e$data): already exporting
#> variable(s): force_param_values
#> Time difference of 15.16197 secs
```

If cores is not given, parallel execution is performed over machine
total cores number minus 1.

``` r
library(parallel)

# Used cores number
detectCores() - 1
#> [1] 3

sim_options <- stics_wrapper_options(javastics_path = javastics_path,
                                     data_dir = output_path, parallel =TRUE,
                                     time_display = TRUE, verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
#> Warning in e$fun(obj, substitute(ex), parent.frame(), e$data): already exporting
#> variable(s): force_param_values
#> Time difference of 9.150996 secs
```

## Citation

If you have used this package for a study that led to a publication or
report, please cite us. To get the suggested citation, run
`citation("SticsOnR")`.

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
