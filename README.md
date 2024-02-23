SticsOnR
================

The R package for the [STICS](https://eng-stics.paca.hub.inrae.fr/)
model
<img src="man/figures/logo.png" alt="logo" width="150" align="right" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/SticsRPacks/SticsOnR/workflows/R-CMD-check/badge.svg)](https://github.com/SticsRPacks/SticsOnR/actions)
[![Codecov test
coverage](https://codecov.io/gh/SticsRPacks/SticsOnR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SticsRPacks/SticsOnR?branch=main)
[![DOI](https://zenodo.org/badge/166790380.svg)](https://zenodo.org/badge/latestdoi/166790380)
<!-- badges: end -->

The goal of SticsOnR is to perform simulations of the Stics model,
downloadable with its graphical user interface from
<https://eng-stics.paca.hub.inrae.fr/Download>.

If you want to be notified when a new release of this package is made,
you can tick the Releases box in the “Watch / Unwatch =\> Custom” menu
at the top right of [this
page](https://github.com/SticsRPacks/SticsOnR).

## Prerequisites and technical tips

### JavaStics software

JavaStics must be installed and the minimal version is version 1.41.

The latest distribution version for Stics is downloadable
[here](https://eng-stics.paca.hub.inrae.fr/Download).

The installation process only consists of unzipping the JavaStics
archive, except for MacOS system (see MacOS installation
[specificities](#under-macos-systems))

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

- for JavaStics 1.41, the java version must be at most a Java 8 version
- for JavaStics 1.5.0/1.5.1 versions, the java version must be at least
  a Java 11 version
- for JavaStics 1.5.2 version, the java version must be at least a Java
  17 version

So, for adapting the java version to the JavaStics version some
manipulations must be done either by switching between versions through
system commands [see
here](https://sticsrpacks.github.io/SticsOnR/articles/Changing_java_version_linux.html),
using a specific java executable path ([see running
JavaStics](#javastics-command-line-interface)).

#### System libraries

For the linux operating system, the SticsOnR package may require to
install an `xslt` library.

If the SticsOnR installation fails, and the `xslt` library is missing,
the error message indicates what is the name of the xslt library to be
installed (according to the common linux distributions). For example,
for the Ubuntu or Debian OS `libxslt1-dev` must be installed.

### Under MacOS systems

The STICS executable must be compiled and imported in the JavaStics
directory. The procedure is fully described in the JavaStics
documentation (see JavaStics_documentation.html under the doc folder),
in the prerequisites sub-section under the Software section. Java
version installation management is also described in it.

### Files/directories paths syntax

Under all systems, file paths must not contain any special character or
space.

Under unix like systems, using the `~` in files or directories paths may
cause errors in SticsOnR functions. So, it is safer for the moment to
use absolute paths. This will be fixed in the future versions.

#### Remote installation tools

For installing packages from the Github site an additional package must
be installed. One can use either `devtools` or
[`remotes`](https://github.com/r-lib/remotes#readme)

For `devtools`, installation requires system dependent tools. They must
be installed first.

- For Windows RTools must be installed using the latest installer from
  [here](https://cran.r-project.org/bin/windows/Rtools/)
- For linux Development tools must be installed first, like the
  `build-essentials` package for a Debian like distribution for example.

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

- With `remotes`

``` r

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

- With `devtools`

``` r
devtools::install_github("SticsRPacks/SticsOnR@*release")
```

- With `remotes`

``` r

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

## Running the model

Here are basic examples which show you how to run the model either from
a R model interface or a JavaStics (command line) one. More complete
examples will be detailed in a specific documentation later.

### JavaStics command line interface

The JavaStics installation folder (for example,
JavaSTICS-1.5.2-STICS-10.1.0) contains an `example` workspace folder
with a set of runnable usms.

For running simulations from it, we can use the `run_javastics()`
function.

- Specifying the JavaStics folder  
  <pre>javastics_path <- /path/to/JavaSTICS-1.5.2-STICS-10.1.0</pre>
- Specifying a workspace
  - as a sub-folder of JavaStics  
    <pre>workspace_path <- "example"</pre>
  - or an absolute path to an external folder  
    <pre>workspace_path <- "/path/to/javastics/workspace"</pre>

``` r
########## For Windows or linux with a compatible java version ################
# Running specific usms from the workspace
run_javastics(javastics_path, workspace_path, usm = c("banana", "wheat"),
              verbose = FALSE)

# Running all usms contained in the workspace
run_javastics(javastics_path, workspace_path,
              verbose = FALSE)

# Getting information about execution:
runs_info <- run_javastics(javastics_path, workspace_path,
                           usm = c("banana", "wheat"), verbose = FALSE)

runs_info
```

- For linux systems, if the java version is not compatible with the
  JavaStics version a compatible java executable path must be used
  <pre>java_cmd <- "/path/to/java/exe"</pre>

``` r
################ Only for linux with a specific java executable################

# Running specific usms from the workspace
run_javastics(javastics_path, workspace_path, usm = c("banana", "wheat"),
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
runs_info <- run_javastics(javastics_path, workspace_path,
                           usm = c("banana", "wheat"),
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
#> [1] "[20/02/24]-[11:29:25] INFO - Files generated :"                                                                       
#> [2] "[20/02/24]-[11:29:25] INFO - \t/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/example/mod_bbanana.sti"
#> [3] "[20/02/24]-[11:29:25] INFO - \t/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/example/modhistory.sti" 
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
#> [1] "[20/02/24]-[11:29:25] INFO - Files generated :"                                                                      
#> [2] "[20/02/24]-[11:29:25] INFO - \t/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/example/mod_bwheat.sti"
#> [3] "[20/02/24]-[11:29:25] INFO - \t/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/example/modhistory.sti"
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

- for windows  
  <pre>stics_path <- file.path(javastics_path,"bin","stics_modulo.exe")</pre>
- for linux  
  <pre>stics_path <- file.path(javastics_path,"bin","stics_modulo")</pre>
- for Mac
  <pre>stics_path <- file.path(javastics_path,"bin","stics_modulo_mac")</pre>

``` r
# Specifying a directory containing Stics input files
# For example reusing a generated sub-directory in the previous section
# of the document
# Running one usm
files_dir_path <- file.path(output_path, "banana")
run_stics(stics_path, files_dir_path)

# Specifying a root directory containing usms individual directories
# For example reusing a generated directory in the previous section
# of the document
# Running two usms
run_stics(stics_path, output_path, usm = c("banana", "wheat"))

# Running all the usms defined in the sub-directories of output_path
run_stics(stics_path, output_path, usm = "all")

# Getting returned information about stics runs
runs_info <- run_stics(stics_path, output_path, usm = c("banana", "wheat"))

runs_info
#> [[1]]
#> [[1]]$name
#> [1] "banana"
#> 
#> [[1]]$error
#> [1] FALSE
#> 
#> [[1]]$message
#> [1] " The execution was successful." " Duration = 56ms"              
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
#> [1] " The execution was successful." " Duration = 86ms"
```

### Advanced simulations parameterization

A specific function `stics_wrapper()` is dedicated to manage simulations
with a higher level of parameterization than what `run_stics()` offers.

This `stics_wrapper()` function allows:

- Forcing the values of a set of parameters (common or specific values
  per USM)
- Returning simulated daily outputs for each usm with possible dates and
  variables filtering
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
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     stics_exe = "stics_custom",
                                     workspace = output_path, verbose = FALSE)
```

2.  if it is located in the bin directory of the JavaStics installation
    directory, provide the executable name:

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     stics_exe = "stics_custom.exe",
                                     workspace = output_path, verbose = FALSE)
```

3.  if it is located in any other folder, provide the full path to the
    executable name, and no need to use `javastics_path`:

``` r
sim_options <- stics_wrapper_options(stics_exe = "path/to/stics_custom.exe",
                                     workspace = output_path, verbose = FALSE)
```

#### Simple simulations cases

- Without filtering usms or outputs

``` r
results <- stics_wrapper(model_options = sim_options)
```

- Filtering on usms list

``` r
usm <- c("wheat", "maize")

results <- stics_wrapper(model_options = sim_options, situation = usm)
```

- Filtering outputs on variables

``` r
usm <- c("wheat", "maize")

stics_wrapper(model_options = sim_options, situation = usm,
              var = c("masec_n", "mafruit"))
```

- Filtering outputs on variables and dates for several USMs

The argument `sit_var_dates_mask` must contain a named list (named by
usms names) containing data.frames, as the sim_list element of the list
returned by stics_wrapper (see here-after) or as observations data.

It defines a mask: stics_wrapper will return a result for each USM,
variable and date that contains at least a value (*i.e.* different from
NA) in the mask.

The stics_wrapper function returns a list that contains two elements:

- error, a boolean indicating if an error occurs during the simulations,
- sim_list, a named list of data.frames containing the simulated values
  for the requested USMs, variables and dates.

``` r
obs_list <- get_obs(workspace = workspace_path, usm = c("wheat", "maize"),
                    verbose = FALSE)

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
#> ✔ Using stics: "/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/bin/stics_modulo"

results <- stics_wrapper(model_options = sim_options,
                         sit_var_dates_mask = obs_list)
#> ✔ Using stics: "/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/bin/stics_modulo"
#> Warning: Requested date(s) 1996-04-16 is(are) not simulated for USM maize
head(results)
#> $error
#> [1] TRUE
#> 
#> $sim_list
#> $maize
#>          Date   lai_n  masec_n  mafruit  AZnit_1   AZnit_2  AZnit_3  QNplante
#> 1  1996-05-14 0.00048  0.00007  0.00000 72.08270  87.81961 18.33810   0.06000
#> 2  1996-06-11 0.52827  0.34602  0.00000 41.25511 101.20517 36.68687  11.47820
#> 3  1996-06-19 1.20877  1.47023  0.00000 32.54288  92.90659 36.48687  35.17423
#> 4  1996-06-26 1.77350  2.31686  0.00000 28.37093  81.70900 35.38020  57.85825
#> 5  1996-07-02 2.45930  3.57229  0.00000 23.37866  68.11768 32.88448  82.31290
#> 6  1996-07-05 2.78486  3.95258  0.00000 16.47478  73.86994 31.59177  89.59812
#> 7  1996-07-15 4.04477  6.19012  0.00000  8.27292  54.08646 32.73148 125.06309
#> 8  1996-07-24 5.80729  9.19373  0.00000  7.73798  36.58909 29.51844 163.77870
#> 9  1996-07-25 5.80729  9.48679  0.00000  7.98975  35.55027 29.72262 167.79420
#> 10 1996-08-02 5.80729 11.66596  0.00000  8.22348  27.48977 22.84721 196.25015
#> 11 1996-08-09 5.80729 13.23096  0.00000  7.37852  20.25411 23.36592 215.35924
#> 12 1996-08-14 5.80729 14.20629  0.00000  5.79748  15.77424 20.80305 226.98662
#> 13 1996-08-20 5.80698 15.78884  0.80934  6.55602  10.70735 14.43708 245.12354
#> 14 1996-09-03 5.79138 18.41684  4.01828  4.90096   5.51324  6.09316 274.03082
#> 15 1996-09-12 5.54056 20.32296  6.55791  4.04308   3.14270  3.69524 289.17609
#> 16 1996-10-15 4.31971 23.43360 13.78294  2.21475   0.99676  1.10243 318.40985
#> 17 1996-10-23 3.54667 23.99480 14.85000  2.04173   0.75705  0.80540 322.53854
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
#> 1  1995-01-30 0.31050  0.09176 24.50000 24.50000 21.49999 192.3001  1.07899
#> 2  1995-02-03 0.31650  0.11303 24.50000 24.50000 21.49999 192.3001  0.76862
#> 3  1995-02-07 0.31678  0.12561 23.83189 24.38442 21.49393 190.8273  0.60993
#> 4  1995-02-16 0.32971  0.18710 24.50000 24.50000 21.49999 192.3001  0.56298
#> 5  1995-02-24 0.36057  0.24471 24.50000 24.50000 21.49999 192.3001 26.17110
#> 6  1995-03-06 0.44779  0.35035 24.14443 24.38385 21.49153 191.3779 19.42991
#> 7  1995-03-16 0.65951  0.52936 22.92095 23.63181 21.38363 187.1108  9.46765
#> 8  1995-03-23 0.93227  0.71936 21.66335 24.01440 21.42110 185.8192 83.92502
#> 9  1995-04-03 1.57855  1.55586 20.56154 22.34904 20.97554 178.3337 64.59159
#> 10 1995-04-11 2.09987  2.55337 17.92109 20.99722 20.03741 166.5205 52.84023
#> 11 1995-04-18 2.35501  3.15292 23.58026 19.75672 19.03260 169.5803 46.57108
#> 12 1995-04-26 3.14867  3.99308 24.50000 24.50000 21.49999 192.3001 15.92698
#> 13 1995-05-02 4.00809  4.94513 22.89795 23.84206 21.11930 186.3215 79.20723
#> 14 1995-05-05 4.66786  6.02794 20.59809 22.80039 20.38152 176.6742 69.22348
#> 15 1995-05-09 4.84330  7.28487 18.33193 21.14837 19.03372 163.0608 59.09080
#> 16 1995-05-12 4.79468  7.88745 24.50000 24.50000 21.49999 192.3001 26.11957
#> 17 1995-05-15 4.74392  8.47785 23.56760 24.06944 21.22628 188.4858 18.00424
#> 18 1995-05-19 4.57415  9.35893 23.88076 24.22463 21.14586 189.0135  9.34575
#> 19 1995-05-29 3.97903 11.72744 23.74543 21.45070 19.37518 174.9766  3.42815
#> 20 1995-07-17 0.00000 15.87414 21.79237 24.26819 21.48919 186.8909  7.14489
#>     AZnit_2 AZnit_3  QNplante   Plant
#> 1   1.48309 5.94223   6.41090 plant_1
#> 2   1.02958 5.36714   7.28596 plant_1
#> 3   0.77065 5.15434   8.12731 plant_1
#> 4   0.46101 4.17630  10.04601 plant_1
#> 5   6.90490 4.46663  13.25611 plant_1
#> 6   6.11377 4.21979  20.68182 plant_1
#> 7   4.74362 3.64634  32.71018 plant_1
#> 8   3.00263 3.02404  41.23375 plant_1
#> 9   0.56898 0.83703  65.91978 plant_1
#> 10  0.34906 0.21218  79.40999 plant_1
#> 11  0.37182 0.06465  87.25914 plant_1
#> 12 10.59079 3.15716 106.08575 plant_1
#> 13 12.77094 1.92864 126.41373 plant_1
#> 14  9.40576 0.93281 140.18689 plant_1
#> 15  6.08418 0.38794 153.95473 plant_1
#> 16 19.42081 8.97776 165.35764 plant_1
#> 17 16.99088 8.49830 176.44348 plant_1
#> 18 12.89605 7.68171 190.11299 plant_1
#> 19  4.10260 1.08262 211.33589 plant_1
#> 20  6.79536 2.67405 222.61639 plant_1
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

- Applying a single parameter values vector for all the selected usms

Parameters values are prescribed using the `param_values` argument. It
can be a named vector containing the values and names of the parameters
to force. In this case, the same values will be applied for all the
simulated usms.

``` r
param_values <- c(0.002, 50)
names(param_values) <- c("dlaimax", "durvieF")

results <- stics_wrapper(model_options = sim_options, situation = usm,
                         param_values = param_values)
```

- Defining different parameters values depending on the usms

`param_values` can also be a data.frame or a tibble having one named
column per parameter and an optional column named `situation` containing
the name of the situations (USMs for Stics) that allows to define
different values of the parameters for different situations.

``` r

# Let's run usm wheat with c(dlaimax=0.001, durvieF=50),
# usm pea with c(dlaimax=0.001, durvieF=60),
# and usm maize with c(dlaimax=0.001, durvieF=70)
param_values <- data.frame(situation = c("wheat", "pea", "maize"),
                           dlaimax = c(0.001, 0.001, 0.001),
                           durvieF = c(50, 60, 70))

# Let's display it
param_values
#>   situation dlaimax durvieF
#> 1     wheat   0.001      50
#> 2       pea   0.001      60
#> 3     maize   0.001      70

results <- stics_wrapper(model_options = sim_options,
                         param_values = param_values,
                         situation = c("wheat", "maize"))
#> ✔ Using stics: "/home/plecharpent/Téléchargements/JavaSTICS-1.5.2-STICS-10.1.0/bin/stics_modulo"
```

#### Simulations in successive mode

USMs can be run in successive mode, if they are adequately defined
(*i.e.* if the beginning and end of simulations are consistent), using
the option `successive_usms`.

`successive_usms` is a list of vectors containing the names of the UMSs
to consider as successive
(e.g. `list(c("usm1.1","usm1.2"),c("usm2.1","usm2.2"))` defines 2
successions usm1.1 -\> usm1.2 and usm2.1 -\> usm2.2).

#### Other Optional arguments

- Displaying execution time

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path,
                                     time_display = TRUE,
                                     verbose = FALSE)

results <- stics_wrapper(model_options = sim_options)
```

- Activating parallel execution

On may specify the number of cores to use with the cores argument.

``` r
sim_options <- stics_wrapper_options(javastics = javastics_path,
                                     workspace = output_path,
                                     parallel = TRUE, time_display = TRUE,
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
                                     workspace = output_path, parallel = TRUE,
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
