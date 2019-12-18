
# SticsOnR: The R package for the [STICS](https://www6.paca.inra.fr/stics_eng/) model <img src="man/figures/logo.png" alt="logo" width="150" align="right" />

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

## Installation

The development version from [GitHub](https://github.com/) can be
installed with:

``` r
devtools::install_github("SticsRPacks/SticsOnR")
```

Or using the lightweight
[remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOnR")
```

## Examples

Here are basic examples which show you how to run the model either from
a R model interface or a JavaStics (command line) one. More complete
examples will be detailed in a specific documentation soon.

### Running the JavaStics command line interface

We need for that a JavaStics folder and a JavaStics workspace folder.
For example using the last distribution version for Stics 9.1
(JavaSTICS-1.41-stics-9.1).  
It contains an `example` folder with a set of runnable usms.

For running simulations from it, we can use the `run_javastics` function
as follows:

``` r
# Spefifying the JavaStics folder
javastics_path <- "/path/to/JavaSTICS-1.41-stics-9.1"
# specifying a workspace as a subfolder of JavaStics 
workspace_path <- "example"
# or an absolute path to an extrenal folder
# workspace_path <- "/path/to/javastics/workspace"

# Running all usms contained in the workspace 
run_javatics(javastics_path, workspace_path)
```

### Converting JavaStics workspace files

For using the model directly neither using the JavaStics graphical
interface nor the run\_javastics function interface, we provide a
function, `gen_usms_xml2txt`, for converting JavasStics XML files to
Stics text files from a JavaStics workspace.

Observation files may also be copied if they have a standard name as an
usm name and a `.obs` extension. If not, they must be renamed to do so.

``` r
# Spefifying the JavaStics folder
javastics_path <- "/path/to/JavaSTICS-1.41-stics-9.1"
# specifying a workspace as a subfolder of JavaStics 
workspace_path <- "example"
# or an absolute path to an extrenal folder
# workspace_path <- "/path/to/javastics/workspace"

# Generating files for all the usms contained in the workspace
# one directory per usm
# In the workspace
gen_usms_xml2txt(javastics_path, workspace_path)
# In a specific folder
gen_usms_xml2txt(javastics_path, workspace_path, target_path = "/path/to/output/folder")

# Generating files for a subset of usms
# In the workspace
gen_usms_xml2txt(javastics_path, workspace_path, usms_list = c("banana","wheat"))
# In a specific folder
gen_usms_xml2txt(javastics_path, workspace_path, usms_list = c("banana","wheat"), target_path = "/path/to/output/folder")

# Generating files directly in the workspace or a specific folder (no usm sub-folder) 
# In the workspace
gen_usms_xml2txt(javastics_path, workspace_path, dir_per_usm_flag = FALSE)
# In a specific folder
gen_usms_xml2txt(javastics_path, workspace_path, target_path = "/path/to/output/folder", dir_per_usm_flag = FALSE)
```

### Running the model

We need for that a JavaStics folder and a directory, or a folder
containing usms subdirectories with text input files (converted from xml
JavaStics files).

The `run_stics` function can be used as follows with one folder or
multiple sub-folders.

``` r
# Spefifying the Stics executable file path
# for windows/linux
stics_path <- "/path/to/JavaSTICS-1.41-stics-9.1/stics_modulo"
# for Mac
# stics_path <- "/path/to/JavaSTICS-1.41-stics-9.1/stics_modulo_mac

# Specifying a directory containing Stics input files  
files_dir_path <- "/path/to/files/dir"
run_stics(stics_path, files_dir_path)
# or a root directory containing usms indicidual directories
# files_root_dirs_path <- "/path/to/files/dirs/root"
run_stics(stics_path, files_root_dirs_path)
```

-----
