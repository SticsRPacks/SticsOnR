
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
devtools::install_github("SticsRPacks/SticsOnR@*release")
```

Or using the lightweight
[remotes](https://github.com/r-lib/remotes#readme) package:

``` r
# install.packages("remotes")
remotes::install_github("SticsRPacks/SticsOnR@*release")
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

# Getting execution returned information
exec_info <- run_javastics(javastics_path, workspace_path, , usms_list = c("banana","wheat"), display = FALSE)

head(exec_info)
#> $names
#> [1] "banana" "wheat" 
#> 
#> $error
#> [1] "[09/01/20]-[18:08:05] INFO - Modulostics files generation..\n[09/01/20]-[18:08:05] INFO - Generating txt files ...\n[09/01/20]-[18:08:05] INFO - Files generated under /home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example\nFiles generated :\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/mod_bbanana.sti\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/modhistory.sti"
#> [2] "[09/01/20]-[18:08:06] INFO - Modulostics files generation..\n[09/01/20]-[18:08:06] INFO - Generating txt files ...\n[09/01/20]-[18:08:07] INFO - Files generated under /home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example\nFiles generated :\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/mod_bwheat.sti\n\t/home/plecharpent/Work/projet_tests_modulostics/JavaSTICS-v141-stics-v9.0/example/modhistory.sti"
```

### Converting JavaStics workspace files

For using the model directly neither using the JavaStics graphical
interface nor the run\_javastics function interface, we provide a
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

# Displaying returned information of head lines
head(gen_info)
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
files_dir_path <- file.path(output_path,"banana")
#run_stics(stics_path, files_dir_path)

# Specifying a root directory containing usms individual directories
# For example reusing a generated directory in the previous section
# of the document
# Running one usm
run_stics(stics_path, output_path, usm_dir_names = c("banana","wheat"))

# Running all usms sub-directories
run_stics(stics_path, output_path, usm_dir_names = "all")

# Getting returned information about stics runs
runs_info <- run_stics(stics_path, output_path, usm_dir_names = c("banana","wheat"))

head(runs_info)
#> [[1]]
#> [[1]]$name
#> [1] "banana"
#> 
#> [[1]]$error
#> [1] FALSE
#> 
#> 
#> [[2]]
#> [[2]]$name
#> [1] "wheat"
#> 
#> [[2]]$error
#> [1] FALSE
```
