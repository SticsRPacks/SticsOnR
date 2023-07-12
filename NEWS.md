# SticsOnR 1.1.0 _2023-07-12_

MAIN CHANGES:
  * Added a valid license (LGPL)
  * According to SticsRFiles changes 
  * Using CRAN SticsRFiles package (1.1.3)

FIXES:
  * Removed useless functions provided by SticsRFiles 1.1.3
  * Removed useless files and function documentation
  * linting issues


# SticsOnR 1.0.1  _2023-01-03_

* Fixed cff file generation using github action. Now generated on each commit that modifies the Description file + on release.
* Minor changes following CRAN checks procedure application.

# SticsOnR 1.0.0  _2022-07-22_

MAIN CHANGES:
  * run_javastics: new argument for giving the path of the java version to use
  * stics_wrapper: 
    * set "variete" to be forced first if included in param_values argument (so that varietal parameters are well handled if included in param_values)
    * using () in names of variables (var argument) is now taken into account
  * homogenization of argument names between functions
  * some functions moved to the SticsRFiles package
  * simulated phenological stages now take the same value of julian day whatever the date is

FIXES:
  * stics_wrapper: run successive USMs did not work in case required variables (var argument) were not defined in var.mod
  * run_stics: running automatically usms stored in subdirectories of workspace did not work
  * run_javastics: 
    * STICS execution errors now captured
    * relative path of workspace now correctly taken into account
  
  

# SticsOnR 0.2.2  _2021-06-17_

## Changes

* Improved the way stics output files are read in stics_wrapper
* Minor change in run_javastics and stics_wrapper doc
* FIX: on.exit was badly placed in stics_wrapper, sometimes lead to ugly error messages.

# SticsOnR 0.2.1 _2021-03-12_

## General changes

* A few bugs fixed 

## Specific changes

R/run_system_cmd.R
 * FIX: relative path for stics executable were not well handled
 
R/stics_wrapper.R
 * FIX: situation was not correctly spelled in doc ... (2021-02-24)
 * FIX: incorrect test on param_values in case parameters take different values for different USMs (2021-02-24)
 * FIX: searching if the OS specific stics fortran exe name exists in the preference.xml file (whole word search) (2021-02-12)
 * FIX: force_param_values removed from .export (already exported in the foreach loop, this generated a warning) (2021-02-02)
 * FIX: Fail proofing check_java_workspace + add winslash = "/" to all normalizePath for consistency (2021-01-21)
 * FIX: exe check crashed when only Stics exe was given => test is deactivated
 
R/stics_exe_utilities.R
 * FIX: better handling of workspace sub-folders list for identifying available situations to simulate

# SticsOnR 0.2.0 _2021-01-15_

## General changes

* DESCRIPTION
  * license changed to License for CeCILL-C

* README: fix stics_wrapper examples and new ones added

* Automatic testing: 
  * Travis checks removed
  * Github Actions added for checks
  * SticsRTest Travis tests triggering removed
  
* Documentation
  * vignettes : updates according to functions changes, completion
  * functions help: content fixes, examples, setting examples to dontrun, ...
  
* fix: CRAN issues for checks

## Specific changes

* stics_wrapper: using force_param_values, keeping Plant column if exists, added some functions export in foreach loop, get_daily_results call updated according to SticsRFiles changes, added cropr_simulation class attribute, taking into account successives simulations, interface changes, changes about output structure format and way of handling dates and variables changed, added selection of results using sit_names and sit_var_dates_mask, fix error treatment for all simulations failing case, output content fixed in case of errors, added case when using only javastics_path for fixing Stics executable file (whithout knowing its version), passing libpath to workers (run on clusters), re-run when missing output variables (after adding them to the var.mod)

* stics_exe_utilities: fix of Stics executable to use based on only JavaStics content 
and OS type, fix format of available executables list, refactoring and merging of pre-existing functions, 

* run_system_cmd: using now system2, setting executable property for unix like OS

* run_javastics: using now system2, workspace path checks added, fix get_usms_list() usage, added Stics version as an optional argument, added information message at the end of the run, fix about selecting built in Stics executable according to OS type, now masquing all noisy messages from JavaStics command line execution behind R functions.

* JavaStics preferences management: changes in init and overwriting default values, added default preferences files according to OS type 

* Functions added for getting and fixing cores number 



# SticsOnR 0.1.0.9005 _2020-02-06_

* Added a `NEWS.md` file to track changes to the package.
