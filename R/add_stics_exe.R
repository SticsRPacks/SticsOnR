#' @title Adding a new Stics model executable in JavaStics
#'
#' @description Add a new executable of the Stics model to use in JavaStics, and set it as
#' the one to use by default. Please refer to `select_stics_exe()` to change the stics
#' executable used by JavaStics, and `list_stics_exe()` to list all available executables.
#'
#' @param javastics_path JavaStics installation root folder
#' @param stics_exe      Stics executable name (identifier) or executable path
#' @param overwrite      Boolean. If `stics_exe` is an executable path and an executable with the same name already exist in the bin,
#'  overwrite it if `TRUE`, or return an error if `FALSE` default.
#'
#' @details `stics_exe` may be :
#' 1. a model name pointing to a stics executable as done in JavaStics, e.g. "modulostics" for `stics_module.exe`;
#' 2. a stics executable file available from the bin folder in JavaStics, e.g. "stics_module.exe";
#' 3. a path to a stics executable file, eg. "C:/Users/username/Desktop/stics.exe"
#' The function will test those hypothesis in the same order they are presented in the paragraph above. Note that the
#' stics executable will be copied in the bin folder of JavaStics in the third case, and named after the executable name and the
#' user's OS, e.g. "stics_intercrop_win" for ane executable called "stics_intercrop.exe" in a windows OS.
#' If the file already exists in the bin, the function will return an error `overwrite= FALSE` or will replace the executable if `overwrite= TRUE`.
#' If the name already exist, it will check if the executable is the same. If it is, the same name is used, if not, a new name with an incremented index
#' is given, e.g. stics_intercrop_win_2.
#' In any case, the function will inform the user of which stics is being used to avoid any issue.
#'
#' @note "stics_modulo", "sticsmodulo" and "modulostics" are synonyms for the standard STICS executable.
#'
#' @return Nothing. Update the "preference.xml" file in the config of JavaStics.
#'
#' @examples
#' \dontrun{
#'  # Using model name:
#'  set_stics_exe("/path/to/JavaSTICS/dir","modulostics")
#'  # Using model executable:
#'  set_stics_exe("/path/to/JavaSTICS/dir","stics_modulo.exe")
#'  # Using path to add a new executable:
#'  set_stics_exe("/path/to/JavaSTICS/dir","path/to/stics.exe")
#' }
#'
#' @keywords internal
set_stics_exe <- function(javastics_path, stics_exe, overwrite= FALSE) {

  # checking javastics path
  check_java_path(javastics_path)

  if(stics_exe=="stics_modulo"|stics_exe=="sticsmodulo"){
    stics_exe= "modulostics"
  }

  # Case 1: stics_exe is a model name
  if(exist_stics_exe(javastics_path, stics_exe)){
    exe_name= list_stics_exe(javastics_path)$stics_list[stics_exe][[1]]
    cli::cli_alert_success("Using stics {.val {stics_exe}} (exe: {.val {exe_name}})")
    select_stics_exe(javastics_path, stics_exe)
    return(invisible())
  }

  # Case 2: stics_exe is an executable from the bin directory in JavaStics:
  exe_file_name <- basename(stics_exe)
  java_stics_exe <- file.path(javastics_path, "bin", stics_exe)

  if(check_stics_exe(model_path = java_stics_exe, stop = FALSE)){

    # If the executable is already listed with a name:
    stics_list= list_stics_exe(javastics_path)$stics_list
    exe_in_list= grepl(stics_exe,unlist(stics_list))

    # If several are listed with the same exe (but different name), take the first one (we don't care which name here:
    if(any(exe_in_list)){
      exe_to_use= which(exe_in_list==TRUE)
      if(length(exe_to_use)>1){
        exe_to_use= exe_to_use[1]
      }
      stics_exe= stics_list[exe_to_use]
      select_stics_exe(javastics_path, names(stics_exe))
      cli::cli_alert_success("Using stics {.val {names(stics_exe)}} (exe: {.val {stics_exe[[1]]}})")
      return(invisible())
    }

    # If not, continue.
    cli::cli_alert_success("Using stics executable from: {.val {java_stics_exe}}")

  }else if(check_stics_exe(model_path = stics_exe, stop = FALSE)){
    # Case 3: stics_exe is a path to the executable

    if(exe_file_name=="stics_modulo"){
      stop("Overwriting the standard STICS version shipping with JavaStics is not allowed. Please rename your executable file.")
    }

    java_stics_exe <- file.path(javastics_path, "bin", exe_file_name)


    # Copy the executable file in the bin folder of JavaStics:
    is_copied= file.copy(from = stics_exe, to = java_stics_exe, overwrite = overwrite)
    if(!is_copied){
      stop("Impossible to copy stics_exe file into the bin directory of JavaStics. Check if the file exists already and ",
           "delete it manually if needed (overwrite is always FALSE). Use only the file executable name as stics_exe if you need to execute the one from JavaStics/bin")
    }
    cli::cli_alert_success("Using stics executable from: {.val {stics_exe}}")
  }else{
    # Case were stics_exe was not found anywhere
    stop("stics_exe was not found as a stics name, executable in the bin path of JavaStics nor executable path: ",stics_exe)
  }

  xml_path <- file.path(javastics_path, "config", "preferences.xml")
  xml_path_ori <- file.path(javastics_path, "config", "preferences_ori.xml")
  xml_path_prev <- file.path(javastics_path, "config", "preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path, xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path, xml_path_prev)


  xml_pref <- SticsRFiles:::xmldocument(xml_path)


  # Getting the existing list in pref file
  stics_exe_list <- list_stics_exe(javastics_path)
  nb_models <- length(stics_exe_list$stics_list)

  # Adding the new exe in the list, and name it using the exe name + the user OS name
  new_stics_name= paste0(gsub(".exe","",exe_file_name),"_",user_os())

  # Check if the name already exist:
  exist_stics_name= exist_stics_exe(javastics_path, new_stics_name)
  if(exist_stics_name){
    # If it does, check if the executable is the same:
    if(stics_exe_list$stics_list[[new_stics_name]]!=exe_file_name){
      # If it is different, uses a new name with an index as a suffix.
      i= 1
      while(exist_stics_exe(javastics_path, new_stics_name)){
        i= i + 1
        new_stics_name= paste0(gsub(".exe","",exe_file_name),"_",user_os(),"_",i)
      }
    }else{
      # If they have the same executable, use the same name.
      SticsRFiles:::setValues(xml_pref, '//entry[@key="model.last"]', new_stics_name)
      SticsRFiles:::saveXmlDoc(xml_pref, xml_path)
      return(check_stics_exe(java_stics_exe))
    }
  }

  stics_exe_list$stics_list[nb_models + 1]= exe_file_name
  names(stics_exe_list$stics_list)[nb_models + 1]= new_stics_name

  # writing models list string
  # and setting the current used model with the added one
  stics_exe_string= paste0(sprintf("{%s\t%s},", names(stics_exe_list$stics_list), stics_exe_list$stics_list), collapse = "")
  xml_pref <- SticsRFiles:::xmldocument(xml_path)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.last"]', new_stics_name)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.list"]', stics_exe_string)

  # writing file
  SticsRFiles:::saveXmlDoc(xml_pref, xml_path)

  # Setting stics_exe to executable (OS != windows)
  # and checking if it is a Stics exe file
  check_stics_exe(java_stics_exe)
}
