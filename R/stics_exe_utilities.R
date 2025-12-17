#' @title Adding a new Stics model executable in JavaStics
#'
#' @description Add a new executable of the Stics model to use in JavaStics,
#' and set it as the one to use by default. Please refer to `select_stics_exe()`
#' to change the stics executable used by JavaStics, and `list_stics_exe()`
#' to list all available executables.
#'
#' @param javastics JavaStics installation root folder
#' @param stics_exe      Stics executable name (identifier) or executable path
#' @param overwrite      Boolean. If `stics_exe` is an executable path and an
#' executable with the same name already exist in the bin, overwrite it
#' if `TRUE`, or return an error if `FALSE` default.
#' @param verbose Logical value (optional), `TRUE` to display messages,
#'  `FALSE` otherwise.
#'
#' @details `stics_exe` may be :
#' 1. a model name pointing to a stics executable as done in JavaStics,
#' e.g. "modulostics" for `stics_module.exe`;
#' 2. a stics executable file available from the bin folder in JavaStics,
#' e.g. "stics_module.exe";
#' 3. a path to a stics executable file,
#' eg. "C:/Users/username/Desktop/stics.exe"
#' The function will test those hypothesis in the same order they are presented
#' in the paragraph above. Note that the stics executable will be copied in the
#' bin folder of JavaStics in the third case, and named after the executable
#' name and the user's OS, e.g. "stics_intercrop_win" for ane executable called
#' "stics_intercrop.exe" in a windows OS.
#' If the file already exists in the bin, the function will return an error
#' `overwrite= FALSE` or will replace the executable if `overwrite= TRUE`.
#' If the name already exist, it will check if the executable is the same.
#' If it is, the same name is used, if not, a new name with an incremented index
#' is given, e.g. stics_intercrop_win_2.
#' In any case, the function will inform the user of which stics is being used
#' to avoid any issue.
#'
#' @note "stics_modulo", "sticsmodulo" and "modulostics" are synonyms for the
#' standard STICS executable.
#'
#' @return Nothing. Update the "preference.xml" file in the config of JavaStics.
#'
#' @examples
#' \dontrun{
#' # Using model name:
#' set_stics_exe("/path/to/JavaSTICS/dir", "modulostics")
#' # Using model executable:
#' set_stics_exe("/path/to/JavaSTICS/dir", "stics_modulo.exe")
#' # Using path to add a new executable:
#' set_stics_exe("/path/to/JavaSTICS/dir", "path/to/stics.exe")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
set_stics_exe <- function(
  javastics,
  stics_exe,
  overwrite = FALSE,
  verbose = TRUE
) {
  # checking javastics path
  SticsRFiles:::check_java_path(javastics)

  if (stics_exe == "stics_modulo" || stics_exe == "sticsmodulo") {
    # ' stics_exe= "modulostics"
    switch(SticsRFiles:::user_os(),
      lin = {
        "modulostics_linux"
      },
      mac = {
        "modulostics_mac"
      },
      {
        "modulostics"
      }
    )
  }

  # Case 1: stics_exe is a model name
  if (exist_stics_exe(javastics, stics_exe)) {
    if (verbose) {
      cli::cli_alert_success(
        "Using stics {.val {stics_exe}} (exe: {.val {exe_name}})"
      )
    }
    select_stics_exe(javastics, stics_exe)
    return(invisible())
  }

  # Case 2: stics_exe is an executable from the bin directory in JavaStics:
  exe_file_name <- basename(stics_exe)
  java_stics_exe <- file.path(javastics, "bin", stics_exe)

  if (check_stics_exe(model_path = java_stics_exe, stop = FALSE)) {
    # If the executable is already listed with a name:
    stics_list <- list_stics_exe(javastics)$stics_list
    exe_in_list <- grepl(paste0("^", stics_exe, "$"), unlist(stics_list))

    # If several are listed with the same exe (but different name), take the
    # first one (we don't care which name here):
    if (any(exe_in_list)) {
      exe_to_use <- which(exe_in_list == TRUE)
      if (length(exe_to_use) > 1) {
        exe_to_use <- exe_to_use[1]
      }
      stics_exe <- stics_list[exe_to_use]
      select_stics_exe(javastics, names(stics_exe))
      if (verbose) {
        cli::cli_alert_success(
          "Using stics {.val {names(stics_exe)}} (exe: {.val {stics_exe[[1]]}})"
        )
      }
      return(invisible())
    }

    # If not, continue.
    if (verbose) {
      cli::cli_alert_success(
        "Using stics executable from: {.val {java_stics_exe}}"
      )
    }
  } else if (check_stics_exe(model_path = stics_exe, stop = FALSE)) {
    # Case 3: stics_exe is a path to the executable

    if (exe_file_name == "stics_modulo") {
      stop(
        "Overwriting the standard STICS version shipping with JavaStics
           is not allowed. Please rename your executable file."
      )
    }

    java_stics_exe <- file.path(javastics, "bin", exe_file_name)

    # Copy the executable file in the bin folder of JavaStics:
    is_copied <- file.copy(
      from = stics_exe,
      to = java_stics_exe,
      overwrite = overwrite
    )
    if (!is_copied) {
      stop(
        "Impossible to copy stics_exe file into the bin directory of
           JavaStics. Check if the file exists already and ",
        "delete it manually if needed (overwrite is always FALSE).
           Use only the file executable name as stics_exe if you need to
           execute the one from JavaStics/bin"
      )
    }
    if (verbose) {
      cli::cli_alert_success(
        "Using stics executable from:
                                       {.val {stics_exe}}"
      )
    }
  } else {
    # Case were stics_exe was not found anywhere
    stop(
      "stics_exe was not found as a stics name, executable in the bin path
         of JavaStics nor executable path: ",
      stics_exe
    )
  }

  xml_path <- file.path(javastics, "config", "preferences.xml")
  xml_path_ori <- file.path(javastics, "config", "preferences_ori.xml")
  xml_path_prev <- file.path(javastics, "config", "preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path, xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path, xml_path_prev)

  xml_pref <- get_xml_doc(xml_path)

  # Getting the existing list in pref file
  stics_exe_list <- list_stics_exe(javastics)
  nb_models <- length(stics_exe_list$stics_list)

  # Adding the new exe in the list, and name it using the exe name
  # + the user OS name
  new_stics_name <- paste0(
    gsub(".exe", "", exe_file_name),
    "_",
    SticsRFiles:::user_os()
  )

  # Check if the name already exist:
  exist_stics_name <- exist_stics_exe(javastics, new_stics_name)
  if (exist_stics_name) {
    # If it does, check if the executable is the same:
    if (stics_exe_list$stics_list[[new_stics_name]] != exe_file_name) {
      # If it is different, uses a new name with an index as a suffix.
      i <- 1
      while (exist_stics_exe(javastics, new_stics_name)) {
        i <- i + 1
        new_stics_name <- paste0(
          gsub(".exe", "", exe_file_name),
          "_",
          SticsRFiles:::user_os(),
          "_",
          i
        )
      }
    } else {
      # If they have the same executable, use the same name.
      SticsRFiles:::set_values(
        xml_pref,
        '//entry[@key="model.last"]',
        new_stics_name
      )
      SticsRFiles:::save_xml_doc(xml_pref, xml_path)
      return(check_stics_exe(java_stics_exe))
    }
  }

  stics_exe_list$stics_list[nb_models + 1] <- exe_file_name
  names(stics_exe_list$stics_list)[nb_models + 1] <- new_stics_name

  # writing models list string
  # and setting the current used model with the added one
  stics_exe_string <- paste0(
    sprintf(
      "{%s\t%s},",
      names(stics_exe_list$stics_list),
      stics_exe_list$stics_list
    ),
    collapse = ""
  )

  xml_pref <- get_xml_doc(xml_path)

  SticsRFiles:::set_values(
    xml_pref,
    '//entry[@key="model.last"]',
    new_stics_name
  )
  SticsRFiles:::set_values(
    xml_pref,
    '//entry[@key="model.list"]',
    stics_exe_string
  )

  # writing file
  SticsRFiles:::save_xml_doc(xml_pref, xml_path)

  # Setting stics_exe to executable (OS != windows)
  # and checking if it is a Stics exe file
  check_stics_exe(java_stics_exe)
}

#' @title List stics executables available in JavaStics
#'
#' @description Return all stics identifier names and executable available
#' in JavaStics
#'
#' @param javastics Path to the JavaStics installation directory
#'
#' @return A list of two:
#' - stics_list: named list of the stics executable
#' - current: a named list of the executable currently in use
#'
#' @details The information is read from the `preference.xml` file in JavaStics.
#' The function first controls that it exists and is complete, and if not it
#' creates it using an OS-specific version.
#'
#' @examples
#' \dontrun{
#' list_stics_exe("path/to/Javastics")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
list_stics_exe <- function(javastics) {
  # checking javastics path
  SticsRFiles:::check_java_path(javastics)

  # If the preferences file does not exist, or is incomplete, it means JavaStics
  # was never used before. So we have to use a template for the preferences.
  is_pref <- SticsRFiles:::exists_javastics_pref(javastics)

  config_pref <- file.path(javastics, "config", "preferences.xml")

  if (!is_pref) {
    cli::cli_alert_info("Preference file not found in {.code javastics}.")
    SticsRFiles:::init_javastics_pref(javastics, overwrite = FALSE)
  } else {
    # If the preferences is availabble, control that it is complete
    # (it is not on at install)

    xml_pref <- get_xml_doc(config_pref)

    current_stics <- SticsRFiles:::get_values(
      xml_pref,
      '//entry[@key="model.last"]'
    )
    if (is.null(current_stics)) {
      cli::cli_alert_info(
        "Preference file in {.code javastics}
                          was found incomplete."
      )
      SticsRFiles:::init_javastics_pref(javastics, overwrite = TRUE)
    }
  }

  xml_pref <- get_xml_doc(config_pref)

  current_stics <- SticsRFiles:::get_values(
    xml_pref,
    '//entry[@key="model.last"]'
  )

  stics_list <- SticsRFiles:::get_values(xml_pref, '//entry[@key="model.list"]')
  stics_list_parsed <- gsub(
    "\\{|\\}",
    "",
    stics_list
  ) %>%
    strsplit(",|\t") %>%
    unlist()
  stics_list_names <- stics_list_parsed[seq_along(stics_list_parsed) %% 2 == 1]
  stics_list <- as.list(stics_list_parsed[
    seq_along(stics_list_parsed) %% 2 == 0
  ])
  names(stics_list) <- stics_list_names
  list(stics_list = stics_list, current = stics_list[current_stics])
}


#' Checking if given path is a Stics executable path
#'
#' @param model_path Model executable path
#' @param version Logical, or getting system command return i.e.
#' model version or not (default)
#' @param stop Logical for stopping or not execution
#' @param verbose provide hints to the user if `TRUE` (only if `stop= FALSE`)
#'
#' @return System output (error,...)
#'
#' @keywords internal
#'
#' @noRd
#'
check_stics_exe <- function(
  model_path,
  version = FALSE,
  stop = TRUE,
  verbose = FALSE
) {
  # Need to set the directory to the one of the exe for system calls
  start_dir <- getwd()
  on.exit(setwd(start_dir))

  # Check that file exist:
  if (!file.exists(model_path)) {
    if (stop) {
      stop(paste("Executable file doesn't exist: ", model_path))
    } else {
      if (verbose) {
        cli::cli_alert_danger(
          "Executable file does not exist:
                                        {.val {model_path}}"
        )
      }
      return(invisible(FALSE))
    }
  }

  # Make the file executable if needed for linux or Mac
  if (!SticsRFiles:::set_file_executable(model_path)) {
    if (stop) {
      stop(paste("Cannot give execute permissions for model: ", model_path))
    } else {
      if (verbose) {
        cli::cli_alert_danger(
          "Cannot give execute permissions
                                        for model: {.val {model_path}}."
        )
      }
      return(invisible(FALSE))
    }
  }
  # catching returned error status
  err_status <- suppressWarnings(run_system_cmd(
    model_path,
    com_args = "--version",
    output = version
  ))

  # exiting if any error
  if (!err_status) {
    if (stop) {
      stop(paste(
        "File",
        model_path,
        "is either not executable, or an exe for another OS."
      ))
    } else {
      if (verbose) {
        cli::cli_alert_danger(
          "File {.val {model_path}} is either not
                                        executable, or an exe for another OS."
        )
      }
      return(invisible(FALSE))
    }
  }

  # If version is required
  if (version) {
    # attaching the version attribute & removing the output one
    # Filtering only the first line in case of other information
    # exist on additional lines (commit,...)
    attr(err_status, "version") <- gsub(
      pattern = "Modulostics version : ",
      x = trimws(attr(err_status, "output")[1]),
      replacement = ""
    )
    attr(err_status, "output") <- NULL
  }

  return(invisible(err_status))
}

#' @title Select the Stics executable
#'
#' @description Select the Stics model executable to use from the
#' preference file in JavaStics.
#'
#' @param javastics Path to the JavaStics installation directory
#' @param stics_exe      Stics executable name (see details)
#'
#' @details The `stics_exe` is **not** the name of the executable file, but the
#' identification name. Please use `list_stics_exe()` to list all available
#' executables, and `set_stics_exe()` to add and select a new one.
#' The identification names can be retreived using
#' `names(list_stics_exe(javastics)$stics_list)`
#'
#' @note "stics_modulo", "sticsmodulo" and "modulostics" are synonyms for
#' the standard STICS executable.
#'
#' @return Nothing. Update the "preference.xml" file in the config of JavaStics.
#'
#' @examples
#' \dontrun{
#' select_stics_exe("path/to/Javastics", "modulostics")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
select_stics_exe <- function(javastics, stics_exe = "stics_modulo") {
  # checking javastics path
  SticsRFiles:::check_java_path(javastics)

  if (stics_exe == "stics_modulo" || stics_exe == "sticsmodulo") {
    stics_exe <- "modulostics"
  }

  # if no preference have been set yet
  if (!SticsRFiles:::exists_javastics_pref(javastics)) {
    SticsRFiles:::init_javastics_pref(javastics)
  }

  # If the executable does not exist yet in the preference file:
  if (!exist_stics_exe(javastics, stics_exe)) {
    stop(
      "The provided model name doesn't exist in this configuration : ",
      javastics,
      ".\n Add it before with `set_stics_exe()` function!"
    )
  }

  xml_path <- file.path(javastics, "config", "preferences.xml")
  xml_path_ori <- file.path(javastics, "config", "preferences_ori.xml")
  xml_path_prev <- file.path(javastics, "config", "preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path, xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path, xml_path_prev)

  xml_pref <- get_xml_doc(xml_path)

  current_model <- SticsRFiles:::get_values(
    xml_pref,
    '//entry[@key="model.last"]'
  )
  # no need to set the model
  if (current_model == stics_exe) {
    return(invisible())
  }
  # setting model exe
  SticsRFiles:::set_values(xml_pref, '//entry[@key="model.last"]', stics_exe)
  # saving modified file
  SticsRFiles:::save_xml_doc(xml_pref, xml_path)
}


#' @title Check if a stics executable is available
#'
#' @description Checks if a stics model executable is available in JavaStics
#' (in the "preference.xml" file).
#'
#' @param javastics JavaStics installation root folder
#' @param stics_exe  Stics executable name (see details)
#'
#' @details The current model executable available in JavaStics can be
#' listed using `list_stics_exe()`.
#'
#' @examples
#' \dontrun{
#' exist_stics_exe("path/to/JavaSTICS-v131-stics-v841", "stics_name")
#' }
#'
#' @return Existing status, logical
#'
#' @keywords internal
exist_stics_exe <- function(javastics, stics_exe) {
  is.element(stics_exe, names(list_stics_exe(javastics)$stics_list))
}


#' @title Remove a Stics model executable from JavaStics
#'
#' @description Remove a stics model from the list of available model
#' executables in JavaStics (modifies the "preferences.xml" file).
#'
#'
#' @param javastics Path to the JavaStics installation directory
#' @param stics_exe      Stics executable identifier name (see details)
#'
#' @details The executable file in the current JavaStics bin folder will not be
#' deleted, please do it by hand instead (in "JavaStics.../bin").
#' The `stics_exe` is **not** the name of the executable file, but the
#' identification name. Please use `list_stics_exe()` to list all available
#' executables, and `set_stics_exe()` to add a new one. The identification names
#' can be retreived using `names(list_stics_exe(javastics)$stics_list)`
#'
#' @return Nothing. Update the "preference.xml" file in the config of JavaStics.
#'
#' @examples
#' \dontrun{
#' remove_stics_exe("path/to/JavaStics", "model_name")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
remove_stics_exe <- function(javastics, stics_exe) {
  # checking javastics path
  SticsRFiles:::check_java_path(javastics)

  if (!exist_stics_exe(javastics, stics_exe)) {
    warning(
      "The model doesn't exists or his name is miss spelled : ",
      stics_exe,
      ".\n Call names(list_stics_exe(javastics)$stics_list)
            to list existing executables"
    )
    return()
  }

  xml_path <- file.path(javastics, "config", "preferences.xml")
  xml_path_ori <- file.path(javastics, "config", "preferences_ori.xml")
  xml_path_prev <- file.path(javastics, "config", "preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path, xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path, xml_path_prev)

  stics_exe_list <- list_stics_exe(javastics)

  # Remove the model version:
  stics_exe_list$stics_list <- stics_exe_list$stics_list[
    -grep(
      stics_exe,
      names(stics_exe_list$stics_list)
    )
  ]

  # writing models list string
  stics_exe_string <- paste0(
    sprintf(
      "{%s\t%s},",
      names(stics_exe_list$stics_list),
      stics_exe_list$stics_list
    ),
    collapse = ""
  )

  xml_pref <- get_xml_doc(xml_path)

  # removing model from last if needed
  if (stics_exe_list$current == stics_exe) {
    warning(
      "JavaStics was using this Stics executable currently",
      " Please set a new model executable to use using
            `select_stics_exe()`"
    )
    SticsRFiles:::set_values(xml_pref, '//entry[@key="model.last"]', "")
  }

  SticsRFiles:::set_values(
    xml_pref,
    '//entry[@key="model.list"]',
    stics_exe_string
  )

  # writing file
  SticsRFiles:::save_xml_doc(xml_pref, xml_path)
}


#' Getting a xml_document from an XML file
#'
#' @param file
#'
#' @return a SticsRFiles xml_document object
#' @keywords internal
#'
#' @noRd

get_xml_doc <- function(file) {
  suppressMessages(doc <- SticsRFiles:::xmldocument(file))
  doc
}
