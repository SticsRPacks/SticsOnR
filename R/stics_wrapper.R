#' @title Running usm(s) from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses Stics directly through a system call, can
#' force Stics input parameters with values given in arguments.
#'
#' @param model_options List containing any information needed by the model.
#' In the case of Stics: `javastics` (or/and `stics_exe`) and
#' `workspace` the path of the directory containing the Stics input data
#' for each USM (one folder per USM where Stics input files are stored in txt
#' format). See `stics_wrapper_options()` for more information.
#'
#' @param param_values (optional) a named vector or a tibble that contains
#' values of Stics input parameters to use in the simulations.
#' Should have one named column per parameter.
#' An optional column named situation containing the name of the situations
#' (USMs for Stics) allows to define different values of the parameters for
#' different situations. If param_values is not provided, the simulations will
#' be performed using the parameters values defined in the Stics input files
#' referenced in model_options argument.
#' WARNING: up to now, for intercrop situations, plant parameter(s) defined in
#' param_values is(are) only associated to the main crop.
#'
#' @param situation (optional) vector of situations (USMs) names for which
#' results must be returned. Results for all simulated situations are returned
#' if not provided.
#' @param var (optional) vector of variables names for which results
#' must be returned. If not provided, it returns the results for all simulated
#' variables that were already listed in the var.mod
#' (i.e. from the last simulation).
#' @param dates (optional) vector of dates (POSIXct) for which results must
#' be returned. Results for all dates simulated are returned if not provided.
#' If required dates varies between situations, use sit_var_dates_mask argument
#'
#' @param sit_var_dates_mask (optional) List of situations: a named list
#' containing a mask for variables and dates for which simulated values
#' should be returned. Typically a list containing the observations to which
#' simulations should be compared as provided by `SticsRFiles::get_obs`
#'
#' @param sit_names `r lifecycle::badge("deprecated")` `sit_names` is no
#'   longer supported, use `situation` instead.
#'
#' @param var_names `r lifecycle::badge("deprecated")` `var_names` is no
#'   longer supported, use `var` instead.
#'
#' @return A list containing simulated values (`sim_list`: a list of tibbles
#' (one element per situation) and an error code (`error`) indicating if at
#' least one simulation ended with an error.
#'
#' @seealso `stics_wrapper_options()` for more information on how to
#' provide `model_options`.
#'
#' @examples
#' \dontrun{
#'
#' # Specifying the JavaStics folder
#' javastics <- "/path/to/JavaSTICS/folder"
#'
#' # Setting the input data folder path, root directory of the usms directories
#' data_path <- "/path/to/usms/subdirs/root"
#'
#' # Setting the mandatory simulations options
#' sim_options <- stics_wrapper_options(
#'   javastics = javastics,
#'   workspace = data_path
#' )
#'
#' # Running all the usms that have a corresponding input folder in data_path
#' results <- stics_wrapper(sim_options)
#'
#' # Running a sublist of usm
#' usms_list <- c("wheat", "pea", "maize")
#' results <- stics_wrapper(sim_options, situation = usms_list)
#'
#' # Applying a single parameter values vector for the sublist of usms
#' param_values <- c(0.002, 50)
#' names(param_values) <- c("dlaimax", "durvieF")
#' results <- stics_wrapper(
#'   model_options = sim_options,
#'   situation = usms_list, param_values = param_values
#' )
#'
#' # Applying different values of the parameters for the usms
#' # Let's run usm wheat with c(dlaimax=0.001, durvieF=50),
#' # usm pea with c(dlaimax=0.001, durvieF=60),
#' # and usm maize with c(dlaimax=0.001, durvieF=70)
#' param_values <- data.frame(
#'   Situation = c("wheat", "pea", "maize"),
#'   dlaimax = c(0.001, 0.001, 0.001),
#'   durvieF = c(50, 60, 70)
#' )
#' results <- stics_wrapper(
#'   model_options = sim_options,
#'   param_values = param_values, situation = c("wheat", "pea", "maize")
#' )
#' }
#'
#' @export
#'
#' @importFrom foreach %dopar% %do%
#' @importFrom parallel clusterCall makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom magrittr %>%
#'
stics_wrapper <- function(model_options,
                          param_values = NULL,
                          situation = NULL,
                          var = NULL,
                          dates = NULL,
                          sit_var_dates_mask = NULL,
                          sit_names = lifecycle::deprecated(),
                          var_names = lifecycle::deprecated()) {

  # TODO LIST
  #   - handle the case of stages (stages should be specified in the var.mod ...
  #   + handle the case when simulations does not reach the asked stages ...)
  #


  if (lifecycle::is_present(sit_names)) {
    lifecycle::deprecate_warn(
      "1.0.0", "stics_wrapper(sit_names)",
      "stics_wrapper(situation)"
    )
  } else {
    sit_names <- situation # to remove when we update inside the function
  }
  if (lifecycle::is_present(var_names)) {
    lifecycle::deprecate_warn(
      "1.0.0", "stics_wrapper(var_names)",
      "stics_wrapper(var)"
    )
  } else {
    var_names <- var # to remove when we update inside the function
  }


  # Preliminary model checks and initializations -------------------------------

  # Checking model_options content, stopping when mandatory
  # values are not set or not valid
  model_options <- do.call(stics_wrapper_options, model_options)
  # NB: splating the arguments given in input so the function checks by itself,
  # no need to explicitely add an argument for checking

  # Getting list values into separated variables
  stics_exe <- model_options$stics_exe
  data_dir <- model_options$workspace
  parallel <- model_options$parallel
  cores <- model_options$cores
  verbose <- model_options$verbose
  time_display <- model_options$time_display
  successive_usms <- model_options$successive
  force <- model_options$force
  javastics <- model_options$javastics

  # Checking if javastics path is set when forcing parameters
  if (!is.null(param_values) && is.null(javastics))
    stop(paste0("When parameters values are to be forced,",
                " a JavaStics path must be set in model_options list",
                " (see javastics argument of stics_wrapper_options function)."))

  # Checking Stics executable
  if (!force) check_stics_exe(stics_exe)

  # Activate the stopwatch if required
  if (time_display) start_time <- Sys.time()

  # Define the list of USMs to simulate and initialize results -----------------
  # Check the available USMs
  avail_sit <- list.dirs(data_dir, full.names = TRUE, recursive = FALSE)

  ## Checking existing files
  files_exist <- file.exists(file.path(avail_sit, "new_travail.usm"))
  avail_sit <- basename(avail_sit)[files_exist]
  if (length(avail_sit) == 0) {
    stop(paste("Not any Stics directories found in:", data_dir))
  }

  # Define the USMs to simulate from available USMs and user requirements
  # concerning the results to return (sit_names and sit_var_dates_mask
  # arguments) => sit2simulate and required_situations
  required_situations <- union(sit_names, names(sit_var_dates_mask))
  if (!is.null(required_situations)) {
    # If some required situations can not be simulated, warns the user
    if (length(setdiff(required_situations, avail_sit)) > 0) {
      warning(paste0(
        "No folder(s) found in ", data_dir, " for USMs ",
        paste(setdiff(required_situations, avail_sit),
              collapse = " "
        ),
        "\n These USMs will not be simulated."
      ))
    }
    sit2simulate <- intersect(avail_sit, required_situations)
  } else {
    # If neither sit_names nor sit_var_dates_mask are provided, all USMs defined
    #  in subfolders of data_dir must be simulated
    sit2simulate <- avail_sit
    required_situations <- avail_sit
  }

  # Case of successive USMs (argument successive_usms)
  ## Check that all successive usms are available
  if (length(setdiff(unlist(successive_usms), avail_sit)) > 0) {
    warning(paste0(
      "No folder(s) found in ", data_dir, " for USMs ",
      paste(setdiff(unlist(successive_usms), avail_sit),
            collapse = " "
      ),
      "\n The corresponding successions of USMs will not be simulated."
    ))
    # Remove successions for which at least one USMs is not available
    idx <- unique(sapply(
      setdiff(unlist(successive_usms), avail_sit),
      function(x) {
        which(sapply(
          successive_usms,
          function(y) x %in% y
        ))
      }
    ))
    successive_usms[[idx]] <- NULL
  }
  ## Add the successive USMs in the list of USMs to simulate if there are some
  ## missing ones and order them
  sit2simulate <- c(
    unlist(successive_usms),
    setdiff(sit2simulate, unlist(successive_usms))
  )

  # Initialize the list of phenological stages (specific treatment in the loop)
  stages_list <- c("iamfs", "idebdess", "idebdorms", "idrps", "ifindorms",
                   "iflos", "iflos_minus_150", "iflos_plus_150", "igers",
                   "ilans", "ilaxs", "ilevs", "imats", "imontaisons")


  # Calculating directories list
  run_dirs <- file.path(data_dir, sit2simulate)

  res <- list()
  res$error <- FALSE
  res$sim_list <- stats::setNames(
    vector("list", length(required_situations)),
    required_situations
  )

  # Should all data be returned for each required situation ?
  keep_all_data <- is.null(sit_var_dates_mask) && is.null(var_names) &&
    is.null(dates)

  # In case of successive USMs or single USM, disable parallel run
  if (!is.null(successive_usms) || length(sit2simulate)==1) parallel <- FALSE

  if (parallel) {
    # Managing parallel model simulations
    # Managing cores number to use
    cores_nb <- get_cores_nb(parallel = parallel, required_nb = cores)

    # Do not allow more cores than number of USMs to simulate: waste of time
    cores_nb <- min(cores_nb, length(sit2simulate))

    # Launching the cluster
    cl <- makeCluster(cores_nb)

    # Stopping the cluster when exiting
    on.exit(stopCluster(cl))

    # Registering cluster
    registerDoParallel(cl)
    clusterCall(cl, function(x) .libPaths(x), .libPaths())

    `%do_par_or_not%` <- foreach::`%dopar%`
  } else {
    `%do_par_or_not%` <- foreach::`%do%`

  }

  # Run Stics and store results ------------------------------------------------

  # Hack to make force_param_values available on the shared environment.
  # This is done to make it compatible with clusters
  # (Meso@LR didn't work without).
  # Create a local copy of force_param_values function,
  # assigning it with SticsRFiles::force_param_values

  i <- 1 # initialization to avoid Note in check ...
  out <- foreach::foreach(
    i = seq_along(sit2simulate),
    .export = c("run_stics", "select_results"),
    .packages = c("SticsRFiles")
    #) %doparornot% {
  ) %do_par_or_not% {
    ## Loops on the USMs that can be simulated
    ## out is a list containing vectors of:
    ##   o list of simulated outputs,
    ##   o flag TRUE if the requested simulation has been not performed
    ##                        (model error),
    ##   o flag FALSE if all the requested dates and variables were
    ##     not simulated,
    ##   o message in case of warning or error
    ## (one value per set of parameter values to force)


    run_dir <- run_dirs[i]
    situation <- sit2simulate[i]

    # Select param_values depending on the situation to simulate
    # convert param_values in a tibble if needed
    param_values_sit <- tibble::tibble(!!!param_values)
    if (!is.null(param_values)) {
      if ("situation" %in% names(param_values_sit)) {
        param_values_sit <- param_values_sit %>%
          dplyr::filter(situation == sit2simulate[i]) %>%
          dplyr::select(-situation)
      }
      if ("variete" %in% names(param_values_sit)) {
        param_values_sit <- dplyr::select(
          param_values_sit, c("variete",
                              setdiff(names(param_values_sit),
                                      "variete"))
        )
      }
    }
    if (is.null((param_values_sit)) || nrow(param_values_sit) == 0) {
      param_values_sit <- tibble::tibble(NA)
    }


    # Initialize out content
    sim_list <- vector("list", nrow(param_values_sit))
    flag_error <- rep(FALSE, nrow(param_values_sit))
    flag_rqd_res <- rep(TRUE, nrow(param_values_sit))
    messages <- as.list(rep("", nrow(param_values_sit)))

    # For each set of parameter values to force in the model
    for (ip in seq_len(nrow(param_values_sit))) {

      # Force parameters values
      if (!SticsRFiles::force_param_values(
        run_dir,
        dplyr::slice(param_values_sit, ip),
        javastics
      )) {
        mess <- warning(paste(
          "Error when generating the forcing parameters file for USM",
          situation, ". \n "
        ))
        sim_list[ip] <- NULL
        flag_error[ip] <- TRUE
        flag_rqd_res[ip] <- FALSE
        messages[ip] <- mess
        next()
      }

      # Handle the simulation (may be repeated - using flag simulate - in case
      # some configuration files are not well defined)
      varmod_modified <- FALSE
      simulate <- TRUE
      while (simulate) {

        # Handling successive USMs (if the usm is part of the list and not in
        # first position it must be linked with previous one)
        is_succ <- any(sapply(
          successive_usms,
          function(x) match(sit2simulate[i], x)
        ) >= 2)
        if (!is.na(is_succ) && is_succ) {

          # Checking recup.tmp and snow_variables.txt files
          f_recup <- c(
            file.path(run_dirs[i - 1], paste0("recup", ip, ".tmp")),
            file.path(
              run_dirs[i - 1],
              paste0("snow_variables", ip, ".txt")
            )
          )
          f_exist <- file.exists(f_recup)

          if (!all(f_exist)) {
            mess <- warning(paste(
              "Error running the Stics model for USM",
              situation,
              ". \n This USMs is part of a succession",
              "but recup.tmp or snow_variables.txt",
              "file(s) was/were not created by the previous USM."
            ))
            sim_list[ip] <- NULL
            flag_error[ip] <- TRUE
            flag_rqd_res[ip] <- FALSE
            messages[ip] <- mess
            next()
          }

          # Copying files and checking return
          recup_copy <- file.copy(
            from = f_recup,
            to = file.path(run_dir, c(
              "recup.tmp",
              "snow_variables.txt"
            )),
            overwrite = TRUE
          )
          if (!all(recup_copy)) {
            mess <- warning(
              paste(
                "Error copying recup.tmp and/or",
                "snow_variables.txt file(s) for USM",
                situation
              )
            )
            sim_list[ip] <- NULL
            flag_error[ip] <- TRUE
            flag_rqd_res[ip] <- FALSE
            messages[ip] <- mess
            next()
          }

          # The following could be done only once in case of repeated call
          # to the wrapper (e.g. parameters estimation ...)
          SticsRFiles::set_usm_txt(
            file = file.path(run_dir, "new_travail.usm"),
            param = "codesuite", value = 1
          )
        }


        ## Run the model, forcing not to check the model executable (saves time)
        usm_out <- run_stics(stics_exe, run_dir, check = FALSE)

        ### In case of successive USMs, re-initialize codesuite (to allow next
        ### run to be in non-successive mode) and rename recup.tmp and
        ### snow_variables.txt (for usms that have a successor)
        if (!is.na(is_succ) && is_succ) {
          SticsRFiles::set_usm_txt(
            file = file.path(
              run_dir,
              "new_travail.usm"
            ),
            param = "codesuite", value = 0
          )
        }
        is_prev <- any(sapply(
          successive_usms,
          function(x) match(sit2simulate[i], x) < length(x)
        ))
        if (!is.na(is_prev) && is_prev) {
          file.rename(
            from = file.path(run_dir, "recup.tmp"),
            to = file.path(run_dir, paste0("recup", ip, ".tmp"))
          )
          file.rename(
            from = file.path(run_dir, "snow_variables.txt"),
            to = file.path(run_dir, paste0("snow_variables", ip, ".txt"))
          )
        }

        ### if the model returns an error, ... go to next simulation
        if (usm_out[[1]]$error) {
          mess <- warning(paste(
            "Error running the Stics model for USM",
            situation,
            ". \n ", usm_out[[1]]$message
          ))
          sim_list[[ip]] <- NULL
          flag_error[ip] <- TRUE
          flag_rqd_res[ip] <- FALSE
          messages[[ip]] <- mess
          simulate <- FALSE
          next()
        }

        ## Get results
        sim_tmp <- SticsRFiles::get_sim(run_dir)[[1]]

        ## Any error reading output file ... go to next simulation
        if (is.null(sim_tmp)) {
          mess <- warning(paste(
            "Error reading outputs for ", situation,
            ". \n "
          ))
          sim_list[[ip]] <- NULL
          flag_error[ip] <- TRUE
          flag_rqd_res[ip] <- FALSE
          messages[[ip]] <- mess
          simulate <- FALSE
          next()
        }

        # For phenological stages, replace the zeros by the following non-zero
        # value (works even in case of simulations replicated on several years
        # within a single USM)
        if (length(sim_tmp) > 0) {
          if (length(intersect(stages_list, names(sim_tmp)) > 0)) {
            sim_tmp <-
              sim_tmp %>%
              dplyr::mutate(
                dplyr::across(
                  dplyr::all_of(
                    intersect(stages_list, names(.))
                  ),
                  ~dplyr::na_if(., 0)) %>%
                  tidyr::fill(tidyr::everything(), .direction = "up")
              )
          }
        }

        ## Select data to return
        tmp <- select_results(
          keep_all_data, sit_var_dates_mask, var_names,
          dates, situation, sim_tmp,
          varmod_modified, verbose, run_dir
        )
        sim_list[[ip]] <- tmp$sim_list
        flag_error[ip] <- tmp$flag_error
        flag_rqd_res[ip] <- tmp$flag_rqd_res
        messages[[ip]] <- tmp$message
        simulate <- tmp$simulate
        varmod_modified <- tmp$varmod_modified

      }
    }

    return(list(sim_list, flag_error, flag_rqd_res, messages))
  }


  # Filtering situation names for keeping only the required results
  names(out) <- sit2simulate

  for (isit in required_situations) {
    if (!is.null(out[[isit]][[1]])) {
      res$sim_list[[isit]] <- out[[isit]][[1]]
    }
  }

  # Displaying warnings
  lapply(out, function(x) sapply(x[[4]], function(y) stics_display_warnings(y)))

  # Gather results in one tibble per sit
  for (isit in names(res$sim_list)) {
    res$sim_list[[isit]] <- dplyr::bind_rows(res$sim_list[[isit]])
    if (length(res$sim_list[[isit]]) == 0) res$sim_list[[isit]] <- NULL
  }

  if (length(res$sim_list) == 0) {
    warning("Stics simulations failed for all USMs!!!")
    res$sim_list <- NULL
  } else {
    # Add the attribute cropr_simulation for using CroPlotR package
    attr(res$sim_list, "class") <- "cropr_simulation"
  }


  # Handling errors
  res$error <- any(unlist(lapply(
    out,
    function(x) {
      return(any(x[[2]]) || !all(x[[3]]))
    }
  )))


  # Calculating and printing duration
  if (time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  return(invisible(res))
}



#' @title Select results to return
#'
#' @description This function selects the variables and dates in the simulation
#' results as required by the user through the stics_wrapper arguments.
#' Has been created to lighten the stics_wrapper code.
#'
#' @inheritParams stics_wrapper
#'
#' @param keep_all_data Logical indicating if all simulated data must be
#' selected
#' @param situation Name of the simulated situation
#' @param sim_tmp Results of the simulated situation, as given by get_sim
#' @param varmod_modified Logical indicating if the var.mod file has already
#' been modified in a previous simulation.
#' @param verbose Logical value indicating if information messages must be
#' displayed or not.
#' @param run_dir path of the Stics workspace
#'
#' @return A list containing:
#'    o sim_list: the results of the simulated situation for the required
#'    variables and/or dates
#'    o flag_error: a logical indicating if an error occured
#'    o flag_rqd_res: a logical indicating if the required variables and/or
#'      dates have been found in the simulated results
#'    o simulate: a logical indicating if a new simulation is necessary to get
#'      result for addtional variables
#'    o message: a string containing a message if a warning occurs
#'    o varmod_modified: a logical indicating if the var.mod file has been
#'    modified
#'
#' @noRd

select_results <- function(keep_all_data, sit_var_dates_mask, var_names,
                           dates, situation, sim_tmp, varmod_modified,
                           verbose, run_dir) {
  res <- list(
    sim_list = NULL, flag_error = FALSE, flag_rqd_res = TRUE,
    simulate = FALSE, message = NULL, varmod_modified = varmod_modified
  )

  if (keep_all_data) {

    # return all simulated data
    ############################################################################

    res$sim_list <- sim_tmp
    res$flag_error <- FALSE
    res$flag_rqd_res <- TRUE
    res$simulate <- FALSE
    return(res)
  } else if (!is.null(sit_var_dates_mask) &&
             is.null(sit_var_dates_mask[[situation]])) {

    # no results required for this situation -> return NULL
    ############################################################################

    res$sim_list <- NULL
    res$flag_error <- FALSE
    res$flag_rqd_res <- TRUE
    res$simulate <- FALSE
    return(res)
  } else {

    # some variables/dates explicitely required
    # -> Select from sit_var_dates_mask, var_names, dates arguments
    ############################################################################

    # First select variables ...
    ############################

    if (!is.null(sit_var_dates_mask)) {
      req_var_names <- colnames(sit_var_dates_mask[[situation]])
    } else {
      req_var_names <- c(var_names)
    }

    ## Convert required variables names to Stics variables names (i.e. handle ())
    req_var_names <- SticsRFiles:::var_to_col_names(req_var_names)

    ## Add reserved keywords "Plant" and "Date" from the list
    req_var_names <- unique(c(c("Date", "Plant"), req_var_names))

    ## Identify indexes of required variables among simulated ones
    sim_var_names <- colnames(sim_tmp)
    req_vars_idx <- sim_var_names %in% req_var_names
    inter_var_names <- sim_var_names[req_vars_idx]


    ## In case some variables are not simulated, warn the user, add them in var.mod
    ## and re-simulate or select the results if var.mod has already been modified.
    if (length(inter_var_names) < length(req_var_names)) {
      if (varmod_modified) {
        ## var.mod has already been modified ... warn the user the required
        ## variables will not be simulated

        res$message <- warning(paste(
          "Variable(s)",
          paste(setdiff(req_var_names, inter_var_names),
                collapse = ", "
          ),
          "not simulated by the Stics model for USM",
          situation,
          "although added in", file.path(run_dir, "var.mod"),
          "=> these variables may not be Stics variables, please check spelling. \n ",
          "Simulated variables:", paste(sim_var_names, collapse = ", ")
        ))
        res$flag_error <- FALSE
        res$flag_rqd_res <- FALSE
      } else {
        ## var.mod has not yet been modified ...
        ## try to modify it and resimulate (keyword simulate)

        # TODO: check or set the flagecriture to 15 to get daily data results !!

        ## Remove the reserved keywords from required variables names so that
        ## they do not appear in warning message nor in var.mod
        req_var_names <- req_var_names[!req_var_names %in% c("Date", "Plant")]

        if (verbose) {
          res$message <- warning(paste(
            "Variable(s)",
            paste(setdiff(req_var_names, inter_var_names),
                  collapse = ", "
            ),
            "not simulated by the Stics model for USM", situation,
            "=>", file.path(run_dir, "var.mod"),
            "has been modified and the model re-run."
          ))
        }

        ## For the moment, as we do not provide functions for adding new stics
        ## versions, we don't check the existence of Stics variables (so that
        ## if they are defined in the var.mod they can be simulated even if not
        ## defined in outputs.csv)
        SticsRFiles::gen_varmod(
          workspace = run_dir,
          var = req_var_names,
          force = TRUE
        )
        res$varmod_modified <- TRUE
        res$simulate <- TRUE
        return(res)
      }
    }

    ## Select the results wrt to the required and simulated variables
    ## if required
    if (length(req_var_names) > 2) {
      if (any(req_vars_idx)) {
        sim_tmp <- sim_tmp[, req_vars_idx]
      } else { ## no variable simulated, warn the user and return NULL

        res$message <- warning(paste(
          "Requested variable(s)",
          paste(req_var_names, collapse = ", "),
          "for USM", situation,
          " is (are) not valid STICS variable(s)."
        ))
        res$sim_list <- NULL
        res$flag_error <- TRUE
        res$flag_rqd_res <- FALSE
        res$simulate <- FALSE
        return(res)
      }
    }



    # Then select dates ...
    #######################

    # check required dates
    if (!is.null(sit_var_dates_mask)) {
      req_date_list <- sit_var_dates_mask[[situation]]$Date
    } else if (!is.null(dates)) {
      req_date_list <- dates
    } else { ## do not operate selection on dates
      res$sim_list <- sim_tmp
      res$flag_error <- FALSE
      res$flag_rqd_res <- TRUE
      res$simulate <- FALSE
      return(res)
    }

    req_dates_idx <- sim_tmp$Date %in% req_date_list
    inter_dates <- sim_tmp$Date[req_dates_idx]

    ## Select requested dates
    if (any(req_dates_idx)) {

      ## In case some dates are not simulated, warn the user
      if (length(inter_dates) < length(req_date_list)) {
        missing_dates <- req_date_list[!req_date_list %in% inter_dates]
        if (verbose) {
          res$message <- warning(paste(
            "Requested date(s)",
            paste(missing_dates, collapse = ", "),
            "is(are) not simulated for USM", situation
          ))
        }
        res$flag_error <- FALSE
        res$flag_rqd_res <- FALSE
      }

      sim_tmp <- sim_tmp[req_dates_idx, ]
      res$sim_list <- sim_tmp
      res$simulate <- FALSE
    } else { ## not any required date simulated => return NULL
      res$message <- warning(paste(
        "Not any requested date(s)",
        paste(req_date_list, collapse = ", "),
        "is simulated for USM", situation
      ))
      res$sim_list <- NULL
      res$flag_error <- TRUE
      res$flag_rqd_res <- FALSE
      res$simulate <- FALSE
    }

    return(res)
  }
}




#' @title Getting/setting a stics_wrapper options list with initialized fields
#'
#' @description This function returns a default options list if called with no
#' arguments, or a pre-formated model option list with checks on the inputs.
#'
#' @param javastics Path of JavaStics. Optional, needed if stics_exe is not
#' provided, or if stics_exe relates to an exe in the javastics_path
#' (see details)
#' @param stics_exe The name, executable or path of the stics executable to use
#' (optional, default to "modulostics", see details)
#' @param workspace Path of the workspace containing the Stics (txt) input files
#'  or path of a single directory containing one sub-folder per USM (named as
#'  the USM names) with Stics (txt) input files in them.
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores    Number of cores to use for parallel computation.
#' @param time_display Display time
#' @param verbose Logical value (optional), `TRUE` to display informations
#' during execution, `FALSE` otherwise (default)
#' @param force    Boolean. Don't check `javastics`, `stics_exe` and `workspace`
#'  (default to `FALSE`, see details)
#' @param successive List of vectors containing the names of the UMSs to
#' consider as successive
#' (e.g. list(c("usm1.1","usm1.2"),c("usm2.1","usm2.2")) defines 2 successions
#' usm1.1->usm1.2 and usm2.1->usm2.2)
#' @param ... Add further arguments set the options list values
#' @param data_dir `r lifecycle::badge("deprecated")` `data_dir` is no
#'   longer supported, use `workspace` instead.
#' @param javastics_path `r lifecycle::badge("deprecated")` `javastics_path`
#' is no longer supported, use `javastics` instead.
#' @param successive_usms `r lifecycle::badge("deprecated")` `successive_usms`
#' is no longer supported, use `successive` instead.
#'
#' @details `stics_exe` may be :
#' 1. a model name pointing to a stics executable as done in JavaStics, e.g.
#' "modulostics" for `stics_modulo.exe`, the standard version of the model
#' shipping with JavaStics;
#' 2. a stics executable file available from the bin folder in JavaStics,
#' e.g. "stics_modulo.exe";
#' 3. a path to a stics executable file,
#' eg. "C:/Users/username/Desktop/stics.exe".
#'
#' `javastics` must be provided for case (1) and (2).
#'
#' If `force=TRUE`, checks are not done for `javastics`, `stics_exe` and
#' `workspace`. In this case, they are returned as is, and will be checked
#'  (and potentially updated to match the right stics executable) only at
#'  execution of `stics_wrapper()`. This option is used for portability,
#'  when e.g. `stics_wrapper_options()` outputs are sent to a remote.
#'
#' @return A list containing Stics model stics_wrapper options
#'
#' @examples
#' \dontrun{
#' # Getting simulations options and defaults values for the stics_wrapper
#' # function
#'
#' stics_wrapper_options()
#'
#' #> $javastics
#' #> [1] "unknown"
#' #>
#' #> $stics_exe
#' #> [1] "modulostics"
#' #>
#' #> $workspace
#' #> [1] "unknown"
#' #>
#' #> $parallel
#' #> [1] FALSE
#' #>
#' #> $cores
#' #> [1] NA
#' #>
#' #> $time_display
#' #> [1] FALSE
#' #>
#' #> $verbose
#' #> [1] TRUE
#' #>
#' #> $force
#' #> [1] FALSE
#'
#' # Setting mandatory simulations options
#' javastics <- "path/to/javastics"
#' data_path <- "path/to/data_directory"
#' sim_options <- stics_wrapper_options(
#'   javastics = javastics,
#'   workspace = data_path
#' )
#'
#' # Changing default values (e.g. parallel):
#' sim_options <- stics_wrapper_options(
#'   javastics = javastics,
#'   workspace = data_path, parallel = TRUE
#' )
#'
#' #> $javastics
#' #> [1] "path/to/JavaSTICS-v85"
#' #>
#' #> $stics_exe
#' #> [1] "path/to/JavaSTICS-v85/bin/stics_modulo.exe"
#' #>
#' #> $workspace
#' #> [1] "path/to/data"
#' #>
#' #> $parallel
#' #> [1] TRUE
#' #>
#' #> $cores
#' #> [1] NA
#' #>
#' #> $time_display
#' #> [1] FALSE
#' #>
#' #> $verbose
#' #> [1] TRUE
#' #>
#' #> $force
#' #> [1] FALSE
#'
#' # Using the `force` argument to keep the inputs as is:
#' sim_options <- stics_wrapper_options(
#'   javastics = javastics,
#'   data_dir = data_path, force = TRUE
#' )
#'
#' #> $javastics
#' #> [1] "path/to/JavaSTICS-v85"
#' #>
#' #> $stics_exe
#' #> [1] "modulostics"
#' #>
#' #> $workspace
#' #> [1] "path/to/data"
#' #>
#' #> $parallel
#' #> [1] FALSE
#' #>
#' #> $cores
#' #> [1] NA
#' #>
#' #> $time_display
#' #> [1] FALSE
#' #>
#' #> $verbose
#' #> [1] TRUE
#' #>
#' #> $force
#' #> [1] TRUE
#'
#' # This will be checked and modified by a `do.call()` in `stics_wrapper()`:
#' do.call(stics_wrapper_options, model_options)
#'
#' #> $javastics
#' #> [1] "path/to/JavaSTICS-v85"
#' #>
#' #> $stics_exe
#' #> [1] "path/to/JavaSTICS-v85/bin/stics_modulo.exe"
#' #>
#' #> $workspace
#' #> [1] "path/to/data"
#' #>
#' #> $parallel
#' #> [1] FALSE
#' #>
#' #> $cores
#' #> [1] NA
#' #>
#' #> $time_display
#' #> [1] FALSE
#' #>
#' #> $verbose
#' #> [1] TRUE
#' #>
#' #> $force
#' #> [1] FALSE
#'
#' # Note the `stics_exe` path that was modified and checked to the path were
#' # it was found.
#' }
#'
#' @export
#'
#'
stics_wrapper_options <- function(javastics = NULL,
                                  stics_exe = "modulostics",
                                  workspace = NULL,
                                  parallel = FALSE,
                                  cores = NA,
                                  time_display = FALSE,
                                  verbose = TRUE,
                                  force = FALSE,
                                  successive = NULL,
                                  javastics_path = lifecycle::deprecated(),
                                  data_dir = lifecycle::deprecated(),
                                  successive_usms = lifecycle::deprecated(),
                                  ...) {
  if (lifecycle::is_present(successive_usms)) {
    lifecycle::deprecate_warn(
      "1.0.0", "stics_wrapper_options(successive_usms)",
      "stics_wrapper_options(successive)"
    )
    successive <- successive_usms
  }
  if (lifecycle::is_present(data_dir)) {
    lifecycle::deprecate_warn(
      "1.0.0", "stics_wrapper_options(data_dir)",
      "stics_wrapper_options(workspace)"
    )
    workspace <- data_dir
  }
  if (lifecycle::is_present(javastics_path)) {
    lifecycle::deprecate_warn(
      "1.0.0", "stics_wrapper_options(javastics_path)",
      "stics_wrapper_options(javastics)"
    )
    javastics <- javastics_path
  }
  options <- list()
  # To get a template, run the function without arguments:
  if (!nargs()) {
    # Template list
    options$javastics <- "unknown"
    options$stics_exe <- "unknown"
    options$workspace <- "unknown"
    options$parallel <- FALSE
    options$cores <- NA
    options$time_display <- FALSE
    options$verbose <- TRUE
    options$successive <- NULL
    options$force <- FALSE
    return(options)
  }

  if (force) {
    # Forced, no checks on the arguments.
    options$javastics <- javastics
    options$stics_exe <- stics_exe
    options$workspace <- workspace
    options$parallel <- parallel
    options$cores <- cores
    options$time_display <- time_display
    options$verbose <- verbose
    options$successive <- successive
    options$force <- force
    return(options)
  }

  if (is.null(workspace)) {
    stop("The workspace argument is mandatory")
  }

  # Help people that don't remember well the standard name:
  if (stics_exe == "stics_modulo" || stics_exe == "sticsmodulo") {
    stics_exe <- "modulostics"
  }

  # Getting right executable name for the platform
  if (stics_exe == "modulostics") {
    # using the exe name instead of the identifier to select the right one
    # for the user's OS
    stics_exe <- paste0("stics_modulo", SticsRFiles:::os_suffix())
  }

  if (!is.null(javastics)) {
    # Checking javastics path if present
    SticsRFiles:::check_java_path(javastics)
  }

  # Case 1: stics_exe is a model name present in the preference file:
  if (!is.null(javastics) && exist_stics_exe(javastics, stics_exe)) {
    stics_exe <- file.path(
      javastics,
      list_stics_exe(javastics)$stics_list[stics_exe][[1]]
    )
  } else if (!is.null(javastics) &&
             check_stics_exe(
               model_path = file.path(
                 javastics, "bin",
                 basename(stics_exe)
               ),
               stop = FALSE
             )) {
    # Case 2: stics_exe is an executable from the bin directory in JavaStics:
    stics_exe <- file.path(javastics, "bin", basename(stics_exe))
  } else if (!check_stics_exe(model_path = stics_exe, stop = FALSE)) {
    # Case were stics_exe was not found in case 1 and 2, and is not a valid
    # path to an executable either:
    stop(
      "stics_exe was not found as a stics name,
         executable in the bin path of JavaStics nor executable path: ",
      stics_exe
    )
    # NB: case 3 (i.e. stics_exe is a full path to an executable) is implicit
    # here: it is the case where
  }

  if (verbose) cli::cli_alert_success("Using stics: {.val {stics_exe}}")

  # Adding arguments values to the option list:
  if (!is.null(javastics)) options$javastics <- javastics
  if (!is.null(stics_exe)) options$stics_exe <- stics_exe
  if (!is.null(workspace)) options$workspace <- workspace
  if (!is.null(parallel)) options$parallel <- parallel
  if (!is.null(cores)) options$cores <- cores
  if (!is.null(time_display)) options$time_display <- time_display
  if (!is.null(verbose)) options$verbose <- verbose
  if (!is.null(successive)) options$successive <- successive
  options$force <- force

  # Adding future-proof optional fields:
  dot_args <- list(...)
  options <- c(options, dot_args)

  return(options)
}


stics_display_warnings <- function(in_string) {
  if (nchar(in_string)) warning(in_string, call. = FALSE)
}
