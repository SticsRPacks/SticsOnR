# Helpers used to test stics_wrapper() without the Stics model: the model run
# and the SticsRFiles functions generating/reading the text files are mocked.

#' Create a workspace holding fake USM directories
#'
#' Each USM directory holds a `new_travail.usm` file, which is what
#' `stics_wrapper()` looks for to detect a Stics USM directory.
#'
#' @param usms Names of the USM directories to create.
#' @param envir Environment the directory life cycle is bound to.
#'
#' @return The path of the created workspace.
#'
#' @noRd
local_wrapper_workspace <- function(
  usms = c("usm1", "usm2"),
  envir = parent.frame()
) {
  workspace <- local_test_workspace(envir = envir)

  for (usm in usms) {
    dir.create(file.path(workspace, usm))
    writeLines("fake usm file", file.path(workspace, usm, "new_travail.usm"))
  }

  workspace
}

#' A simulated results table, as returned by SticsRFiles::get_sim()
#'
#' @param dates Dates of the simulated days.
#' @param vars Named list of simulated variables.
#'
#' @return A tibble holding the `Date`, `Plant` and `Dominance` columns plus
#' the given variables.
#'
#' @noRd
fake_sim_table <- function(
  dates = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
  vars = list(lai_n = c(0.1, 0.2, 0.3), masec_n = c(1.1, 1.2, 1.3))
) {
  tibble::as_tibble(c(
    list(
      Date = dates,
      Plant = rep("plant1", length(dates)),
      Dominance = rep("Principal", length(dates))
    ),
    vars
  ))
}

#' Mock everything stics_wrapper() needs to run without Stics
#'
#' @param sim Simulated results returned in place of
#'   `SticsRFiles::get_sim()`. Either a table, `NULL` (to mimic a reading
#'   error) or a function of the USM directory returning one of both.
#' @param error Logical (recycled over the USMs) telling if the model run must
#'   report an error.
#' @param force_ok Value returned by `SticsRFiles::force_param_values()`.
#' @param codesnow Named list giving the `codesnow` parameter value per USM
#'   directory name, `0` by default.
#' @param create_recup Logical, `TRUE` to make the fake model run create a
#'   `recup.tmp` file, as Stics does for successive USMs.
#' @param create_snow Logical, `TRUE` to make the fake model run create a
#'   `snow_variables.txt` file, as Stics does when the snow module is used.
#' @param envir Environment the mocks are bound to.
#'
#' @return An environment holding what the mocks received: `run_dirs`,
#' `forced` (the forced parameters values), `varmod` (the variables added in
#' var.mod) and `codesuite` (the values set in new_travail.usm).
#'
#' @noRd
local_wrapper_mocks <- function(
  sim = fake_sim_table(),
  error = FALSE,
  force_ok = TRUE,
  codesnow = list(),
  create_recup = FALSE,
  create_snow = FALSE,
  envir = parent.frame()
) {
  calls <- new.env(parent = emptyenv())
  calls$run_dirs <- character(0)
  calls$forced <- list()
  calls$varmod <- list()
  calls$codesuite <- numeric(0)

  get_sim_value <- function(run_dir) {
    if (is.function(sim)) sim(run_dir) else sim
  }

  testthat::local_mocked_bindings(
    run_stics = function(stics_exe, run_dir, verbose = FALSE, check = TRUE) {
      calls$run_dirs <- c(calls$run_dirs, basename(run_dir))
      if (create_recup) {
        file.create(file.path(run_dir, "recup.tmp"))
      }
      if (create_snow) {
        file.create(file.path(run_dir, "snow_variables.txt"))
      }
      run_nb <- length(calls$run_dirs)
      is_error <- error[[min(run_nb, length(error))]]
      list(list(
        name = basename(run_dir),
        error = is_error,
        message = if (is_error) "Stics stopped" else ""
      ))
    },
    .env = envir
  )

  testthat::local_mocked_bindings(
    force_param_values = function(run_dir, values, javastics, ...) {
      calls$forced <- c(calls$forced, list(values))
      force_ok
    },
    get_sim = function(workspace, ...) list(get_sim_value(workspace)),
    gen_varmod = function(workspace, var, ...) {
      calls$varmod <- c(calls$varmod, list(var))
      invisible(TRUE)
    },
    get_param_txt = function(workspace, param, ...) {
      usm <- basename(workspace)
      value <- if (is.null(codesnow[[usm]])) 0 else codesnow[[usm]]
      list(codesnow = value)
    },
    set_usm_txt = function(file, param, value, ...) {
      calls$codesuite <- c(calls$codesuite, value)
      invisible(TRUE)
    },
    .package = "SticsRFiles",
    .env = envir
  )

  calls
}

#' Evaluate an expression, collecting all the warnings it emits
#'
#' `stics_wrapper()` stores the warnings met while simulating and displays
#' them again at the end of the run (`stics_display_warnings()`), so a given
#' message is emitted twice: `expect_warning()` would only catch the first
#' one and let the second escape the test.
#'
#' @param expr Expression to evaluate.
#'
#' @return A list holding the `value` of the expression and the `warnings`
#' messages.
#'
#' @noRd
run_with_warnings <- function(expr) {
  warns <- character(0)

  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(value = value, warnings = warns)
}

#' Check that one of the collected warnings matches a pattern
#'
#' @param warnings Warning messages, as collected by `run_with_warnings()`.
#' @param regexp Pattern the messages are matched against.
#'
#' @noRd
expect_warned <- function(warnings, regexp) {
  testthat::expect_true(
    any(grepl(regexp, warnings)),
    info = paste0(
      "no warning matching '",
      regexp,
      "' among: ",
      paste(warnings, collapse = " | ")
    )
  )
}
