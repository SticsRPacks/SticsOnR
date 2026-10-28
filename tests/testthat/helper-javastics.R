# Helpers used to test run_javastics() without any JavaStics installation.

#' Mock everything run_javastics() needs outside of the package
#'
#' Replaces the JavaStics and SticsRFiles calls of `run_javastics()` by
#' in-memory stubs, so that the function can be run on any machine, without
#' JavaStics, Java, nor the Stics executable.
#'
#' @param usms_list USM names the `usms.xml` file is supposed to hold.
#' @param workspace_path Path returned by `check_java_workspace()`, `NULL` to
#' simulate an invalid workspace.
#' @param command,cmd_string Command and arguments string returned by
#' `get_javastics_cmd()`.
#' @param run_system_out Value returned by the mocked `run_system()`.
#' @param envir Environment the mocks life cycle is bound to.
#'
#' @return An environment in which the arguments received by the mocked
#' functions are stored (`set_stics_exe`, `get_javastics_cmd`, `run_system`).
#'
#' @noRd
local_javastics_mocks <- function(
  usms_list = c("wheat", "maize"),
  workspace_path = NULL,
  command = "echo",
  cmd_string = "run",
  run_system_out = list(list(name = "usm", error = FALSE, message = "ok")),
  envir = parent.frame()
) {
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    check_java_path = function(...) TRUE,
    os_suffix = function(...) "_test",
    check_java_workspace = function(javastics, workspace, ...) workspace_path,
    get_usms_list = function(...) usms_list,
    get_javastics_cmd = function(javastics, java_cmd, type, workspace, ...) {
      calls$cmd_type <- type
      calls$java_cmd <- java_cmd
      calls$cmd_workspace <- workspace
      list(command, cmd_string)
    },
    .package = "SticsRFiles",
    .env = envir
  )

  testthat::local_mocked_bindings(
    set_stics_exe = function(javastics, stics_exe, ...) {
      calls$stics_exe <- c(calls$stics_exe, stics_exe)
      invisible()
    },
    list_stics_exe = function(javastics) {
      list(current = list("previous_stics"))
    },
    run_system = function(stics_exe, workspace, ...) {
      calls$run_system_exe <- stics_exe
      calls$run_system_workspace <- workspace
      invisible(run_system_out)
    },
    .package = "SticsOnR",
    .env = envir
  )

  calls
}

#' Create a fake JavaStics command line executable
#'
#' The script echoes the arguments it receives, and optionally creates a
#' `modhistory.sti` file in the given workspace, as JavaStics does.
#'
#' @param history_dir Directory the history file must be created in, or `NULL`
#' for no history file.
#' @param envir Environment the file life cycle is bound to.
#'
#' @return The path of the created script.
#'
#' @noRd
local_fake_javastics_cmd <- function(history_dir = NULL, envir = parent.frame()) {
  script <- tempfile("fake_javastics_", fileext = ".sh")

  lines <- c("#!/bin/sh", "echo \"$@\"")
  if (!is.null(history_dir)) {
    lines <- c(lines, paste0("touch \"", history_dir, "/modhistory.sti\""))
  }

  writeLines(lines, script)
  Sys.chmod(script, mode = "0755")

  cleanup <- substitute(unlink(file), list(file = script))
  do.call(on.exit, list(cleanup, add = TRUE), envir = envir)

  script
}
