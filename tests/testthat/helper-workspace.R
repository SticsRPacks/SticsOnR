# Helpers shared by the test files.

#' Create a temporary Stics workspace, removed when the caller exits
#'
#' @param envir Environment the temporary directory life cycle is bound to,
#' the calling environment (i.e. the `test_that()` block) by default.
#'
#' @return The path of the created directory.
#'
#' @noRd
local_test_workspace <- function(envir = parent.frame()) {
  workspace <- tempfile("stics_workspace_")
  dir.create(workspace, recursive = TRUE)

  # Remove the directory as soon as the calling block is left
  cleanup <- substitute(
    unlink(dir, recursive = TRUE),
    list(dir = workspace)
  )
  do.call(on.exit, list(cleanup, add = TRUE), envir = envir)

  workspace
}
