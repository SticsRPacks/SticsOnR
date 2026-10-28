# Helpers used to test the JavaStics executables management functions
# without any JavaStics installation.

#' Create a fake JavaStics installation directory
#'
#' Creates the `bin` and `config` sub-directories and a minimal
#' `preferences.xml` file holding two stics executables, as JavaStics does.
#' `SticsRFiles:::check_java_path()` is mocked so that the directory is
#' accepted as a JavaStics installation.
#'
#' @param current Identifier of the executable used by JavaStics.
#' @param envir Environment the mocks and directory life cycle is bound to.
#'
#' @return The path of the created directory.
#'
#' @noRd
local_fake_javastics_dir <- function(
  current = "modulostics",
  envir = parent.frame()
) {
  javastics <- tempfile("fake_javastics_")
  dir.create(file.path(javastics, "bin"), recursive = TRUE)
  dir.create(file.path(javastics, "config"), recursive = TRUE)

  # NB: no DOCTYPE here, to avoid any external DTD fetch while parsing
  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
      "<properties>",
      "<comment>Javastics user preferences</comment>",
      "<entry key=\"workingDirectory.current\">example</entry>",
      paste0("<entry key=\"model.last\">", current, "</entry>"),
      paste0(
        "<entry key=\"model.list\">{modulostics\tstics_modulo},",
        "{stics_custom_test\tstics_custom},</entry>"
      ),
      "</properties>"
    ),
    file.path(javastics, "config", "preferences.xml")
  )

  # The fake directory is not a real JavaStics installation
  testthat::local_mocked_bindings(
    check_java_path = function(...) TRUE,
    .package = "SticsRFiles",
    .env = envir
  )

  cleanup <- substitute(unlink(dir, recursive = TRUE), list(dir = javastics))
  do.call(on.exit, list(cleanup, add = TRUE), envir = envir)

  javastics
}

#' Create a fake stics executable
#'
#' A shell script printing a Stics-like version line, used in place of the
#' model executable (POSIX systems only).
#'
#' @param name File name of the executable.
#' @param version_line Line printed by the fake executable.
#' @param dir Directory the file is created in.
#' @param envir Environment the file life cycle is bound to.
#'
#' @return The path of the created file.
#'
#' @noRd
local_fake_stics_exe <- function(
  name = "stics_test.exe",
  version_line = " stics_v10.4.1_2025-07-30",
  dir = tempdir(),
  envir = parent.frame()
) {
  exe <- file.path(dir, name)

  writeLines(c("#!/bin/sh", paste0("echo \"", version_line, "\"")), exe)
  Sys.chmod(exe, mode = "0755")

  cleanup <- substitute(unlink(file), list(file = exe))
  do.call(on.exit, list(cleanup, add = TRUE), envir = envir)

  exe
}

#' Read the identifier of the executable currently used in a preferences file
#'
#' @param javastics Path of the (fake) JavaStics installation.
#'
#' @return The `model.last` entry value, as a character string.
#'
#' @noRd
read_current_stics_name <- function(javastics) {
  pref <- readLines(file.path(javastics, "config", "preferences.xml"))
  line <- grep("model.last", pref, value = TRUE)
  sub(".*<entry key=\"model.last\">(.*)</entry>.*", "\\1", line)
}
