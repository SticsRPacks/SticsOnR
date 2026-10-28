# Tests on the management of the stics executables listed in the JavaStics
# preferences file. A fake JavaStics directory holding a real (minimal)
# preferences.xml file is used, so the XML reading and writing code is really
# exercised (see helper-stics-exe.R).

test_that("list_stics_exe reads the executables from the preferences", {
  javastics <- local_fake_javastics_dir(current = "modulostics")

  stics_exe <- list_stics_exe(javastics)

  expect_named(stics_exe, c("stics_list", "current"))
  expect_equal(
    stics_exe$stics_list,
    list(modulostics = "stics_modulo", stics_custom_test = "stics_custom")
  )
  expect_equal(stics_exe$current, list(modulostics = "stics_modulo"))
})

test_that("exist_stics_exe tells if an executable is listed", {
  javastics <- local_fake_javastics_dir()

  expect_true(exist_stics_exe(javastics, "modulostics"))
  expect_true(exist_stics_exe(javastics, "stics_custom_test"))
  expect_false(exist_stics_exe(javastics, "unknown_stics"))
})

test_that("select_stics_exe stops on an unknown executable", {
  javastics <- local_fake_javastics_dir()

  expect_error(
    select_stics_exe(javastics, "unknown_stics"),
    regexp = "doesn't exist in this configuration"
  )
})

test_that("select_stics_exe does nothing if already selected", {
  javastics <- local_fake_javastics_dir(current = "modulostics")

  # "stics_modulo" and "sticsmodulo" are synonyms of "modulostics"
  expect_null(select_stics_exe(javastics, "stics_modulo"))
  expect_equal(read_current_stics_name(javastics), "modulostics")
  # NB: the backup copies are made before the check, so they exist even
  # though the preferences file has not been modified
  expect_true(
    file.exists(file.path(javastics, "config", "preferences_prev.xml"))
  )
})

test_that("select_stics_exe selects another executable and backs up the file", {
  javastics <- local_fake_javastics_dir(current = "modulostics")

  select_stics_exe(javastics, "stics_custom_test")

  expect_equal(read_current_stics_name(javastics), "stics_custom_test")
  expect_equal(
    list_stics_exe(javastics)$current,
    list(stics_custom_test = "stics_custom")
  )
  # The original and the previous preferences files are kept
  expect_true(
    file.exists(file.path(javastics, "config", "preferences_ori.xml"))
  )
  expect_true(
    file.exists(file.path(javastics, "config", "preferences_prev.xml"))
  )
})

test_that("remove_stics_exe warns on an unknown executable", {
  javastics <- local_fake_javastics_dir()

  expect_warning(
    remove_stics_exe(javastics, "unknown_stics"),
    regexp = "doesn't exists or his name is miss spelled"
  )

  # Nothing has been removed
  expect_length(list_stics_exe(javastics)$stics_list, 2)
})

test_that("remove_stics_exe removes an executable from the list", {
  javastics <- local_fake_javastics_dir(current = "modulostics")

  remove_stics_exe(javastics, "stics_custom_test")

  stics_list <- list_stics_exe(javastics)$stics_list
  expect_equal(stics_list, list(modulostics = "stics_modulo"))
  # The selected executable is untouched
  expect_equal(read_current_stics_name(javastics), "modulostics")
})

test_that("remove_stics_exe only removes the given executable", {
  # A model whose name contains the name of the removed one
  javastics <- local_fake_javastics_dir(
    current = "modulostics",
    models = c(
      modulostics = "stics_modulo",
      stics_custom = "stics_custom",
      stics_custom_2 = "stics_custom_2"
    )
  )

  remove_stics_exe(javastics, "stics_custom")

  expect_equal(
    list_stics_exe(javastics)$stics_list,
    list(modulostics = "stics_modulo", stics_custom_2 = "stics_custom_2")
  )
})

test_that("remove_stics_exe unsets the executable used by JavaStics", {
  javastics <- local_fake_javastics_dir(current = "stics_custom_test")

  expect_warning(
    remove_stics_exe(javastics, "stics_custom_test"),
    regexp = "JavaStics was using this Stics executable"
  )

  expect_equal(
    list_stics_exe(javastics)$stics_list,
    list(modulostics = "stics_modulo")
  )
  # No model is selected anymore
  expect_equal(read_current_stics_name(javastics), "")
})

test_that("set_stics_exe stops when the executable is not found", {
  javastics <- local_fake_javastics_dir()

  expect_error(
    set_stics_exe(javastics, "unknown_stics", verbose = FALSE),
    regexp = "was not found as a stics name"
  )
})

test_that("set_stics_exe selects an already listed executable", {
  javastics <- local_fake_javastics_dir(current = "modulostics")

  expect_null(set_stics_exe(javastics, "stics_custom_test", verbose = FALSE))
  expect_equal(read_current_stics_name(javastics), "stics_custom_test")
})

test_that("set_stics_exe informs the user about the selected executable", {
  javastics <- local_fake_javastics_dir()

  # Both the model name and its executable file name are displayed
  expect_message(
    set_stics_exe(javastics, "stics_custom_test", verbose = TRUE),
    regexp = "stics_custom_test.*stics_custom"
  )
})

test_that("set_stics_exe accepts the standard model name synonyms", {
  # The OS specific model name is used, as in the preferences templates
  os_model_name <- switch(SticsRFiles:::user_os(),
    lin = "modulostics_linux",
    mac = "modulostics_mac",
    "modulostics"
  )
  models <- c("stics_modulo", "stics_custom")
  names(models) <- c(os_model_name, "stics_custom_test")

  for (name in c("stics_modulo", "sticsmodulo")) {
    javastics <- local_fake_javastics_dir(
      current = "stics_custom_test",
      models = models
    )

    set_stics_exe(javastics, name, verbose = FALSE)

    expect_equal(read_current_stics_name(javastics), os_model_name)
  }
})

test_that("set_stics_exe falls back on the generic standard model name", {
  # Only the generic name is listed (e.g. a Windows like configuration)
  javastics <- local_fake_javastics_dir(current = "stics_custom_test")

  set_stics_exe(javastics, "stics_modulo", verbose = FALSE)

  expect_equal(read_current_stics_name(javastics), "modulostics")
})

test_that("set_stics_exe adds a new executable from a path", {
  skip_on_os("windows")

  javastics <- local_fake_javastics_dir(current = "modulostics")
  exe <- local_fake_stics_exe(name = "stics_new_test")

  set_stics_exe(javastics, exe, verbose = FALSE)

  # The executable has been copied in the bin folder of JavaStics ...
  expect_true(file.exists(file.path(javastics, "bin", "stics_new_test")))
  # ... and added in the preferences, named after the user OS, then selected
  new_name <- paste0("stics_new_test_", SticsRFiles:::user_os())
  stics_list <- list_stics_exe(javastics)$stics_list
  expect_equal(stics_list[[new_name]], "stics_new_test")
  expect_equal(read_current_stics_name(javastics), new_name)
})

test_that("set_stics_exe refuses to overwrite the standard stics", {
  skip_on_os("windows")

  javastics <- local_fake_javastics_dir()
  exe_dir <- tempfile("stics_exe_dir_")
  dir.create(exe_dir)
  on.exit(unlink(exe_dir, recursive = TRUE), add = TRUE)
  exe <- local_fake_stics_exe(name = "stics_modulo", dir = exe_dir)

  expect_error(
    set_stics_exe(javastics, exe, verbose = FALSE),
    regexp = "is not allowed"
  )
})

test_that("get_version_number reads the version from the executable", {
  skip_on_os("windows")

  exe <- local_fake_stics_exe(version_line = " stics_v10.4.1_2025-07-30")

  version <- get_version_number(exe)
  expect_equal(as.character(version), "10.4.1")
  expect_equal(attr(version, "date"), as.Date("2025-07-30"))

  expect_equal(
    as.character(get_version_number(exe, numeric = FALSE)),
    "10.4.1"
  )
  expect_equal(get_exe_date(exe), as.Date("2025-07-30"))
})

test_that("get_version_number warns when the output is not a version", {
  skip_on_os("windows")

  # An executable printing nothing at all
  exe <- file.path(tempdir(), "silent_stics_test")
  writeLines(c("#!/bin/sh", "exit 0"), exe)
  Sys.chmod(exe, mode = "0755")
  on.exit(unlink(exe), add = TRUE)

  expect_warning(
    version <- get_version_number(exe),
    regexp = "No output from the executable"
  )
  expect_equal(version, NA)
})

test_that("check_stics_exe returns the version when required", {
  skip_on_os("windows")

  exe <- local_fake_stics_exe(version_line = " stics_v10.4.1_2025-07-30")

  status <- check_stics_exe(exe)
  expect_true(status)

  status <- check_stics_exe(exe, add_version = TRUE)
  expect_true(status)
  expect_equal(as.character(attr(status, "version")), "10.4.1")
  expect_false("output" %in% names(attributes(status)))
})
