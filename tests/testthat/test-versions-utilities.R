# Additional tests on the version parsing helpers, focusing on edge cases
# and on the exported `get_version_number()` behaviour.

test_that("extract_version_string gets semver strings, whatever the case", {
  expect_equal(extract_version_string("stics_v10.4.1_2025-07-30"), "10.4.1")
  expect_equal(extract_version_string("V10.0.0_r3391_2022-10-26"), "10.0.0")
  expect_equal(extract_version_string("v11.0.0-rc2_2026-06-11"), "11.0.0-rc2")
  # No semver-like string in the line
  expect_equal(extract_version_string("b09f41236_2026-02-17"), NA_character_)
  expect_equal(extract_version_string("10.4_2025-07-30"), NA_character_)
  expect_equal(extract_version_string(""), NA_character_)
})

test_that("extract_version_hash only matches 8 or 9 alnum characters", {
  expect_equal(extract_version_hash("abcdef12_2026-02-17"), "abcdef12")
  expect_equal(extract_version_hash("abcdef123_2026-02-17"), "abcdef123")
  # Too short (7) or too long (10) to be considered as a hash
  expect_equal(extract_version_hash("abcdef1_2026-02-17"), NA_character_)
  expect_equal(extract_version_hash("abcdef1234_2026-02-17"), NA_character_)
  # Not followed by an underscore
  expect_equal(extract_version_hash("abcdef12-2026-02-17"), NA_character_)
  expect_equal(extract_version_hash(""), NA_character_)
})

test_that("extract_version_date only matches ISO dates", {
  expect_equal(extract_version_date("v10.4.1_2025-07-30"), "2025-07-30")
  expect_equal(extract_version_date("no date here"), NA)
  expect_equal(extract_version_date("2025-7-30"), NA)
  expect_equal(extract_version_date(""), NA)
})

test_that("extract_version_label needs a trailing date", {
  expect_equal(extract_version_label("test_name_2026-04-12"), "test_name")
  # Labels are lower cased
  expect_equal(extract_version_label("Test_Name_2026-04-12"), "test_name")
  expect_equal(extract_version_label("test_name"), NA_character_)
})

test_that("complete_version completes partial versions", {
  expect_equal(complete_version(10), "10.0.0")
  expect_equal(complete_version("10"), "10.0.0")
  expect_equal(complete_version(10.5), "10.5.0")
  expect_equal(complete_version("10.5"), "10.5.0")
  expect_equal(complete_version("10.5.0"), "10.5.0")
  # Already complete versions, with a pre-release tag, are left untouched
  expect_equal(complete_version("11.0.0-rc2"), "11.0.0-rc2")
  # Missing or empty versions
  expect_equal(complete_version(NA), NA)
  expect_equal(complete_version(""), NA)
})

test_that("get_version attaches the version date as an attribute", {
  # Hash case
  version <- get_version(" b09f41236_2026-02-17")
  expect_equal(attr(version, "date"), as.Date("2026-02-17"))
  expect_equal(get_version_date(version), as.Date("2026-02-17"))

  # Numeric version case
  version <- get_version(" stics_v10.4.1_2025-07-30")
  expect_s3_class(version, "svlist")
  expect_equal(attr(version, "date"), as.Date("2025-07-30"))

  # Character version case
  version <- get_version(" stics_v10.4.1_2025-07-30", numeric = FALSE)
  expect_type(version, "character")
  expect_equal(as.character(version), "10.4.1")

  # Label case
  version <- get_version("test_named_2026-06-11", numeric = FALSE)
  expect_equal(get_version_date(version), as.Date("2026-06-11"))
})

test_that("get_version returns NA when no date is found", {
  expect_equal(get_version("no version at all"), NA)
  expect_equal(get_version(""), NA)
  expect_equal(get_version("   "), NA)
})

test_that("get_version_date returns NA without a date attribute", {
  expect_equal(get_version_date(NA), NA)
  expect_equal(get_version_date("10.4.1"), NA)
})

test_that("get_version_number warns on a missing executable", {
  missing_exe <- file.path(tempdir(), "no_such_stics_exe")
  expect_false(file.exists(missing_exe))

  expect_warning(
    version <- get_version_number(missing_exe),
    regexp = "doesn't exist"
  )
  expect_equal(version, NA)

  expect_warning(
    date <- get_exe_date(missing_exe),
    regexp = "doesn't exist"
  )
  expect_equal(date, NA)
})
