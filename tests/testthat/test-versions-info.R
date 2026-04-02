test_that("Get the version hash, date from the exe info", {
  version_line <- " b09f41236_2026-02-17"
  testthat::expect_equal(
    extract_version_hash(version_line),
    "b09f41236"
  )
  testthat::expect_equal(
    extract_version_date(version_line),
    "2026-02-17"
  )
  version_line <- " stics_v10.4.1_2025-07-30"
  testthat::expect_equal(
    extract_version_hash(version_line),
    NA
  )
  testthat::expect_equal(
    extract_version_date(version_line),
    "2025-07-30"
  )
  version_line <- " Modulostics version : V10.0.0_r3391_2022-10-26"
  testthat::expect_equal(
    extract_version_hash(version_line),
    NA
  )
  testthat::expect_equal(
    extract_version_date(version_line),
    "2022-10-26"
  )
})

test_that("Get the full version from number or string", {
  testthat::expect_equal(complete_version(10.5), "10.5.0")
  testthat::expect_equal(complete_version("10.5"), "10.5.0")
  testthat::expect_equal(complete_version("10.5.0"), "10.5.0")
})

test_that("Get the version hash from the executable info string", {
  version_line <- " b09f41236_2026-02-17"
  testthat::expect_equal(get_version(version_line), "b09f41236")
  testthat::expect_equal(
    get_version(version_line, numeric = FALSE),
    "b09f41236"
  )
  version_line <- " stics_v10.4.1_2025-07-30"
  num_version <- semver::parse_version("10.4.1")
  testthat::expect_equivalent(get_version(version_line), num_version)
  testthat::expect_equivalent(
    get_version(version_line, numeric = FALSE),
    "10.4.1"
  )
  version_line <- " Modulostics version : V10.0.0_r3391_2022-10-26"
  num_version <- semver::parse_version("10.0.0")
  testthat::expect_equivalent(get_version(version_line), num_version)
  testthat::expect_equivalent(
    get_version(version_line, numeric = FALSE),
    "10.0.0"
  )
  version_line <- "   "
  testthat::expect_error(get_version(version_line))
})
