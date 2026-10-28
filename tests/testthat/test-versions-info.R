test_that("Get the version hash or date, date from the exe info", {
  version_line <- " b09f41236_2026-02-17"
  expect_equal(extract_version_hash(version_line), "b09f41236")
  expect_equal(extract_version_date(version_line), "2026-02-17")

  version_line <- " stics_v10.4.1_2025-07-30"
  expect_equal(extract_version_hash(version_line), NA_character_)
  expect_equal(extract_version_date(version_line), "2025-07-30")

  version_line <- " Modulostics version : V10.0.0_r3391_2022-10-26"
  expect_equal(extract_version_hash(version_line), NA_character_)
  expect_equal(extract_version_date(version_line), "2022-10-26")
})

test_that("Get the version label from the exe info", {
  expect_equal(extract_version_label(" test_name_2026-04-12"), "test_name")
  expect_equal(extract_version_label(" test_name"), NA_character_)
  expect_equal(extract_version_label(" _2026-11-25"), NA_character_)
})


test_that("Get the full version from number or string", {
  expect_equal(complete_version(10.5), "10.5.0")
  expect_equal(complete_version("10.5"), "10.5.0")
  expect_equal(complete_version("10.5.0"), "10.5.0")
})

test_that("Get the version from the executable info string", {
  # NB: semver versions embed an external pointer, so they are compared
  # through their character representation.
  version_line <- " b09f41236_2026-02-17"
  expect_equal(as.character(get_version(version_line)), "b09f41236")
  expect_equal(
    as.character(get_version(version_line, numeric = FALSE)),
    "b09f41236"
  )

  version_line <- " stics_v10.4.1_2025-07-30"
  expect_s3_class(get_version(version_line), "svlist")
  expect_equal(as.character(get_version(version_line)), "10.4.1")
  expect_equal(
    as.character(get_version(version_line, numeric = FALSE)),
    "10.4.1"
  )

  version_line <- " Modulostics version : V10.0.0_r3391_2022-10-26"
  expect_equal(as.character(get_version(version_line)), "10.0.0")
  expect_equal(
    as.character(get_version(version_line, numeric = FALSE)),
    "10.0.0"
  )

  version_line <- "v11.0.0-rc2_2026-06-11"
  expect_equal(as.character(get_version(version_line)), "11.0.0-rc2")
  expect_equal(
    as.character(get_version(version_line, numeric = FALSE)),
    "11.0.0-rc2"
  )

  version_line <- "test_named_2026-06-11"
  expect_equal(
    as.character(get_version(version_line, numeric = FALSE)),
    "test_named"
  )

  version_line <- "   "
  expect_equal(get_version(version_line), NA)
})


test_that("Get the version date from the version object", {
  version_line <- " b09f41236_2026-02-17"
  expect_equal(
    get_version_date(get_version(version_line)),
    as.Date("2026-02-17")
  )

  version_line <- " stics_v10.4.1_2025-07-30"
  expect_equal(
    get_version_date(get_version(version_line)),
    as.Date("2025-07-30")
  )

  version_line <- " Modulostics version : V10.0.0_r3391_2022-10-26"
  expect_equal(
    get_version_date(get_version(version_line)),
    as.Date("2022-10-26")
  )
})
