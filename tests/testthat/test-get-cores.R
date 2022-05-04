context("Test getting cores number")
library(SticsOnR)
library(parallel)


test_that("Without parallel", {
  # always 1 returned cores nb
  # whithout or with required_nb
  expect_equal(SticsOnR:::get_cores_nb(), 1)
  expect_equal(SticsOnR:::get_cores_nb(required_nb = 2), 1)
  expect_equal(SticsOnR:::get_cores_nb(required_nb = 4), 1)
})

test_that("With parallel", {
  # Getting machine cores number
  machine_cores <- detectCores()
  expect_equal(SticsOnR:::get_cores_nb(parallel = TRUE), machine_cores - 1)

  # with required over cores number - 1
  expect_equal(
    SticsOnR:::get_cores_nb(
      parallel = TRUE,
      required_nb = machine_cores
    ),
    machine_cores - 1
  )
  expect_equal(
    SticsOnR:::get_cores_nb(
      parallel = TRUE,
      required_nb = machine_cores + 1
    ),
    machine_cores - 1
  )

  # with required below cores number
  expect_equal(
    SticsOnR:::get_cores_nb(
      parallel = TRUE,
      required_nb = machine_cores - 1
    ),
    machine_cores - 1
  )
  expect_equal(
    SticsOnR:::get_cores_nb(
      parallel = TRUE,
      required_nb = machine_cores - 2
    ),
    machine_cores - 2
  )
})

test_that("With forcing cores number", {
  #
  # with required
  # ' required < available
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 1,
    cores_nb = 2
  ), 1)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 2,
    cores_nb = 3
  ), 2)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 3,
    cores_nb = 4
  ), 3)
  # ' required == available
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 1,
    cores_nb = 1
  ), 1)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 2,
    cores_nb = 2
  ), 1)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 3,
    cores_nb = 3
  ), 2)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 4,
    cores_nb = 4
  ), 3)
  # ' required > available
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 2,
    cores_nb = 1
  ), 1)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 3,
    cores_nb = 2
  ), 1)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 4,
    cores_nb = 3
  ), 2)
  expect_equal(SticsOnR:::get_cores_nb(
    parallel = TRUE, required_nb = 5,
    cores_nb = 4
  ), 3)
})
