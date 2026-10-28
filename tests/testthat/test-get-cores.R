test_that("Without parallel, one single core is returned", {
  # always 1 returned cores nb
  # whithout or with required_nb
  expect_equal(get_cores_nb(), 1)
  expect_equal(get_cores_nb(required_nb = 2), 1)
  expect_equal(get_cores_nb(required_nb = 4), 1)
})

test_that("get_cores returns the machine cores number, or a forced one", {
  expect_equal(get_cores(), parallel::detectCores())
  # cores_nb is only meant for testing purpose, it bypasses detectCores()
  expect_equal(get_cores(cores_nb = 4), 4)
  expect_equal(get_cores(cores_nb = 1), 1)
})

test_that("With parallel, one core is kept free on the machine", {
  # Getting machine cores number.
  # NB: on a single core machine (or runner), no core can be freed,
  # so the tests below would not make sense.
  machine_cores <- parallel::detectCores()
  skip_if(
    machine_cores < 3,
    "At least 3 cores are needed to test the cores number limitation"
  )

  expect_equal(get_cores_nb(parallel = TRUE), machine_cores - 1)

  # with required over cores number - 1
  expect_equal(
    get_cores_nb(parallel = TRUE, required_nb = machine_cores),
    machine_cores - 1
  )
  expect_equal(
    get_cores_nb(parallel = TRUE, required_nb = machine_cores + 1),
    machine_cores - 1
  )

  # with required below cores number
  expect_equal(
    get_cores_nb(parallel = TRUE, required_nb = machine_cores - 1),
    machine_cores - 1
  )
  expect_equal(
    get_cores_nb(parallel = TRUE, required_nb = machine_cores - 2),
    machine_cores - 2
  )
})

test_that("With forcing cores number", {
  #
  # with required
  # ' required < available
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 1,
      cores_nb = 2
    ),
    1
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 2,
      cores_nb = 3
    ),
    2
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 3,
      cores_nb = 4
    ),
    3
  )
  # ' required == available
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 1,
      cores_nb = 1
    ),
    1
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 2,
      cores_nb = 2
    ),
    1
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 3,
      cores_nb = 3
    ),
    2
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 4,
      cores_nb = 4
    ),
    3
  )
  # ' required > available
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 2,
      cores_nb = 1
    ),
    1
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 3,
      cores_nb = 2
    ),
    1
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 4,
      cores_nb = 3
    ),
    2
  )
  expect_equal(
    get_cores_nb(
      parallel = TRUE,
      required_nb = 5,
      cores_nb = 4
    ),
    3
  )
})

test_that("Without required cores number, all available cores are used", {
  expect_equal(get_cores_nb(parallel = TRUE, cores_nb = 4), 3)
  expect_equal(get_cores_nb(parallel = TRUE, cores_nb = 1), 1)
  expect_equal(
    get_cores_nb(parallel = TRUE, required_nb = NA, cores_nb = 8),
    7
  )
})
