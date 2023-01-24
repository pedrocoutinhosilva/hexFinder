# Tests for get_cran_info
test_that("get_cran_info", {
  # package does not exist
  expect_error(get_cran_info("notAPackage"))

  # package exists and returns a named list of settings
  expect_named(get_cran_info("rlang"))
})

# Tests for get_metacran_info
test_that("get_metacran_info", {
  # package does not exist
  expect_null(get_metacran_info("notAPackage"))

  # package exists and returns a named list of settings
  expect_named(get_metacran_info("rlang"))
})

# Tests for get_endpoint_content
test_that("get_endpoint_content", {
  # missing arguments
  expect_error(get_endpoint_content())

  # Skip endpoint tests on ci
  skip_on_ci()

  # Valid return for existing repo with no token
  get_endpoint_content("https://api.github.com/repos/pedrocoutinhosilva/hexFinder") |> #nolint
    expect_named()

  # Error return for existing repo with invalid token
  get_endpoint_content(
    "https://api.github.com/repos/pedrocoutinhosilva/hexFinder", token = "fake") |> #nolint
    expect_null()

  # Error return for existing repo with invalid branch
  get_endpoint_content(
    "https://api.github.com/repos/pedrocoutinhosilva/hexFinder", branch = "fake") |> #nolint
    expect_null()

})

# Tests for get_possible_paths
test_that("get_possible_paths", {

  mock_ok_content <- list(
    list(path = "path/logo", type = "blob"),
    list(path = "path/package", type = "blob"),
    list(path = "path/ignore", type = "blob")
  )

  # missing arguments
  expect_error(get_possible_paths())
  expect_null(get_possible_paths(mock_ok_content))
  expect_null(get_possible_paths(mock_ok_content, c("ignore")))
  expect_null(get_possible_paths(mock_ok_content, c("ignore"), c("logo")))

  # Mininal example
  expect_type(get_possible_paths(mock_ok_content, c("ignore"), c("logo"), "package"), "list") #nolint

  # Empty content
  expect_null(get_possible_paths(list(), c("ignore"), c("logo"), "package")) #nolint
  expect_type(get_possible_paths(list(list()), c("ignore"), c("logo"), "package"), "list") #nolint

  # wrong format of content entry
  expect_type(get_possible_paths(list(list(notpath = "")), c("ignore"), c("logo"), "package"), "list") #nolint

  # Entry is ignored because its not a blob type
  expect_type(get_possible_paths(list(list(path = "", type  = "notblob")), c("ignore"), c("logo"), "package"), "list") #nolint
})

# Tests for get_best_image
test_that("get_best_image ", {
  # missing arguments
  expect_error(get_best_image())
  expect_error(get_best_image(list(), "https://raw.githubusercontent.com/repos/pedrocoutinhosilva/hexFinder", "main")) #nolint

  # Skip endpoint tests on ci
  skip_on_ci()

  # Minimal examples
  expect_type(get_best_image(
    list(list(path = "man/figures/logo.svg")),
    "https://raw.githubusercontent.com/pedrocoutinhosilva/hexFinder",
    "main"
  ), "character")
})

# Tests for keep_good_ratio_images
test_that("keep_good_ratio_images ", {
  # missing arguments
  expect_error(keep_good_ratio_images())

  # Skip endpoint tests on ci
  skip_on_ci()

  # Minimal examples
  expect_type(keep_good_ratio_images(
    list(),
    "https://raw.githubusercontent.com/pedrocoutinhosilva/hexFinder",
    "main"
  ), "list")

  expect_type(keep_good_ratio_images(
    list(list(path = "man/figures/logo.svg")),
    "https://raw.githubusercontent.com/pedrocoutinhosilva/hexFinder",
    "main"
  ), "list")
})