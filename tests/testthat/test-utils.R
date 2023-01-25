# Tests for url_file_exists
with_mock_api({
  test_that("url_file_exists", {
    # package does not exist
    expect_error(url_file_exists())
  })
})

# Tests for clean_repo_links
test_that("clean_repo_links ", {
  results <- clean_repo_links("notarepo, github.com/arepo", "github.com/arepo/issues") #nolint
  expect_equal(results, "github.com/arepo")
})

# Tests for clean_repo_links
test_that("catch ", {
  # package does not exist
  expect_equal(catch("a string"), "a string")
  expect_null(catch(NULL), NULL)
})