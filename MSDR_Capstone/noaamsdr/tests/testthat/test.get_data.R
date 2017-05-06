test_dir <- tempdir()
test_file <- file.path(test_dir, "NOAA.txt", fsep = "\\")

test_that("eq_get_data returns the dataset", {
  df <- eq_get_data()

  expect_is(df, "data.frame")
  expect_is(df, "tbl")

  expect_true(all(sapply(df, class) == "character"))
})


test_that("eq_file_path returns a absolute path", {
  path <- eq_file_path()

  expect_is(path, "character")
  expect_true(file.exists(path))
})


test_that("eq_download_data download the dataset", {
  should_return_null <- eq_download_data(test_dir)

  expect_null(should_return_null)
  expect_true(file.exists(test_file))

  unlink(test_dir, recursive = TRUE, force = TRUE)
})
