test_df <- eq_get_data() %>% eq_clean_data

test_that("eq_clean_data cleans dates", {
  expect_is(test_df$DATE, "Date")
  expect_gte(min(lubridate::year(test_df$DATE)), 0)
})

test_that("eq_clean_data cleans coordinates", {
  expect_is(test_df$LATITUDE, "numeric")
  expect_is(test_df$LONGITUDE, "numeric")

  expect_true(all(!is.na(test_df$LATITUDE)))
  expect_true(all(!is.na(test_df$LONGITUDE)))
})

test_that("eq_clean_data pases other numeric columns to correct type", {
  expect_is(test_df$TOTAL_DEATHS, "numeric")
  expect_is(test_df$EQ_PRIMARY, "numeric")
})
