test_df <- eq_get_data() %>% eq_clean_data %>% {.[1:50, ]}

test_that("theme_timeline exists and is a theme", {
  expect_is(theme_timeline, "gg")
  expect_is(theme_timeline, "theme")
})

test_that("geom_timeline exists and is a ggproto layer", {
  expect_is(geom_timeline(), "ggproto")
  expect_is(geom_timeline(), "Layer")
})

test_that("geom_timeline_label exists and is a ggproto layer", {
  expect_is(geom_timeline(), "ggproto")
  expect_is(geom_timeline(), "Layer")
})

test_that("eq_map returns a Leaflet", {
  expect_is(eq_map(test_df, "DATE"), "leaflet")
})

test_that("eq_create_label right labels", {
  new_labels <- eq_create_label(test_df)

  expect_is(new_labels, "character")
  expect_true(any(grepl("<b>Location: </b>", new_labels)))
  expect_true(any(grepl("<b>Magnitude: </b>", new_labels)))
  expect_true(any(grepl("<b>Total deaths: </b>", new_labels)))
})
