test_that("test number of columes are 50", {

  file = paste0(system.file("vignettes", package = "RGgitP"), "/",make_filename(2013))
  expect_that(file, is_a("character"))

})
