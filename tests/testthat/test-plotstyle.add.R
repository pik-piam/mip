test_that("plotstyle.add successfully adds an entry to the session cache", {

  a <- plotstyle.add("NEU",  "Other Europe", rgb(0.0, 0.0, 0.65,  1), marker = 9, replace = TRUE)
  b <- plotstyle("NEU", out = "all")
  expect_identical(b$color, "#0000A6FF")
  expect_identical(b$marker, 9)

})
