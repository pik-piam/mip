test_that("plotstyle.add successfully adds an entry to the session cache", {
  a <- plotstyle.add("NEU", "Other Europe", "#0000A6FF", marker = 9, replace = TRUE)
  b <- plotstyle("NEU", out = "all")
  expect_identical(b$color, "#0000A6FF")
  expect_identical(b$marker, 9)

  # making another change does not affect the first change
  a <- plotstyle.add("EUR", "Europe", "orange", marker = 10, replace = TRUE)
  b <- plotstyle("NEU", out = "all")
  c <- plotstyle("EUR", out = "all")
  expect_identical(b$color, "#0000A6FF")
  expect_identical(b$marker, 9)
  expect_identical(c$color, "orange")
  expect_identical(c$marker, 10)


})
