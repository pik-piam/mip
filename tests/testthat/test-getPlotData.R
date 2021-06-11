test_that("getPlotData works for single symbols", {
  testData <- data.frame(
    iteration = as.factor(rep(1:4, each = 4)),
    year = as.factor(c(2000, 2000, 3000, 3000)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName = rep(1:4, each = 4) + 0.1
  )
  attr(testData, "symName") <- "testSymbolName"
  attr(testData, "domains") <- colnames(testData)

  # write test data to gdx file
  testFile <- paste0(tempdir(), "/test.gdx")
  gdxrrw::wgdx.lst(testFile, testData)

  expected <- data.frame(
    iteration = rep(1:4, each = 4),
    year = c(2000, 2000, 3000, 3000),
    region = as.factor(c("ABC", "XYZ")),
    symbol = as.factor("testSymbolName"),
    value = rep(1:4, each = 4) + 0.1
  )
  expect_equal(getPlotData(testFile, "testSymbolName"), expected)
})

test_that("getPlotData works for multiple symbols", {
  # TODO
  expect_true(FALSE)
})
