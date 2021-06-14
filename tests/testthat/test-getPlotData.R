test_that("getPlotData works for single symbol", {
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
  testData1 <- data.frame(
    iteration = as.factor(rep(1:4, each = 4)),
    year = as.factor(c(2000, 2000, 3000, 3000)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName1 = rep(1:4, each = 4) + 0.1
  )
  attr(testData1, "symName") <- "testSymbolName1"
  attr(testData1, "domains") <- colnames(testData1)

  testData2 <- data.frame(
    iteration = as.factor(rep(1:4, each = 4)),
    year = as.factor(c(2000, 2000, 3000, 3000)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName2 = rep(1:4, each = 4) + 0.2
  )
  attr(testData2, "symName") <- "testSymbolName2"
  attr(testData2, "domains") <- colnames(testData2)

  # write test data to gdx file
  testFile <- paste0(tempdir(), "/test.gdx")
  gdxrrw::wgdx.lst(testFile, testData1, testData2)

  expected <- data.frame(
    iteration = rep(1:4, each = 4),
    year = c(2000, 2000, 3000, 3000),
    region = as.factor(c("ABC", "XYZ")),
    symbol = as.factor(c(rep("testSymbolName1", 16), rep("testSymbolName2", 16))),
    value = c(rep(1:4, each = 4) + 0.1, rep(1:4, each = 4) + 0.2)
  )
  expect_equal(getPlotData(testFile, c("testSymbolName1", "testSymbolName2")), expected)
})

test_that("getPlotData works warns when combining incompatible data", {
  testData1 <- data.frame(
    iteration = as.factor(rep(1:4, each = 4)),
    year = as.factor(c(2000, 2000, 3000, 3000)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName1 = rep(1:4, each = 4) + 0.1
  )
  attr(testData1, "symName") <- "testSymbolName1"
  attr(testData1, "domains") <- colnames(testData1)

  testData2 <- data.frame(
    iteration = as.factor(rep(1:4, each = 4)),
    year = as.factor(c(2000, 2000, 3000, 3000)),
    region2 = as.factor(c("ABC", "XYZ")),
    testSymbolName2 = rep(1:4, each = 4) + 0.2
  )
  attr(testData2, "symName") <- "testSymbolName2"
  attr(testData2, "domains") <- colnames(testData2)

  # write test data to gdx file
  testFile <- paste0(tempdir(), "/test.gdx")
  gdxrrw::wgdx.lst(testFile, testData1, testData2)

  expected <- data.frame(
    iteration = rep(1:4, each = 4),
    year = c(2000, 2000, 3000, 3000),
    region = as.factor(c("ABC", "XYZ")),
    symbol = as.factor("testSymbolName1"),
    value = rep(1:4, each = 4) + 0.1
  )
  plotData <- expect_warning(
    getPlotData(testFile, c("testSymbolName1", "testSymbolName2")),
    "Cannot merge with previous data, skipping symbol testSymbolName2"
  )
  expect_equal(plotData, expected)
})
