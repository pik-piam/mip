test_that("getPlotData works for a single gdx file", {
  testData <- data.frame(
    iteration = as.factor(rep(1:4, each = 4)),
    year = as.factor(c(2000, 2000, 3000, 3000)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName = rep(1:4, each = 4) + 0.1
  )
  attr(testData, "symName") <- "testSymbolName"
  attr(testData, "domains") <- c("iteration", "year", "region")

  # write test data to gdx file
  testFile <- file.path(tempdir(), "fulldata.gdx")
  gdxrrw::wgdx.lst(testFile, testData)

  expected <- data.frame(
    iteration = rep(1:4, each = 4),
    year = c("2000", "2000", "3000", "3000"),
    region = c("ABC", "XYZ"),
    testSymbolName = rep(1:4, each = 4) + 0.1
  )

  expect_equal(getPlotData("testSymbolName", testFile), expected)
  expect_equal(getPlotData("testSymbolName", dirname(testFile)), expected)
})

test_that("getPlotData works for multiple gdx files", {
  testDir <- tempdir()

  testData1 <- data.frame(
    year = as.factor(rep(2000 + 0:7, each = 2)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName = rep(1:4, each = 4) + 0.1
  )
  attr(testData1, "symName") <- "testSymbolName"
  attr(testData1, "domains") <- c("year", "region")
  gdxrrw::wgdx.lst(file.path(testDir, "fulldata_1.gdx"), testData1)

  testData2 <- data.frame(
    year = as.factor(rep(2000 + 0:7, each = 2)),
    region = as.factor(c("ABC", "XYZ")),
    testSymbolName = rep(1:4, each = 4) + 0.2
  )
  attr(testData2, "symName") <- "testSymbolName"
  attr(testData2, "domains") <- c("year", "region")
  gdxrrw::wgdx.lst(file.path(testDir, "fulldata_2.gdx"), testData2)

  expected <- data.frame(
    year = as.character(c(rep(2000 + 0:7, each = 2), rep(2000 + 0:7, each = 2))),
    region = c("ABC", "XYZ"),
    iteration = rep(1:2, each = 16),
    testSymbolName = c(rep(1:4, each = 4) + 0.1, rep(1:4, each = 4) + 0.2)
  )

  actual <- getPlotData("testSymbolName", file.path(testDir, paste0("fulldata_", 1:2, ".gdx")))
  expect_equal(actual, expected)
  actual2 <- getPlotData("testSymbolName", testDir)
  expect_equal(actual2, expected)
})
