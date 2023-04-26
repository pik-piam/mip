test_that("shorten_legend works", {
  # first teststring
  teststring <- c("Model Scenario_BLUB", "Model Scenario_BLA", "Model Scenario_BLA_BLA_BLUB")
  for (teststring in list(as.factor(teststring), teststring)) {
    # do nothing
    expect_true(setequal(shorten_legend(teststring, maxchar = 30), teststring))
    # remove identical parts
    result <- shorten_legend(teststring, maxchar = 15)
    expect_true(setequal(c("BLUB", "BLA", "BLA_BLA_BLUB"), result))
    expect_identical(attr(result, "front"), "Model Scenario_")
    result <- shorten_legend(teststring, identical_only = TRUE)
    expect_true(setequal(c("BLUB", "BLA", "BLA_BLA_BLUB"), result))
    expect_identical(attr(result, "front"), "Model Scenario_")
    # cutoff end of string
    result <- shorten_legend(teststring, maxchar = 5)
    expect_true(setequal(c("BLUB", "BLA", "BLA_B"), result))
    expect_identical(attr(result, "front"), "Model Scenario_")
    # ylab specified 1
    ylab <- "Model Scenario_"
    result <- shorten_legend(teststring, ylab = ylab)
    expect_true(setequal(c("BLUB", "BLA", "BLA_BLA_BLUB"), result))
    expect_identical(attr(result, "ylab"), ylab)
    # ylab specified 2
    ylab <- "Model Scenario"
    result <- shorten_legend(teststring, ylab = ylab)
    expect_true(setequal(c("_BLUB", "_BLA", "_BLA_BLA_BLUB"), result))
    expect_identical(attr(result, "ylab"), ylab)
  }

  # second teststring with back
  teststring <- c("FE|1st generation", "FE|2nd generation")
  for (teststring in list(as.factor(teststring), teststring)) {
    result <- shorten_legend(teststring, identical_only = TRUE)
    expect_true(setequal(c("1st", "2nd"), result))
    expect_identical(attr(result, "front"), "FE|")
    expect_identical(attr(result, "back"), " generation")
    expect_identical(attr(result, "ylab"), "FE|... generation")

    # ylab given
    ylab <- "FE"
    result <- shorten_legend(teststring, ylab = ylab)
    expect_true(setequal(c("1st generation", "2nd generation"), result))
    expect_identical(attr(result, "ylab"), ylab)

    # prioritize ylab over identical_only
    ylab <- "FE"
    result <- shorten_legend(teststring, ylab = ylab, identical_only = TRUE)
    expect_true(setequal(c("1st generation", "2nd generation"), result))
    expect_identical(attr(result, "ylab"), ylab)

    unit <- "EJ"
    result <- shorten_legend(teststring, ylab = ylab, identical_only = TRUE, unit = unit)
    expect_true(setequal(c("1st generation", "2nd generation"), result))
    expect_identical(attr(result, "ylab"), "FE (EJ)")

    unit <- c("Ws", "J")
    result <- shorten_legend(teststring, ylab = ylab, identical_only = TRUE, unit = unit)
    expect_true(setequal(c("1st generation", "2nd generation"), result))
    expect_identical(attr(result, "ylab"), "FE (Ws | J)")
  }
})
