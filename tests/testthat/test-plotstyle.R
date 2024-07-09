test_that(
  "plotstyle() does not strip units if told not to do so",
  {
    expect_identical(plotstyle("Forcing|CO2 (W/m2)"),
                     c(`Forcing|CO2` = "#e6194B"))

    expect_identical(plotstyle("Forcing|CO2 (W/m2)", strip_units = FALSE),
                     c(`Forcing|CO2 (W/m2)` = "#e6194B"))

    withr::with_options(
      list('plotstyle.strip_units' = FALSE),
      expect_identical(plotstyle("Forcing|CO2 (W/m2)"),
                     c(`Forcing|CO2 (W/m2)` = "#e6194B"))
    )
  })
