test_that("mipLineHistorical works", {
  qe <- dplyr::filter(quitte::quitte_example_dataAR6,
                      .data$scenario == levels(.data$scenario)[[1]],
                      .data$variable == "Consumption")
  p <- mipLineHistorical(qe)
  expect_identical(p$labels$x, "Year")

  expect_no_error(print(p))
})
