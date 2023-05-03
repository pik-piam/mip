test_that("mipArea works", {
  qe <- dplyr::filter(quitte::quitte_example_dataAR6,
                      .data$scenario == levels(.data$scenario)[[1]],
                      grepl("Final Energy", .data$variable))
  p <- mipArea(qe)
  expect_identical(p$labels$x, "Year")
  expect_identical(p$labels$y, "Final Energy (EJ/yr)")
})
