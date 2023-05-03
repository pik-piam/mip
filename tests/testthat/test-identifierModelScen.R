test_that("shorten_legend works", {
  qe <- quitte::quitte_example_dataAR6

  d <- qe
  d$identifier <- identifierModelScen(d)
  expected <- levels(interaction(d$model, d$scenario, sep = " "))
  expect_true(setequal(d$identifier, expected))

  d <- droplevels(dplyr::filter(qe, !!sym("model") == levels(qe$model)[[1]]))
  d$identifier <- identifierModelScen(d)
  expect_identical(attr(d$identifier, "deletedinfo"), paste("Model:", levels(qe$model)[[1]]))
  attr(d$identifier, "deletedinfo") <- NULL
  expect_identical(d$identifier, d$scenario)

  d <- droplevels(dplyr::filter(qe, !!sym("scenario") == levels(qe$scenario)[[1]]))
  d$identifier <- identifierModelScen(d)
  expect_identical(attr(d$identifier, "deletedinfo"), paste("Scenario:", levels(qe$scenario)[[1]]))
  attr(d$identifier, "deletedinfo") <- NULL
  expect_identical(d$identifier, d$model)

  d <- droplevels(dplyr::filter(qe, !!sym("scenario") == levels(qe$scenario)[[1]],
                                    !!sym("model") == levels(qe$model)[[1]]))
  d$identifier <- identifierModelScen(d)
  expect_identical(attr(d$identifier, "deletedinfo"), paste("Model:", levels(qe$model)[[1]]))
  attr(d$identifier, "deletedinfo") <- NULL
  expect_identical(d$identifier, d$scenario)
})
