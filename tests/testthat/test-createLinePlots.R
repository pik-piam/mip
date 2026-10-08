test_that("createLinePlots draws validation threshold bands", {
  qe <- dplyr::filter(quitte::quitte_example_dataAR6,
                      .data$scenario == levels(.data$scenario)[[1]],
                      .data$variable == "Consumption")
  qe <- droplevels(qe)
  mainReg <- as.character(unique(qe$region))[[1]]

  regions <- unique(as.character(qe$region))
  periods <- sort(unique(qe$period))[1:6]
  thresholds <- data.frame(
    variable = "Consumption",
    unit = as.character(unique(qe$unit))[[1]],
    expand.grid(region = regions, period = periods, stringsAsFactors = FALSE),
    min_red = NA, min_yel = 5000, max_yel = 100000, max_red = 120000)

  itemsBase <- createLinePlots(qe, vars = "Consumption", mainReg = mainReg)
  items <- createLinePlots(qe, vars = "Consumption", mainReg = mainReg,
                           thresholds = thresholds)

  # a green (min_yel to max_yel) and a yellow (max_yel to max_red) ribbon are
  # added below the existing layers, the blue band is skipped as min_red is NA
  expect_length(items[[1]]$layers, length(itemsBase[[1]]$layers) + 2)
  expect_s3_class(items[[1]]$layers[[1]]$geom, "GeomRibbon")
  expect_s3_class(items[[1]]$layers[[2]]$geom, "GeomRibbon")
  expect_no_error(print(items[[1]]))


  expect_length(items[[2]]$layers, length(itemsBase[[2]]$layers) + 2)
  expect_s3_class(items[[2]]$layers[[1]]$geom, "GeomRibbon")
  expect_no_error(print(items[[2]]))

  # thresholds covering a single period are drawn as vertical bars
  thresholdsSingle <- thresholds[thresholds$period == periods[1], ]
  itemsSingle <- createLinePlots(qe, vars = "Consumption", mainReg = mainReg,
                                 thresholds = thresholdsSingle)
  expect_s3_class(itemsSingle[[1]]$layers[[1]]$geom, "GeomLinerange")
  expect_no_error(print(itemsSingle[[1]]))

  # thresholds of other variables do not change the plots
  thresholdsOther <- thresholds
  thresholdsOther$variable <- "Some|Other|Variable"
  itemsOther <- createLinePlots(qe, vars = "Consumption", mainReg = mainReg,
                                thresholds = thresholdsOther)
  expect_length(itemsOther[[1]]$layers, length(itemsBase[[1]]$layers))

  # malformed thresholds are rejected
  expect_error(
    createLinePlots(qe, vars = "Consumption", mainReg = mainReg,
                    thresholds = data.frame(variable = "Consumption")))
})
