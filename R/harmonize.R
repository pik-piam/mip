#' Harmonization of model data to historical data, using harmonization methods
#' of aneris, ported to R.
#' See: https://github.com/iiasa/aneris/blob/master/aneris/methods.py

#' @author Falk Benke
#' @param df data frame with model data to be harmonized, must have the following columns:
#' variable, region, scenario, model, period
#' @param hist data frame with historical data to be used for harmonization, must also have the following columns:
#' variable, region, scenario, model, period
#' @param finalYear when should harmonized data match model data again?
#' @param harmonizeYear when should harmonization begin? sets model data = reference data for this year
#' @param method harmonization method, currently supported methods are "ratio" and "offset"
#' @param suffix to be appended to harmonized variables
#' @importFrom dplyr filter mutate
#' @importFrom data.table `:=`
#' @export
harmonize <- function(df, hist, finalYear = "2050", harmonizeYear = "2015", method = "ratio", suffix = "") {
  if (!method %in% c("offset", "ratio")) {
    stop("Invalid method. Options are 'offset' and 'ratio'.")
  }

  .reduceRatio <- function(df, hist, finalYear = "2050", harmonizeYear = "2015") {
    # harmonize factors
    c <- hist[, harmonizeYear]
    m <- df[, harmonizeYear]
    ratios <- c / m

    years <- as.numeric(names(df)[-1:-5])
    yi <- as.numeric(harmonizeYear)
    yf <- as.numeric(finalYear)

    # get factors that reduce from 1 to 0, but replace with 1s in years prior to harmonization
    factors <- vapply(
      years, function(year) {
        ifelse(year < yi, 1.0, ifelse(year < yf, 1 - (year - yi) / round(yf - yi), 0.0))
      },
      FUN.VALUE = numeric(length(1))
    )

    # multiply existing values by ratio time series
    ratios <- data.frame(outer(ratios - 1, factors) + 1)
    names(ratios) <- as.character(years)

    df[, as.character(years)] <- df[, as.character(years)] * ratios

    return(df)
  }

  .reduceOffset <- function(df, hist, finalYear = "2050", harmonizeYear = "2015") {
    # harmonize factors
    c <- hist[, harmonizeYear]
    m <- df[, harmonizeYear]
    offsets <- c - m

    years <- as.numeric(names(df)[-seq(1, 5)])
    yi <- as.numeric(harmonizeYear)
    yf <- as.numeric(finalYear)

    # get factors that reduce from 1 to 0; factors before base year are > 1
    factors <- vapply(
      years,
      function(year) {
        ifelse(year <= yf, 1 - (year - yi) / round(yf - yi), 0.0)
      },
      FUN.VALUE = numeric(length(1))
    )


    offsets <- data.frame(outer(offsets, factors))
    names(offsets) <- as.character(years)

    df[, as.character(years)] <- df[, as.character(years)] + offsets

    return(df)
  }

  # iterate over models and variables

  vars <- intersect(unique(df$variable), unique(hist$variable))
  regions <- intersect(unique(df$region), unique(hist$region))

  hist <- hist %>%
    filter(!is.na(!!sym("value")), !!sym("variable") %in% vars, !!sym("region") %in% regions) %>%
    reshape2::dcast(formula = ... ~ period)

  df <- df %>%
    filter(!is.na(!!sym("value")), !!sym("variable") %in% vars) %>%
    reshape2::dcast(formula = ... ~ period) %>%
    filter(!is.na(!!sym("harmonizeYear"))) %>%
    droplevels()

  if (!harmonizeYear %in% names(df)) {
    print(names(df))
    stop("Harmonize year not found in data.")
  }

  if (method == "offset") {
    methodHarmonize <- .reduceOffset
  } else {
    methodHarmonize <- .reduceRatio
  }

  dfHarmonized <- NULL
  for (m in unique(df$model)) {
    for (s in unique(df$scenario)) {
      d <- filter(df, !!sym("model") == m, !!sym("scenario") == s) %>% droplevels()

      if (nrow(d) > 0) {
        h <- filter(hist, !!sym("variable") %in% unique(d$variable)) %>%
          droplevels() %>%
          order.levels(variable = levels(d$variable)) %>%
          arrange(!!sym("variable"))

        dfHarmonized <- rbind(
          dfHarmonized,
          methodHarmonize(d, h, finalYear = finalYear, harmonizeYear = harmonizeYear)
        )
      }
    }
  }

  dfHarmonized %>%
    reshape2::melt(id.vars = seq(1:5), variable.name = "period") %>%
    mutate(
      !!sym("period") := as.numeric(as.character(!!sym("period"))),
      !!sym("variable") := paste0(!!sym("variable"), suffix)
    ) %>%
    filter(!!sym("period") >= as.numeric(harmonizeYear)) %>%
    return()
}
