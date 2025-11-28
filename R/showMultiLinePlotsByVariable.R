#' Show Multi-Line Plots by Variable
#'
#' Show plots with different regions in the same plot; x-axis variable chosen by
#' user.
#'
#' Same as \code{\link{showMultiLinePlots}} but with the variable specified by
#' \code{xVar} on x-axis. For every y-axis-value, we need a unique x-axis-value.
#' For historical data, there may be several sources / models of the same
#' variable. For the x-axis-variable a unique historical source / model is
#' chosen via \code{histRefModel}.
#'
#' @inheritDotParams createMultiLinePlotsByVariable
#' @return \code{NULL} is returned invisible.
#' @section Example Plots:
#' \if{html}{page 1: \figure{showMultiLinePlotsByVariable1.png}{options: width="100\%"}}
#' \if{html}{page 2: \figure{showMultiLinePlotsByVariable2.png}{options: width="100\%"}}
#' @examples
#' \dontrun{
#' options(mip.mainReg = "World")
#' options(mip.yearsBarPlot = c(2010, 2030, 2050, 2100))
#' options(mip.histRefModel = c("GDP|PPP pCap" = "James_IMF"))
#' data <- as.quitte(data)
#' vars <- c(
#'   "FE|Transport pCap",
#'   "FE|Buildings pCap",
#'   "FE|Industry pCap")
#' showMultiLinePlotsByVariable(data, vars, "GDP|PPP pCap")
#' }
#' @export
showMultiLinePlotsByVariable <- function(...) {
	for (plot in createMultiLinePlotsByVariable(...)) {
		showPlot(plot)
		cat("\n\n")
	}
	return(invisible(NULL))
}

#' Create Multi Line Plots by Variable
#'
#' Creates the plots for showMultiLinePlotsByVariable
#' @param xVar A single string. The variable for the x-axis.
#' @param showHistorical A single logical value. Should historical data be
#'   shown? It is not recommended to set this to \code{TRUE} as the resulting
#'   plot we probably be quite confusing.
#' @param showGlobal A single logical value. Should global data be
#'  shown? Default is false to save space in pdf
#' @param nrowNum An integer value. Number of rows of the panel figures
#' @param histRefModel A named character vector identifying the unique model to
#'   be chosen for historical data. Use \code{options(mip.histRefModel=<value>)}
#'   to set globally.
#' @param yearsByVariable A numeric vector. The years to be marked in the plots.
#'   As default it uses the value globally set by \code{options(mip.yearsBarPlot=<value>)}.
#' @param logscale A string such as "x", "y" or "xy". Each axis mentioned in this string
#'   is displayed in logarithmic scale (base 10) instead of linear.
#' @inheritParams createMultiLinePlots
#' @importFrom rlang .data .env
#' @importFrom tidyr drop_na
#' @importFrom ggplot2 ylim
createMultiLinePlotsByVariable <- function(
  data, vars, xVar, scales = "free_y",
  showHistorical = FALSE,
  showGlobal = FALSE,
  nrowNum = 1,
  mainReg = getOption("mip.mainReg"),
  histRefModel = getOption("mip.histRefModel"),
  yearsByVariable = getOption("mip.yearsBarPlot"),
  logscale = ""
) {
	# validate function arguments
	stopifnot(is.character(vars))
	stopifnot(is.character(xVar) && length(xVar) == 1)
	stopifnot(is.character(scales) && length(scales) == 1)
	stopifnot(is.character(logscale) && length(logscale) == 1)
	stopifnot(identical(showHistorical, TRUE) || identical(showHistorical, FALSE))
	stopifnot(is.null(yearsByVariable) || is.numeric(yearsByVariable))
	checkGlobalOptionsProvided(c("mainReg", "histRefModel"))
	stopifnot(is.character(mainReg) && length(mainReg) == 1)
	stopifnot(is.character(histRefModel) && !is.null(names(histRefModel)))


	# keep and match relevant variables
	data <- as.quitte(data)
	dy <- data %>%
		filter(.data$variable %in% .env$vars)
	dx <- data %>%
		filter(.data$variable %in% .env$xVar) %>%
		filter(.data$scenario != "historical" | .data$model == .env$histRefModel[.env$xVar])
	d <- dy %>%
		left_join(dx, by = c("scenario", "region", "period"), suffix = c("", ".x") ) %>%
		drop_na(.data$value, .data$value.x) %>%
		filter(if (grepl("x", logscale)) .data$value.x > 0 else TRUE) %>% # if logscale x, drop zeroes
		filter(if (grepl("y", logscale)) .data$value   > 0 else TRUE) %>% # if logscale y, drop zeroes
		arrange(.data$period) %>% droplevels()


	# prepare plotting
	label <- paste0("(", paste0(levels(d$unit), collapse = ","), ")")
	xLabel <- paste0(xVar, " (", paste0(levels(d$unit.x), collapse = ","), ")")

	logscaleRange <- function(dataValues) c(floor(log10(min(dataValues))*10)/10, ceiling(log10(max(dataValues))*10)/10)
	logscaleBreaks <- function(dataValues) {
		majorBreaks <- 10^seq(floor(log10(min(dataValues))), ceiling(log10(max(dataValues))), 1) # 10 100 1000
		minorBreaks <- as.vector(outer(1:9, head(majorBreaks,-1), "*")) # 10 20 .. 90 100 200 .. 900
		if(diff(logscaleRange(dataValues)) < 3) majorBreaks <- minorBreaks
		return(list(majorBreaks, minorBreaks))
	}

	plotOptions <- function(dataOptions)
		dataOptions %>% ggplot(aes(.data$value.x, .data$value)) +
		geom_line(aes(linetype = .data$scenario)) +
		facet_wrap(vars(.data$variable), scales = scales, nrow = nrowNum) +
		theme_minimal() +
		ylab(label) +
		xlab(xLabel) +
		list(
			# year markers
			if(length(yearsByVariable) > 0)
				geom_point(mapping = aes(.data$value.x, .data$value, shape = .data$year),
					data = dataOptions %>%
						filter(.data$period %in% .env$yearsByVariable) %>%
						mutate(year = factor(.data$period))),
			# logscale
			if(grepl("x", logscale))
				logscaleBreaks(dataOptions$value.x) %>% { scale_x_log10(breaks = first(.), minor_breaks = last(.)) },
			if(grepl("y", logscale))
				logscaleBreaks(dataOptions$value) %>% { scale_y_log10(breaks = first(.), minor_breaks = last(.)) },
			# axis limits
			if(grepl("x", logscale)) expand_limits(x = 10^logscaleRange(dataOptions$value.x)),
			if(grepl("y", logscale)) expand_limits(y = 10^logscaleRange(dataOptions$value))
			else					 expand_limits(y = 0)
		)

	
	# plot global or main region data
	if (showGlobal) {
		dMainScen <- d %>% filter(.data$region == .env$mainReg, .data$scenario != "historical") %>% droplevels()
		warnMissingVars(dMainScen, vars)
		if (NROW(dMainScen) == 0) {
			warning("Nothing to plot.", call. = FALSE)
			return(list())
		}
		plotGlobal <- dMainScen %>% plotOptions()
	}


	# plot other regions
	dRegiScen <- d %>% filter(.data$region != .env$mainReg, .data$scenario != "historical") %>% droplevels()
	regions <- levels(dRegiScen$region)
	plotRegi <- dRegiScen %>% plotOptions() +
		aes(color = .data$region) +
		scale_color_manual(values = plotstyle(regions))


	# add historical data 
	if (showHistorical) {
		stopifnot(xVar %in% names(histRefModel))

		plotOptionsHist <- function(dataHist)
			geom_point(data = dataHist, aes(shape = .data$model)) +
			geom_line(data = dataHist, aes(group = paste0(.data$model, .data$region)), alpha = 0.5)

		if (showGlobal) {
			dMainHist <- d %>% filter(.data$region == .env$mainReg, .data$scenario == "historical") %>% droplevels()
			plotGlobal <- plotGlobal + plotOptionsHist(dMainHist)
		}

		dRegiHist <- d %>% filter(.data$region != .env$mainReg, .data$scenario == "historical") %>% droplevels()
		plotRegi <- plotRegi + plotOptionsHist(dRegiHist)
	}

	if (showGlobal) {
		return(list(plotGlobal, plotRegi))
	} else {
		return(list(plotRegi))
	}
}
