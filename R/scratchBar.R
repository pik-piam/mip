#' @title scratchBar
#' @description Fast visualization of a magpie object or quitte object as bar plot.
#' If available, the years 2020 and 2050 will be selected; if not available the first and last year of the available years.
#'
#'
#' @export
#'
#' @param x an object that can be converted to a quitte (e.g. a quitte object or a magpie objet)
#' @param complete "default" or list with specifications for scratchComplete
#' @param simplify "default" or list with specifications for scratchSimplify
#' @param ... furhter arguments handed on to mipBarYearData function
#' @return ggplot object
#' @author Benjamin Leon Bodirsky

#' @examples
#'
#'   \dontrun{
#'     x <- Intake(gdx)
#'   }
#'
#'
scratchBar <- function (x, complete = "default", simplify = "default", ...) {

  # complete object
  x <- scratchComplete(x)

  # simplify object
  x <- scratchSimplify(x)

  # add metadata

  #plot

  plot = mipBarYearData(x, ...)

  return(plot)
}

scratchComplete <- function(x) {

  if(!is.quitte(x)){
    cat("Expected input data format is quitte. Converting to quitte.")
    if(is.magpie(x)){
      # handle missing years
      if(length(getYears(x))==0){
        cat("Year missing. Set to y9999 for plotting")
        x=setYears(x,"y9999")
      }
      # handle misspecified variable column
      potential_vars = setdiff(strsplit(getSets(x,fulldim = F)[[3]],split = "\\.")[[1]],c("model","scenario"))
      if("variable"%in%potential_vars){} else {
        cat(paste0("Using the set ", potential_vars[1], " as variable dimension."))
        tmp=getSets(x)
        tmp[which(tmp==potential_vars[1])]<-"variable"
        getSets(x)<-tmp
      }
    }

    x <- as.quitte(x)
  }

  # rescue model and scenario if missing
  x$model <- revalue(x$model,c("(Missing)"    = "unspecified_model"),warn_missing = FALSE)
  x$scenario <- revalue(x$scenario,c("(Missing)"    = "unspecified_scenario"),warn_missing = FALSE)
  x$variable <- revalue(x$variable,c("(Missing)"    = "unspecified_variable"),warn_missing = FALSE)
  x$unit <- revalue(x$unit,c("(Missing)"    = "unspecified_unit"),warn_missing = FALSE)

  return(x)
}

scratchSimplify<-function(x,simplify="default"){
  x=as.quitte(x)
  x=as.magpie(x)
  if(simplify=="default"){

    #subselect two years for simplication
    if(length(getYears(x)>2)){
      if("y2020"%in%getYears(x)){
        year_one="y2020"
      } else {
        year_one=getYears(x)[1]
      }
      if("y2050" %in% getYears(x)){
        year_two="y2050"
      } else {
        year_two= getYears(x)[length(getYears(x))]
      }
      x = x[,c(year_one,year_two),]
    }

    #if multiple variables, throw warning
    if (length(setdiff(
      strsplit(getSets(x,fulldim = FALSE)[[3]],split = "\\.")[[1]],
      c("model","scenario")))>1) {
      cat("There is more than one variable beyond model and scenario. This cannot be well handled by the plotting script.")
      # no further handling strategy needed as for now, as the plotting function seems to handle this.
    }

    #if first year contains equal values, only show one bar

  }
  x=as.quitte(x)
  return(x)
}
