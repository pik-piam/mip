#' Shorten legend names to a given length
#' 
#' Function will try first to return vector as it is, then remove elements which are identical between all
#' elements and finally cut the end of the character vectors so that it fits the given maxchar setting. Underscores
#' will be replaced with empty spaces for further processing
#' @param x           A character vector or a factor vector that should be shortened
#' @param maxchar     Maximum number of characters allowed
#' @param identical_only If set to TRUE identical parts in the name will be removed, independent of the character length (maxchar will be ignored!)
#' @param sep a vector of characters which should be interpreted as separators
#' @author Jan Philipp Dietrich
#'
#' @examples
#' a <- c("Model Scenario_BLUB", "Model Scenario_BLA", "Model Scenario_BLA_BLA_BLUB")
#' 
#' # do nothing
#' shorten_legend(a,30)
#' # remove identical parts
#' shorten_legend(a,15)
#' # or ...
#' shorten_legend(a,identical_only=TRUE)
#' # cutoff end of string
#' shorten_legend(a,5)
#' @export

shorten_legend <- function(x, maxchar=20, identical_only=FALSE, sep=c(" ","|","_")) {
  if(is.na(maxchar)) {
    warning("Maxchar was set to NA. Set maxchar back to default value (20).")
    maxchar <- 20
  }
  
  if(is.factor(x)) {
    x <- droplevels(x)
    tmp <- shorten_legend(levels(x),maxchar=maxchar, identical_only=identical_only, sep=sep)
    levels(x) <- tmp
    attr(x,"front") <- attr(tmp,"front")
    attr(x,"back") <- attr(tmp,"back")
    return(x)
  }

  sep_or <- paste0("\\",sep,collapse="|")
  sep_no <- paste0("[^",paste0("\\",sep,collapse=""),"]")
  reg_backsplit <- paste0("^(.*)(",sep_or,")(",sep_no,"*)$")
  reg_frontsplit <- paste0("^(",sep_no,"*)(",sep_or,")(.*)$")
  
  if(max(nchar(x)) <= maxchar & !identical_only) return(x)
  #cut beginning
  front <- NULL
  while(length(unique(sub(reg_frontsplit,"\\1",x)))==1 & all(grepl(sep_or,x))) {
    front <- paste0(front, sub(reg_frontsplit,"\\1\\2",x[1]))
    x <- sub(reg_frontsplit,"\\3",x)
  }
  attr(x,"front") <- front
  if(max(nchar(x)) <= maxchar & !identical_only) return(x)
  #cut end
  back <- NULL
  while(length(unique(sub(reg_backsplit,"\\3",x)))==1 & all(grepl(sep_or,x))) {
    back <- paste0(sub(reg_backsplit,"\\2\\3",x[1]),back)
    x <- sub(reg_backsplit,"\\1",x)
  }
  attr(x,"back") <- back
  if(max(nchar(x)) <= maxchar | identical_only) return(x)
  return(substring(x,1,maxchar))
}
  