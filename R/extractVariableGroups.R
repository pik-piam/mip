#' extractVariableGroups
#' 
#' Groups variable names by groups based on the |+| separators given in the variable names
#' 
#' @param x a vector of variable names
#' @param keepOrigNames if set, the returned list contains the original variables
#' (to the value of which the grouped ones have to sum up) as names instead of
#' made up group names, if they exist. The current implementation goes up to two levels (++) deep.
#' @return a named list of variable groups with group name as name and vector of entities as content
#' @author David Klein, Jan Philipp Dietrich
#' @seealso \code{\link{plotstyle.add}}
#' @examples
#' x <- c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb")
#' mip::extractVariableGroups(x)
#' @export

extractVariableGroups <- function(x,keepOrigNames=FALSE) {
  tmp <- function(x,sep="|+|",ext="",allVars,keepOrigNames=FALSE) {
    if (keepOrigNames) ext<-""
    y <- strsplit(x,sep, fixed=TRUE)
    out <- list()
    for(j in 1:length(y)) {
      for(i in 1:(length(y[[j]])-1)) {
        name <- paste0(paste(y[[j]][1:i],collapse=sep),ext)
        ind<-grep(gsub("\\|","\\\\|",paste0("^",name,"$")),
                  sub("\\|\\+\\+|\\|\\+","",sub(" \\(.*.\\)$","",allVars)))
        if (keepOrigNames ) try(name<-allVars[[ind]],silent = T)
        name <- as.character(name)
        out[[name]] <- c(out[[name]],x[j])
      }
    }
    return(out)
  }
  out <- list()
  for(i in 1:10) {
    sep <- paste0("|",paste(rep("+",i),collapse=""),"|")
    matches <- grep(sep,x,fixed=TRUE, value = TRUE)
    if(length(matches)==0) break()
    ext <- ifelse(i>1,paste0(" ",i),"")
    out <- c(out,tmp(matches,sep=sep,ext=ext,allVars = x,keepOrigNames))
  }
  return(out)
}



