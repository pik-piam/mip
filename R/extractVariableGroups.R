#' extractVariableGroups
#'
#' Groups variable names by summation groups based on the |+| separators given in the variable names.
#' If no |+| are present the function will try to derive summation groups based on | separators.
#'
#' @param x a vector of variable names
#' @param keepOrigNames if set, the returned list contains the original variables
#' (to the value of which the grouped ones have to sum up) as names instead of
#' made up group names, if they exist.
#' @param sorted boolean, indicating whether the variables within each group should be returned alp
#' @return a named list of variable groups with group name as name and vector of entities as content
#' @author Anastasis Giannousakis, David Klein, Jan Philipp Dietrich
#' @seealso \code{\link{plotstyle.add}}
#' @importFrom stringr str_match_all
#' @examples
#' x <- c("a|+|1|+|aa","a|+|2|abc","a|+|1|+|bb","a|+|1|+|cc","a|+|3|+|aa","a|+|3|+|bb")
#' mip::extractVariableGroups(x)
#' @export

extractVariableGroups <- function(x, keepOrigNames=FALSE, sorted = FALSE) {

  spltM<-function(y) {
    return(strsplit(y,"\\|"))
  }

  tmp <- function(x,sep="|+|",ext="",allVars,keepOrigNames=FALSE) {
    if (keepOrigNames) ext<-""
    y <- strsplit(x,sep, fixed=TRUE)
    out <- list()
    for(j in 1:length(y)) {
      for(i in 1:(length(y[[j]])-1)) {
        name <- paste0(paste(y[[j]][1:i],collapse=sep),ext)
        ind<-NULL
        try(ind<-grep(gsub("\\|","\\\\|",gsub("\\|[\\+]{1,}","",paste0("^",name,"$"))),
                  gsub("\\|[\\+]{1,}","",sub(" \\(.*.\\)$","",allVars))),silent = T)
        if (keepOrigNames & length(ind) > 0) try(name<-allVars[[ind]],silent = T)
        name <- as.character(name)
        out[[name]] <- c(out[[name]],x[j])
      }
    }
    return(out)
  }
  if (any(grepl("\\|\\++\\|",x))) {
    maxplus <- max(nchar(unlist(str_match_all(x, "\\++")), keepNA = FALSE))
    out <- list()
    for(i in seq(maxplus)) {
      sep <- paste0("|",paste(rep("+",i),collapse=""),"|")
      matches <- grep(sep,x,fixed=TRUE, value = TRUE)
      if(length(matches)==0) next()
      ext <- ifelse(i>1,paste0(" ",i),"")
      out <- c(out,tmp(matches,sep=sep,ext=ext,allVars = x,keepOrigNames))
    }
  } else {
    if (any(grepl(")$",x))) warning("closing parentheses detected in variable names, if these are units the corresponding variables might not be grouped")
    # find out how many levels of nested variables to expect
    prev<-1
    for (i in x) prev <- max(nchar(gsub("[^|]","",i))+1,prev)
    tmp <- sapply(x,spltM)
    out <- list()
    for (k in prev:2) {
      for (i in which(sapply(tmp,length)==k)) {
        for (j in which(sapply(tmp,length)==k-1)) {
          if (all(tmp[[i]][1:k-1]==tmp[[j]])) {
            out[[names(tmp[j])]] <- c(out[[names(tmp[j])]],names(tmp[i]))
          }
        }
      }
    }
  }
  if (isTRUE(sorted)) {
    out <- out[order(names(out))]
    out <- lapply(out, sort)
  }
  return(out)
}
