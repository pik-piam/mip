#' Returns plot styles for given entities
#' 
#' Returns a named vector (using entitiy names) with style codes (e.g. colors) for given entities.
#' 
#' @param ... One or more strings or a vector of strings with names of entities (regions, variable names, etc.). Units in brackets "(US$2005/GJ)" will be ignored. If left empty all available entities will be used
#' @param out Switch defining which type of plot style you want to get: Currently you can choose between "color", "legend" and "all" (the latter will return a dataframe with all available plot styles)
#' @param unknown Optional data frame defining plot styles for unknown entities. A default color map will be used for unknown entities if nothing is provided here
#' @param plot If TRUE plots with all given entities and their colors will be produced (to produce plots with all available entitities leave the \code{...} entry empty!)
#' @return Plot styles for given entities
#' @author David Klein, Jan Philipp Dietrich
#' @seealso \code{\link{plotstyle.add}}
#' @examples
#' entities <- c("AFR","AAA","AFR","UNKNOWN_ELEMENT2")
#' plotstyle(entities)
#' unknown=data.frame(row.names=c("AAA","UNKNOWN_ELEMENT2"),
#'                    color=c("#123456","#345678"),
#'                    legend=c("l_AAA","l_UNKNOWN_ELEMENT2"))
#' plotstyle(entities,unknown=unknown)
#' plotstyle(entities,out="legend")
#' plotstyle(entities,out="all")
#' @export
#' @importFrom utils read.csv2
#' @importFrom grDevices rgb
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot geom_bar coord_flip theme element_blank labs aes
#' @importFrom grDevices colorRampPalette

plotstyle <- function(..., out="color", unknown=NULL, plot=FALSE) {
  
  luplot<-list()
  luplot$plotstyle <- read.csv2(system.file("extdata","plotstyle.csv",package = "mip"),stringsAsFactors = F,row.names=1)
  
  if(is.null(out)) {
    out <- "color"
  } else if(!(out %in% c(names(luplot$plotstyle),"all"))) {
    stop('Unknown style type "',out,'"!')
  }
  
  # returns n levels of gray (in hexadecimal foramt) between lower (default=0: black) and upper (default=1: white) bound
  .makegray <- function(n,from=0,to=1) {
    u <- seq(from,to,length.out=n)
    u <- t(matrix(rep(u,each=3),nrow=3))
    return(rgb(u))  
  }
  
  # make sure that luplot$plotstyle is of type data.frame
  class(luplot$plotstyle) <- "data.frame"
  
  # choose plot colors for given entities
  entity <- c(...)
  if(is.null(entity)) {
    entity <- row.names(luplot$plotstyle)
  } else {
    entity[is.na(entity)] <- "NA"
    entity <- unlist(lapply(strsplit(entity," \\("),function(x) x[1]))    
  }
  
  uq_entity <- unique(entity)
  res <- luplot$plotstyle[uq_entity,]
  row.names(res) <- uq_entity
  
  # count unknown entities
  nna <- sum(!complete.cases(res))

  # replace NA
  if (nna != 0) {
    if (is.null(unknown)) {
      # if you need less than 10 colors brew and ramp for the exact number of colors min(9,nna) -> nna (ramping has no effect -> take the original brewer colors)
      # only if you need more than 9 colors start ramping between maximal 9 brewer colors effectively
      # replace NA in color with freshly brewed colors from "Set1"
      tmpcols <- brewer.pal(9,"Set1")
      if(nna<9) tmpcols <- tmpcols[1:nna]
      set1 <- colorRampPalette(tmpcols)
      res$color[is.na(res$color)] <- set1(nna) #.makegray(nna,from=0.2,to=0.8)
      # replace NA in legends with row names (= entitiy name)
      res$legend[is.na(res$legend)] <- row.names(res[is.na(res$legend),])
    } else {
      if (out=="all") {
        if (!all(names(unknown) %in% names(luplot$plotstyle))) {
          stop("There are elements in names(unknown) that are not in names(plotstyle)!")
        }
        for (n in names(unknown)){
          res[[n]][is.na(res[[n]])] <- as.character((unknown[[n]][1:nna]))
        }
      } else if(!(out %in% names(unknown))) {
        stop('Style type "',out,'" is not existing in argument "unknown"!')
      } else {
        rows_without_data <- rownames(subset(res,is.na(res[[out]])))
        res[rows_without_data,out] <- as.character(unknown[rows_without_data,out])
      }
    }
  }
  
  if (plot) {
    df<-data.frame(x=uq_entity,c=res$color)
    df$x<-factor(df$x,levels=rev(uq_entity)) # prevent ggplot from sorting it alphabetical by giving order explicitly here
  
    ncol    <- 30 # color bars per page
    pagemax <- ceiling(length(res$color)/ncol) # number of pages
    for (page in 1:pagemax) {
      # start and end index for respective page
      from <- (page-1)*ncol+1
      to   <- min(page*ncol,length(res$color))
      # create data frame
      x <- rownames(res)[from:to]
      c <- res$color[from:to]
      df<-data.frame(x=x, c=c)
      # prevent ggplot from sorting it alphabetically by giving order explicitly here
      # using original order of rownames. Reversing it because the bar plot reverses it again
      # To yield the correct mapping between colors and labels the colors have to be also reversed
      df$x<-factor(df$x,levels=rev(x)) 
      # create bar plot
      p1 <- ggplot(data=df, aes(x=x)) + geom_bar(stat="bin",fill=rev(df$c)) + coord_flip() + 
               theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + 
               labs(title=paste0("Color bars (plot ",page," of ",pagemax,")"))

      print(p1)
    }
    
  }
 
  res <- res[entity,]
  
  # select the output data from res according to "out"
  if (out!="all") {
    res <- res[[out]]
    names(res) <- entity
  }
  return(res)
}