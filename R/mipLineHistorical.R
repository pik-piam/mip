#' Compares data by producing line plot
#' 
#' @param x           Data to plot. Allowed data formats: magpie or quitte
#' @param x_hist      historical data to plot. Allowed data formats: magpie or quitte, If no hostoric information is provided the plot will ignore it.
#' @param color.dim   dimension used for different colors, default="moscen"; can only be choosen freely if x_hist is NULL.
#' @param linetype.dim dimension used for different line types, default=NULL
#' @param facet.dim   dimension used for the facets, default="region"
#' @param funnel.dim  dimension used for different funnels, default=NULL
#' @param ylab y-axis label
#' @param xlab x-axis label, default="Year"
#' @param color.dim.name name for the color-dimension used in the legend
#' @param title       title of the plot
#' @param ybreaks      add breaks for the y axis
#' @param ylim        y limits
#' @param ylog        =T if the-axis should be logarithmic
#' @param size        text size in the plot
#' @param scales  Are scales shared across all facets (the default, "fixed"), or do they vary across rows ("free_x"), columns ("free_y"), or both rows and columns ("free")
#' @param leg.proj to add a detailed legend for the projected data. Default is \code{FALSE}.
#' @param plot.priority Sets the order of ploting and overlap of the data by specifying a vector of three stirng elements. Argument \code{x} stands for model output, \code{x_hist} is for obeserved (historical data) and \code{x_proj} is for projected data from other models. 
#' @param ggobject returns a ggplot object. Default is \code{TRUE}.
#' @param paper_style removes grey color from facets if \code{TRUE} Default is \code{FALSE}.
#' @param xlim        x axis limits as vector with min and max year
#' @param facet.ncol  number of columns used for faceting, default=3.
#' @param legend.ncol number of columns used in legends, default=1.
#'
#' @author Lavinia Baumstark, Mishko Stevanovic, Florian Humpenoeder
#'
#' @section Example Plot:
#' \if{html}{\figure{mipLineHistorical.png}{example plot}}
#' \if{html}{\figure{mipLineHistorical_withoutHistoric.png}{example plot}}
#' @examples
#' 
#'   \dontrun{
#'     p <- mipLineHistorical(x,x_hist=hist,ylab="example",xlab="Year",title=NULL)
#'   }
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom ggplot2 ggplot aes_ geom_point scale_color_hue element_line aes_string geom_vline %+replace% scale_color_manual ggtitle theme_bw scale_alpha_manual coord_cartesian
#' margin element_rect ggplot_gtable ggplot_build scale_y_log10 coord_trans
#' @export
#' 

mipLineHistorical <- function(x,x_hist=NULL,color.dim="moscen",linetype.dim=NULL,facet.dim="region",funnel.dim=NULL,
                              ylab=NULL,xlab="Year",title=NULL,color.dim.name="Model output",ybreaks=NULL,ylim=NULL,
                              ylog=NULL, size=14, scales="fixed", leg.proj=FALSE, plot.priority=c("x","x_hist","x_proj"),
                              ggobject=TRUE,paper_style=FALSE,xlim=NULL,facet.ncol=3,legend.ncol=1) {

  x <- as.quitte(x)
  
  class(x) <- setdiff(class(x),"data.table")
  
  x <- droplevels(x)
  x <- x[!is.na(x$value),]
  if(all(is.na(x$scenario))) x$scenario <- ""
  if(all(is.na(x$model))) x$model <- ""
  # add a model.scenario column
  x$moscen <- interaction(x$model,x$scenario)
  
  ## main data object
  a <- x
  a$id <- "x"
  
  if(is.data.frame(x_hist) && dim(x_hist)[1]==0) x_hist <- NULL
  if(!is.null(x_hist)) {
    class(x_hist) <- setdiff(class(x_hist),"data.table")
    x_hist <- as.quitte(x_hist)
    x_hist <- droplevels(x_hist)
    x_hist <- x_hist[!is.na(x_hist$value),]
    x_hist$moscen <- interaction(x_hist$model,x_hist$scenario)
    x_hist$id <- ""
    x_hist[x_hist$scenario!="historical","id"] <- "x_proj"
    x_hist[x_hist$scenario=="historical","id"] <- "x_hist"
    a <- rbind(a,x_hist)
  } 
  
  # remove missing values
  a <- a[!is.na(a$value),]
  a$scenario <- as.factor(a$scenario)
  a$id <- factor(a$id, ordered=TRUE, levels=rev(plot.priority))
  
  # make line plot of data
  p <- ggplot()
  if (color.dim!="moscen" && !is.null(x_hist)) stop("color.dim can only be choosen freely if x_hist is NULL!")

  # log scale
  if(!is.null(ylog)) {
    if(!is.null(ybreaks)) {
      p <- p + scale_y_log10(breaks=ybreaks)
    }
    if(!is.null(ylim)) {
      p <- p + coord_trans(y = "log10", limy = ylim)
    }else{
      p <- p + coord_trans(y = "log10")
    }
  }
  
  if(!is.null(xlim)) p <- p + coord_cartesian(xlim=xlim)
  
  # facet
  if(!is.null(facet.dim)) p <- p + facet_wrap(facet.dim, ncol=facet.ncol, scales=scales) 
  
  # get the plotting year maximum
  ## has to be determened on maximum of model output and historic data
  ymax <- max(a$period[a$id=="x_hist"],a$period[a$id=="x"])
  

  # internal functions for plotting of different types of data
  priority_x <- function(p){
    p <- p + geom_line(data=a[a$id=="x",], aes_string(x="period",y="value",color=color.dim),size=1)
    p <- p + geom_point(data=a[a$id=="x",], aes_string(x="period",y="value",color=color.dim),size=1.5)
    return(p)
  }
  
  priority_x_hist <- function(p,MarkerSize=2.5){
    if(any(a$id=="x_hist")) {
      p <- p + geom_line(data=a[a$id=="x_hist",], aes_string(x="period",y="value",color="model"),size=1, alpha=0.3)
      #plot for creating the legend
      p <- p + geom_point(data=a[a$id=="x_hist",], aes_string(x="period",y="value",color="model",fill="model"),size=0)
      #plot the data without legend
      p <- p + geom_point(data=a[a$id=="x_hist",], aes_string(x="period",y="value",color="model",fill="model"),size=MarkerSize, shape="+", alpha=0.8,show.legend = FALSE)
    }
    return(p)
  }
  
  priority_x_proj <- function(p){
    if(any(a$id=="x_proj")) {
      if(leg.proj){
        #plot for creating the legend
        p <- p + geom_line(data=a[a$id=="x_proj" & a$period<=ymax,], 
                           aes_string(x="period",y="value",group="moscen", color="moscen",linetype=linetype.dim,alpha="moscen"),
                           size=0)
        #plot the data
        p <- p + geom_line(data=a[a$id=="x_proj" & a$period<=ymax,], 
                           aes_string(x="period",y="value",group="moscen", color="moscen",linetype=linetype.dim),
                           size=0.8, alpha=.7,show.legend = TRUE)
      } else {
        #plot for creating the legend
        p <- p + geom_line(data=a[a$id=="x_proj" & a$period<=ymax,], 
                           aes_string(x="period",y="value",group="moscen",linetype=linetype.dim,alpha="model"),
                           size=0, color="white")
        #plot the data
        p <- p + geom_line(data=a[a$id=="x_proj" & a$period<=ymax,], 
                           aes_string(x="period",y="value",group="moscen",linetype=linetype.dim),
                           size=0.8, alpha=.5, color="#A1A194",show.legend = TRUE)
      }
    } 
    return(p)
  }
  
  # plot the data accordig to plotting priority
  plot.priority <- rev(plot.priority)
  for(i in 1:length(plot.priority)){
    if(plot.priority[i] == "x_hist" & i>1 ){  ## if the historic values are plotted on top of the scenario ones, they should be smaller
      p <- priority_x_hist(p,MarkerSize = 5)
    }
    else {
      p <- eval(parse(text = paste0("priority_",plot.priority[i],"(p)")))
    }
  }
  
  # datasources ordering // matrix // needed for colors and legend
  model_output <- as.vector(unlist(unique(a[a$id=="x",color.dim])))
  historical <- as.vector(unlist(unique(a[a$id=="x_hist","model"])))
  if(leg.proj) {
    projection <- as.vector(unlist(unique(a[a$id=="x_proj","moscen"])))
  } else {
    projection <- as.vector(unlist(unique(a[a$id=="x_proj","model"])))
  }
  
  sources <- as.vector(interaction(c(model_output,historical,projection)))
  
  # colors
  color_set <- plotstyle(sources)
  #the color legend includes colors for model_output, historical and projection at this stage
  if(!ggobject)
    p <- p + scale_color_manual(values=color_set, name="Legend")

  # add a vertical line for the starting year of the resutls
  p <- p + geom_vline(xintercept=as.numeric(min(x$period)),linetype=2)
  
  # labels
  p <- p + xlab(xlab) 
  p <- p + ylab(ylab) 
  p <- p + ggtitle(title) 
  
  text_size <- size
  
  p <- p + theme_bw(text_size) %+replace% 
    theme(
      plot.title=element_text(size=text_size+4, face="bold", vjust=1.5),
      strip.text.x=element_text(size=text_size, margin=margin(4,2,4,2,"pt")), 
      axis.title.y=element_text(angle=90, size=text_size, face="bold", vjust=1.3), 
      axis.text.y=element_text(size=text_size, colour="black"), 
      axis.title.x=element_text(size=text_size, face="bold", vjust=-0.3), 
      axis.text.x=element_text(size=text_size, angle=90, hjust=.5, colour="black"),
      legend.position="bottom",
      legend.direction = "horizontal",
      legend.title=element_text(size=text_size,face="bold"), 
      legend.title.align=0,
      legend.text=element_text(size=text_size-2),
      #legend.background=element_rect(fill="white"),
      legend.key=element_blank(),
      #legend.spacing.x=unit(1, "cm"),
      #plot.margin= unit(c(1, 1, 0, 1.7),"lines")
    )
  
  if (paper_style) p <- p + theme(strip.background = element_blank())
  
  if(ggobject) {
    #manipulate the legends: color = model_output, fill = historical, alpha = projection
    #color: show only model_output
    #fill: add colors for historical and keep shape symbol
    #alpha: add colors for projection depending on leg.proj
    p <- p + scale_color_manual(color.dim.name,values = color_set, breaks=model_output,labels=sub("\\."," ",model_output),guide=guide_legend(order=1,title.position = "top", ncol=legend.ncol))
    p <- p + scale_fill_manual("Historical data",values = color_set[historical],breaks=historical,
                               guide=guide_legend(override.aes = list(colour=color_set[historical],shape="+",linetype=0,size=5),order=2,title.position = "top", ncol=legend.ncol))
    if(leg.proj) p <- p + scale_alpha_manual("Other projections",values = seq(0.1,1,length.out = length(projection)),breaks=projection,labels=sub("\\."," ",projection),guide=guide_legend(override.aes = list(colour=color_set[projection],shape=NULL,linetype=1,size=1,alpha=0.5),order=3,title.position = "top", ncol=legend.ncol))
    else p <- p + scale_alpha_manual("Other projections",values = seq(0.1,1,length.out = length(projection)),breaks=projection,labels=sub("\\."," ",projection),guide=guide_legend(override.aes = list(colour="#A1A194",shape=NULL,linetype=1,size=1,alpha=0.5),order=3,title.position = "top", ncol=legend.ncol))
    p <- p + guides(linetype=guide_legend(order=4,title.position="top",ncol=legend.ncol))
    
    return(p)
  }
  
  p <- p + theme(legend.position="none")

  # LEGEND:

  # extract the legend from a ggplot
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }
  
  # theme for legend
  theme_legend <- function(){
    theme(legend.title=element_text(size=text_size,face="bold"), 
          legend.text=element_text(size=text_size-2),
          legend.background=element_rect(fill="white"),
          legend.key=element_blank())
  }
  
  .legend_shares <- function(a){
    x      <- droplevels(a[a$id=="x",])
    x_hist <- droplevels(a[a$id=="x_hist",])
    x_proj <- droplevels(a[a$id=="x_proj",])
    # number of scenarios in modeled, historic, and projected data
    col1 <- nrow(unique(x[c("model","scenario")]))
    col2 <- ifelse(is.null(x_hist),0,nrow(unique(x_hist[,c("model","scenario")])))
    if(leg.proj) {
      col3 <- ifelse(is.null(x_proj),0,nrow(unique(x_proj[,c("model","scenario")])))
    } else col3 <- ifelse(is.null(x_proj),0,nrow(unique(x_proj[,"model", drop=FALSE])))
    
    # number of characters of each model-scenario for each data type
    nch1 <- max(nchar(max(levels(x$moscen))),nchar("Model output"))
    nch2 <- ifelse(col2==0,0,max(nchar(max(levels(x_hist$model))),nchar("Historical data")))
    nch3 <- ifelse(col3==0,0,max(ifelse(leg.proj,nchar(max(levels(x_proj$moscen))),nchar(max(levels(x_proj$model)))),nchar("Other projections")))
    allnch <- nch1 + nch2 + nch3
    c1 <- nch1/allnch
    c2 <- nch2/allnch
    c3 <- nch3/allnch
    
    out <- list()
    out$shares <- c(c1,c2,c3)
    out$nchar <- ceiling(out$shares*50)
    out$col1 <- col1
    out$col2 <- col2
    out$col3 <- col3
    
    return(out)
  }
  
  lsh <- .legend_shares(a)
  
  # construct the legend
  leg <- list()
  ## legend for the model output
  if(lsh$col1>0){
    l1 <- ggplot(data=a[a$id=="x",])
    l1 <- l1 + geom_line(aes_(x=~period,y=~value,color=~moscen),size=1)
    l1 <- l1 + geom_point(aes_(x=~period,y=~value,color=~moscen),size=1.5)
    l1 <- l1 + scale_color_manual(values=color_set[1:lsh$col1],
                                  breaks=interaction(unlist(a[a$id=="x","model"]),unlist(a[a$id=="x","scenario"])), 
                                labels=shorten_legend(interaction(unlist(a[a$id=="x","model"]),unlist(a[a$id=="x","scenario"]),sep=" "),lsh$nchar[1]),
                                name="Model output")
    l1 <- l1 + theme_legend()
    leg[["results"]] <- suppressMessages(g_legend(l1))
  }
  
  ## legend for the historical data
  if(lsh$col2>0 & "x_hist" %in% levels(a$id)){
    l2 <- ggplot(data=a[a$id=="x_hist",])
    l2 <- l2 + geom_line(aes_(x=~period,y=~value,color=~model),size=1,alpha=.15)
    l2 <- l2 + geom_point(aes_(x=~period,y=~value,color=~model),size=3.5,shape="+")
    l2 <- l2 + scale_color_manual(values=as.vector(color_set[(lsh$col1+1):(lsh$col1+lsh$col2)]),name="Historical data")
    l2 <- l2 + theme_legend()
    leg[["historical"]] <- g_legend(l2)  
  }
  
  ## legend for other projections
  if(lsh$col3>0 & "x_proj" %in% levels(a$id)){
    if(leg.proj){
      l3 <- ggplot(data=a[a$id=="x_proj",])
      l3 <- l3 + geom_line(aes_(x=~period,y=~value,color=~moscen),size=1,alpha=.7)
      l3 <- l3 + scale_color_manual(values=color_set[(lsh$col1+lsh$col2+1):(lsh$col1+lsh$col2+lsh$col3)],
                                    breaks=interaction(unlist(a[a$id=="x_proj","model"]),unlist(a[a$id=="x_proj","scenario"])), 
                                    labels=shorten_legend(interaction(unlist(a[a$id=="x_proj","model"]),unlist(a[a$id=="x_proj","scenario"]),sep=" "),lsh$nchar[3]),
                                    name="Other projections")
      l3 <- l3 + theme_legend()
      leg[["other"]] <- g_legend(l3)  
    } else{
        l3 <- ggplot(data=a[a$id=="x_proj",])
        l3 <- l3 + geom_line(aes_(x=~period,y=~value,color=~model),size=1,alpha=.5)
        l3 <- l3 + scale_color_manual(values=rep("#A1A194",lsh$col3),
                                      breaks=unique(unlist(a[a$id=="x_proj","model"])), 
                                      labels=shorten_legend(unique(unlist(a[a$id=="x_proj","model"])),lsh$nchar[3]),
                                      name="Other projections")
    }
    l3 <- l3 + theme_legend()
    leg[["other"]] <- g_legend(l3)  
  } 
  
  
  args <- leg
  args[["ncol"]] <- length(args)
  args[["widths"]]<- lsh$shares[lsh$shares!=0]
  leg <- do.call(arrangeGrob,args=args)
  
  # construct the final plot
  out <- suppressMessages(grid.arrange(arrangeGrob(p,leg,ncol=1,heights=c(0.76,0.24))))
  
  return(invisible(out))
}
