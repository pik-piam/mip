#' Generic area plot function. Automatically creates facet grid from data. Optionally adds total line.
#' 
#' @param x Data to plot. Allowed data formats: magpie or quitte. NOTE: To ensure correct conversion to quitte objects, 
#' the dimension that contains the variables must have one of the following names: variable, scenario, or model.
#' @param stack_priority Name of column you want to stack. If you provide more than one column name the 
#' function will scan the columns in the given order and use the first dimension for stacking that has 
#' more than one element.
#' @param total total data to plot. Allowed inputss: magpie, quitte or boolean. If total data is
#' provided by user in magpie or quitte format it will be added to the plot. If user sets total to 
#' TRUE total will be calculated by the function and added to the plot. If total is FALSE the plot 
#' will ignore it.
#' @param scales Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @param shorten Shorten variable names (default is TRUE) by removing categories only if they are identical (for short names in the legend)
#' @param hist Historical data. Allowed data formats: magpie or quitte. NOTE: To ensure correct conversion to quitte objects, 
#' the dimension that contains the variables must have one of the following names: variable, scenario, or model.
#' @param hist_source If there are multiple historical sources the name of the source that you want to be plotted.
#' @author David Klein, Jan Philipp Dietrich
#' @section Example Plot:
#' \if{html}{\figure{mipArea.png}{example plot}}
#' @examples
#' p <- mipArea(x=mip_example_data)
#' # create plot with best-guess design (internally using theme_mip(size=12))
#' p <- mipArea(mip_example_data)
#' # override default theme with theme_grey and move legend to top
#' library(ggplot2)
#' p <- p + theme_grey() + theme(legend.position="top")
#' # go back to theme_mip and increase font size
#' p <- p + theme_mip(size=18)
#' # change facetting
#' p <- p + facet_grid(region~scenario)
#' @importFrom magclass is.magpie getSets
#' @importFrom quitte is.quitte
#' @importFrom ggplot2 ggplot geom_area aes_ geom_line scale_linetype_discrete facet_wrap facet_grid theme scale_fill_manual xlab
#' @importFrom dplyr group_by_ summarise_ ungroup
#' @export
#'
mipArea <- function(x, stack_priority=c("variable", "region"), total=TRUE, scales="fixed", shorten = TRUE, hist = NULL, hist_source = "first"){

  # library(ggplot2)
  # library(magclass)
  # library(quitte)
  # library(dplyr)
  # stack_priority=c("variable", "region")
  # total=TRUE
  # x <- mip_example_data
  # x <- rep[,getYears(rep)<"y2050",var]
  # shorten <- TRUE
  # hist <-  H[,getYears(H)>"y1970",var]
  # hist_source = "Lavinia"

  ############################################
  ######  P R E P A R E   D A T A  ###########
  ############################################

  # To ensure correct conversion to quitte objects, there should be at least  
  # one dimension named "variable" or "scenario" or "model" in MAgPIE objects.
  if(is.magpie(x) & !any(c("variable","model","scenario") %in% getSets(x))) stop("MAgPIE objects need to have at least one dimension named 'variable' or 'model' or 'scenario'.")
  
  x <- as.quitte(x)
  
  # Add dummy scenario if data does not contain a scenario
  # Otherwise selecting all scenarios that are not 'historicl' further down deletes ALL data
  if(all(is.na(x$scenario))) x$scenario <- "default"
  
  # shorten variable names and calc ylab
  if (shorten) x$variable <- shorten_legend(x$variable,identical_only=TRUE)
  ylab <- paste0(sub(".$","",attr(x$variable,"front")),attr(x$variable,"back"))
  # add unit
  unit <- unique(as.character(x$unit))
  ylab <- paste0(ylab, " (",paste0(unit,collapse=" | "),")")
  
  # Repeat the same for history
  if(!is.null(hist)) {
    if(is.magpie(hist) & !any(c("variable","model","scenario") %in% getSets(hist))) stop("MAgPIE objects with historical data need to have at least one dimension named 'variable' or 'model' or 'scenario'.")
    hist <- as.quitte(hist)
    if (shorten) hist$variable <- shorten_legend(hist$variable,identical_only=TRUE)
    # select historical data source
    if(hist_source == "first") {
      hist <- hist[hist$model==levels(hist$model)[1],]
    } else if (hist_source %in% levels(hist$model)) {
      hist <- hist[hist$model==hist_source,]
    } else {
      warning(paste0(hist_source," could not be found in the historical data!"))
      hist <- NULL
    }
  }
  
  ###
  ### Find out which variables to stack and which to put to facet_grid
  ###
  
  # count levels of the given columns
  n_levels <- sapply(subset(x, select=c("variable","region","scenario","model")), nlevels)

  # initialize
  dim_to_stack <- stack_priority

  # Find first dimension in stack_priority that has more than one element.
  # Initialize dim_to_stack (this applies if if all dimensions have only one element)
  dim_to_stack <- stack_priority[1]
  for (s in stack_priority) {
    if(n_levels[s] > 1) {
      dim_to_stack <- s
      break()  
    }
  }
  
  # find and sort dimension with more than one element, exclude dim_to_stack
  # sort: to be able to build the interaction with the smallest number of resulting combinations (further down)
  facets <- sort(n_levels[n_levels>1])
  facets <- setdiff(names(facets),dim_to_stack)
  
  # Combine data and historical data into one object
  if(!is.null(hist)) x <- rbind(x,hist)
  
  # if there are three facet dimensions that have more than one element combine the two
  # smallest ones into the first one to be able to create a 2-D facet_grid later on
  if (length(facets)==3) {
    x[,facets[1]] <- interaction(x[[facets[1]]],x[[facets[2]]])
  }

  # if not provided by user calculate total by summing over dim_to_stack
  if (isTRUE(total)) {
    dim_to_group <- setdiff(c("model","scenario","region","variable","unit","period"),dim_to_stack)
    total <- x %>% group_by_(as.formula(paste("~",dim_to_group[1])),
                             as.formula(paste("~",dim_to_group[2])),
                             as.formula(paste("~",dim_to_group[3])),
                             as.formula(paste("~",dim_to_group[4])),
                             as.formula(paste("~",dim_to_group[5]))) %>% summarise_(value = ~sum(value,na.rm=TRUE)) %>% ungroup()
    # add missing column dim_to_stack to make it convertable to quitte
    total[,dim_to_stack] <- "Total"
  }
  # } else if (shorten_legend(total,identical_only=TRUE) %in% x[,dim_to_stack]){
  #   total <- x[]
  #   
  # }
  
  # convert total to quitte
  if(!identical(FALSE, total)) total <- as.quitte(total)
  
  # separate positive and negative parts of data for area plot
  tmp <- droplevels(x[x$scenario!="historical",])
  
  pos <- tmp
  pos$value[pos$value < 0] <- 0
  neg <- tmp
  neg$value[neg$value > 0] <- 0
  
  if (!is.null(hist)) {
    tmp <- droplevels(x[x$scenario=="historical",])
    postmp <- tmp
    postmp$value[postmp$value < 0] <- 0
    
    negtmp <- tmp
    negtmp$value[negtmp$value > 0] <- 0
  
    pos_h <- NULL
    neg_h <- NULL
  
    # repeat historical data as often as there are scenarios
    # so that they will be plotted for each single scenario
    for (l in levels(pos$scenario)) {
      postmp$scenario <- factor(l)
      negtmp$scenario <- factor(l)

      pos_h <- rbind(pos_h,postmp)
      neg_h <- rbind(neg_h,negtmp)
    }
  }
  
  # split historical and model total
  if(!identical(FALSE, total)) total_x <- droplevels(total[total$scenario!="historical",])
  
  if(!is.null(hist) & !identical(FALSE, total)) {
    tottmp <- droplevels(total[total$scenario=="historical",])
    # repeat historical total as often as there are scenarios
    # so that it will be plotted for each single scenario
    total_h <- NULL
    for (l in levels(pos$scenario)) {
      tottmp$scenario <- factor(l)
      total_h <- rbind(total_h,tottmp)
    }
  }
  
  ############################################
  ###############  P L O T  ##################
  ############################################

  p <- ggplot()+
       geom_area(data=pos,aes_(~period,~value,fill=as.formula(paste("~",dim_to_stack)))) +
       geom_area(data=neg,aes_(~period,~value,fill=as.formula(paste("~",dim_to_stack))))
  
  if (!is.null(hist)) {
       p <- p + geom_area(data=pos_h,aes_(~period,~value,fill=as.formula(paste("~",dim_to_stack))), alpha=0.3)
       p <- p + geom_area(data=neg_h,aes_(~period,~value,fill=as.formula(paste("~",dim_to_stack))), alpha=0.3) 
  }

  
  # define facet_grid
  if (length(facets)==1) p <- p + facet_wrap(as.formula(paste("~",facets)),scales=scales)
  if (length(facets)==2) p <- p + facet_grid(as.formula(paste(facets[1],"~",facets[2])),scales=scales)
  # facet 1 and 2 are combined in dim 1
  if (length(facets)==3) p <- p + facet_grid(as.formula(paste(facets[1],"~",facets[3])),scales=scales)

  # add total to plot as black line
  if (is.quitte(total)) {
    p <- p + geom_line(data=total_x,aes_(~period,~value,linetype=as.formula(paste("~",dim_to_stack))),color="#000000",size=1)
    p <- p + scale_linetype_discrete(labels="Total",name="")
    if(!is.null(hist)) p <- p + geom_line(data=total_h,aes_(~period,~value,linetype=as.formula(paste("~",dim_to_stack))),color="#000000",size=1,alpha=0.3)
  }

  # plot settings
  p <- p + xlab("Year")
  p <- p + ylab(ylab)
  #p <- p + ggtitle(ylab)
  
  # update theme
  p <- p + theme_minimal()
  p <- p + theme_mip(size=12)
  p <- p + theme(axis.title.x=element_blank())

  # use plotstyle colours and labels by default
  p <- p + scale_fill_manual(values = plotstyle(as.character(unique(x[[dim_to_stack]]))),
                             name   = "") 

  return(p)
}
