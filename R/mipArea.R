#' Generic area plot function. Automatically creates facet grid from data. Optionally adds total line.
#' 
#' @param x Data to plot. Allowed data formats: magpie or quitte
#' @param stack_priority Name of column you want to stack. If you provide more than one column name the 
#' function will scan the columns in the given order and use the first dimension for stacking that has 
#' more than one element.
#' @param total total data to plot. Allowed inputss: magpie, quitte or boolean. If total data is
#' provided by user in magpie or quitte format it will be added to the plot. If user sets total to 
#' TRUE total will be calculated by the function and added to the plot. If total is FALSE the plot 
#' will ignore it.
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
#' @importFrom magclass is.magpie
#' @importFrom quitte is.quitte
#' @importFrom ggplot2 ggplot geom_area aes_ geom_line scale_linetype_discrete facet_wrap facet_grid theme scale_fill_manual xlab
#' @importFrom dplyr group_by_ summarise_ ungroup
#' @export
#'
mipArea <- function(x, stack_priority=c("variable", "region"), total=TRUE){

  # library(ggplot2)
  # library(magclass)
  # library(quitte)
  # library(dplyr)
  # stack_priority=c("variable", "region")
  # total=TRUE
  # x <- mip_example_data

  ############################################
  ######  P R E P A R E   D A T A  ###########
  ############################################

  x <- as.quitte(x)
  
  # shorten variable names and calc ylab
  x$variable <- shorten_legend(x$variable,identical_only=TRUE)
  ylab <- paste0(sub(".$","",attr(x$variable,"front")),attr(x$variable,"back"))
  # add unit
  unit <- unique(as.character(x$unit))
  ylab <- paste0(ylab, " (",paste0(unit,collapse=" | "),")")
  
  ###
  ### Find out which variables to stack and which to put to facet_grid
  ###
  
  # count levels of the given columns
  n_levels <- sapply(subset(x, select=c("variable","region","scenario","model")),nlevels)

  # initialize
  dim_to_stack <- stack_priority

  # find first dimension in stack_priority that has more than one element  
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
  pos <- x
  pos$value[pos$value < 0] <- 0
  neg <- x
  neg$value[neg$value > 0] <- 0
  
  ############################################
  ###############  P L O T  ##################
  ############################################
  
  p <- ggplot()+
       geom_area(data=pos,aes_(~period,~value,fill=as.formula(paste("~",dim_to_stack)))) +
       geom_area(data=neg,aes_(~period,~value,fill=as.formula(paste("~",dim_to_stack)))) 

  # define facet_grid
  if (length(facets)==1) p <- p + facet_wrap(as.formula(paste("~",facets)))
  if (length(facets)==2) p <- p + facet_grid(as.formula(paste(facets[1],"~",facets[2])))
  # facet 1 and 2 are combined in dim 1
  if (length(facets)==3) p <- p + facet_grid(as.formula(paste(facets[1],"~",facets[3])))

  # add total to plot as black line
  if (is.quitte(total)) {
    p <- p + geom_line(data=total,aes_(~period,~value,linetype=as.formula(paste("~",dim_to_stack))),color="#000000",size=1)
    p <- p + scale_linetype_discrete(labels="Total",name="")
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
