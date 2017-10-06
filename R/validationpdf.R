#' Create a validation PDF file
#' 
#' @param x     Data to be validated. All formats allowed which can be converted to quitte (including characters containing the path to a mif file)
#' @param hist  Validation data.All formats allowed which can be converted to quitte (including characters containing the path to a mif file)
#' @param file file name of the output PDF or a Sweave object. If a sweave object is provided the function will return the updated object, otherwise
#' it will write its content to the file
#' @param style data style for the returned data. Currently available: "trafficlight", "detailed", "comparison"
#' @param only_historical boolean deciding whether only historical data should be used for validation or also projections from other sources
#' @param digits integer indicating the number of digits to be shown.
#' @param filter Additional filter to be applied on the data to only plot a subset of the provided data
#' @param prefix Prefix which will be put in front of each part title (useful if validation is integrated into a bigger document)
#' @param hideEmptySection removes sections in output file which would be empty for the reason that variables in input 'x' has have correspondance in the hist file
#' @param show_stats boolean specifying whether additional statistic section should show up or not
#' @param debug Switch to activate or deactivate debug mode.
#' @author Jan Philipp Dietrich
#' @importFrom magclass as.magpie ndata dimSums nyears getRegions getNames nregions
#' @importFrom quitte as.quitte getRegs
#' @importFrom lusweave swopen swclose swfigure swR swtable swlatex
#' @importFrom trafficlight trafficlight
#' @importFrom reshape2 melt
#' @export
#' 

validationpdf <- function(x,hist,file="validation.pdf",style="comparison", only_historical=FALSE, digits=3, filter=NULL, prefix=NULL, hideEmptySection=FALSE, show_stats=TRUE, debug=getOption("debug")) {
  if(is.null(debug)) debug <- FALSE
  styles <- c("trafficlight","comparison","detailed")
  if(!(style %in% styles)) stop("Unknown style \"",style,"\", please use one of the following: ",paste(styles,collapse=", "))
  x <- as.quitte(x)
  if(!is.null(hist)) {
    hist <- as.quitte(hist)
    # strip down NAs
    hist <- hist[!is.na(hist$value),]
  }
  
  #rename "World" in GLO
  .tmp <- function(x) {
    if(is.factor(x)) {
      if("World" %in% levels(x)) levels(x)[levels(x)=="World"] <- "GLO"
    } else {
      w <- which(x=="World")
      if(length(w)>0) x[w] <- "GLO"
    }
    return(x)
  }
  x$region <- .tmp(x$region)
  hist$region <- .tmp(hist$region)
  
  if(all(is.na(x$scenario))) x$scenario <- "default"
  
  tmp <- function(filter,x) {
    if(is.null(filter)) return(TRUE)
    else return(grepl(filter,x))
  }
  
  #filter data
  x <- droplevels(x[!is.na(x$value) & tmp(filter,x$variable),])
  hist <- droplevels(hist[!is.na(hist$value) &  tmp(filter,hist$variable),])
  if(only_historical)  hist <- droplevels(hist[hist$scenario=="historical",])
  
  if(dim(x)[1]==0) stop("No data remaining to be validated after all filter have been applied!")
  if(dim(hist)[1]==0) stop("No validation data remaining after all filter have been applied!")
  
  x$fullname    <- paste0(x$variable, " (",x$unit,")")
  hist$fullname <- paste0(hist$variable, " (",hist$unit,")")
  
  vars     <- x$fullname

  remove_symbols <- function(x) return(gsub("(\\++|\\-+)\\|","", x))
  extract_symbols <- function(x) return(gsub("(\\+|\\-)\\|","\\1",gsub("[^\\|^\\+^\\-]*","",x)))
  
  x$fullname    <- as.factor(remove_symbols(x$fullname))
  hist$fullname <- as.factor(remove_symbols(hist$fullname))
  
  xtrax    <- setdiff(x$fullname,hist$fullname)
  xtrahist <- setdiff(hist$fullname,x$fullname)
  
  x$variable    <- remove_symbols(x$variable)
  hist$variable <- remove_symbols(hist$variable)
 
  splitvars <- function(vars, nlevel=4) {
    properties <- extract_symbols(vars)
    fullname   <- remove_symbols(vars)
    unit <- sub("^.* \\(([^\\)]*)\\)$","\\1",fullname)
    vars <- sub("^(.*) \\(([^\\)]*)\\)$","\\1",fullname)
    .split <- function(vars, ignore_rest=FALSE) {
      out <- vars
      tmp <- vars
      for(i in 1:(nlevel-1)) {
        out <- cbind(out,sub("\\|.*$","",tmp))
        tmp <- sub("^[^\\|]*(\\||$)","",tmp)
      }
      if(ignore_rest) tmp <- sub("\\|.*$","",tmp)
      return(cbind(out,tmp))
    }
    out <- cbind(fullname, unit, .split(vars), .split(properties,TRUE))
    out <- as.data.frame(out)
    names(out) <- c("fullname","unit","name",paste0("level",1:nlevel),"props",paste0("prop",1:nlevel))
    return(out)
  }
  vars <- splitvars(vars, nlevel=4)
  
  calc_digits <- function(tmpx,digits) {
    d <- max(0,-1*ceiling(log10(max(tmpx)))+digits)
    if(is.finite(d) & is.numeric(d)) return(d)
    return(0)
  }
  
  prepmagpie <- function(x,hist) {
    tmpsort <- function(x){
      reg <- sort(getRegions(x))
      if("GLO" %in% reg) reg <- c("GLO",reg[reg!="GLO"])
      return(x[reg,,])
    }
    tmpx <- tmpsort(as.magpie(as.quitte(x)))
    tmphist <- tmpsort(as.magpie(as.quitte(hist)))
    regs <- intersect(getRegions(tmpx),getRegions(tmphist))
    return(list(x=tmpx[regs,,],hist=tmphist[regs,,]))      
  }
  
  preptitle <- function(x) {
    # put unit in brackets
    x <- sub("\\.([^\\.]*)$"," (\\1)",x)
    # combine model and scenario
    x <- sub(".", " ", x, fixed=TRUE)
    # remove scenario name if it is "historical"
    x <- sub(" historical", "", x, fixed=TRUE)
    # replace remaining .-separators
    x <- gsub(".", " | ", x, fixed=TRUE)
    return(x)
  }
  
  histtables <- function(sw, m, digits) {
    dimnames(m$x)[[2]] <- sub("y","",dimnames(m$x)[[2]])
    for(s in 1:ndata(m$x)) {
      swlatex(sw,"\\hspace{10 mm}")
      swtable(sw,m$x[,,s],preptitle(getNames(m$x)[s]), digits=calc_digits(m$x,digits), vert.lines = 0, align="r", table.placement="H", colsplit=11)
    }
    if(any(grepl(".historical.",getNames(m$hist),fixed=TRUE))) {
      m$hist <- m$hist[,,"historical"]
      for(i in 1:ndata(m$hist)) {
        mhist2 <- m$hist[,,i]
        #remove NAs
        mhist2 <- mhist2[,dimSums(!is.na(mhist2),dim=c(1,3))!=0,]
        if(nyears(mhist2)>0) {
          swlatex(sw,"\\hspace{10 mm}")
          dimnames(mhist2)[[2]] <- sub("y","",dimnames(mhist2)[[2]])
          swtable(sw,mhist2, preptitle(getNames(m$hist)[i]), digits=calc_digits(mhist2,digits), vert.lines = 0, align="r", table.placement="H", colsplit=11)
        }
      }
    }
  }
  
  styleTrafficlight <- function(sw, x, hist, stats) {
    m <- prepmagpie(x,hist)
    p <- trafficlight(m$x,m$hist, detailed=FALSE)
    stats$trafficlight <- rbind(stats$trafficlight,melt(attr(p,"data")))
    swfigure(sw,print,p, sw_option="width=10,height=2")
    return(stats)
  }
  
  styleDetailed <- function(sw, x, hist, varname, stats) {
    m <- prepmagpie(x,hist)
    name <- paste0(sub("^.*\\|","",varname["name"]), " (",x$unit[[1]],")")
    if("GLO" %in% intersect(getRegs(x),getRegs(hist))) swfigure(sw,mipLineHistorical,x[x$region=="GLO",],hist[hist$region=="GLO",], ylab=name)
    regs <- setdiff(intersect(getRegs(x),getRegs(hist)),"GLO")
    if(length(regs)>0) swfigure(sw,mipLineHistorical,x[x$region %in% regs,],hist[hist$region %in% regs,], ylab=name, facet.dim = "region", size=12)
    p <- trafficlight(m$x,m$hist)
    stats$trafficlight <- rbind(stats$trafficlight,melt(attr(p,"data")))
    swfigure(sw,print,p, sw_option="width=8,height=11")
    histtables(sw,m,digits)
    return(stats)
  }
  
  styleComparison <- function(sw, x, hist, varname, stats, debug) {
    m <- prepmagpie(x,hist)
    name <- paste0(sub("^.*\\|","",varname["name"]), " (",x$unit[[1]],")")
    if(debug) save(x,hist,file = paste0("mipLineHistorical_",varname["name"],".Rda"))
    if("GLO" %in% intersect(getRegs(x),getRegs(hist))) swfigure(sw,mipLineHistorical,x[x$region=="GLO",],hist[hist$region=="GLO",], ylab=name)
    regs <- setdiff(intersect(getRegs(x),getRegs(hist)),"GLO")
    if(length(regs)>0) swfigure(sw,mipLineHistorical,x[x$region %in% regs,],hist[hist$region %in% regs,], ylab=name, facet.dim = "region", size=12)
    for(s in 1:ndata(m$x)) {
      x <- m$x[,,s]
      xc <- m$hist
      if(debug) save(x,xc,file = paste0("trafficlight_",varname["name"],s,".Rda"))
      p <- trafficlight(m$x[,,s],m$hist, detailed=(nregions(m$x)<2))
      stats$trafficlight <- rbind(stats$trafficlight,melt(attr(p,"data")))
      swfigure(sw,print,p, tex_caption=preptitle(getNames(m$x)[s]), sw_option="width=10,height=2")
    }
    histtables(sw,m,digits)
    return(stats)
  }
  
  hascontent <- function(stats, ...) {
    test <- list(...)
    hascontent <- TRUE
    for(n in names(test)) {
      if(all(test[[n]]$value==0, na.rm=TRUE)) {
        if(is.null(stats$ignored_all0)) stats$ignored_all0 <- list()
        stats$ignored_all0[[n]] <- c(stats$ignored_all0[[n]],levels(droplevels(test[[n]]$fullname))) 
        hascontent <- FALSE  
      }
    }
    stats$hascontent <- hascontent
    return(stats)
  }
  
  
  fillcontent <- function(sw, x, hist, varname, style, stats) {
    stats <- hascontent(stats,x=x,hist=hist)
    if(!stats$hascontent) return(stats)

    x$fullname <- NULL
    hist$fullname <- NULL
    
    if(varname["level4"]!="") {
      cat("      ...",varname["level4"],"\n")
      swlatex(sw,paste0("\\subsubsection{",varname["level4"],"}"))
    }  
    if(style=="trafficlight") {
      stats <- styleTrafficlight(sw, x, hist, stats)
    } else if(style=="comparison") {
      stats <- styleComparison(sw, x, hist, varname, stats, debug) 
    } else if(style=="detailed") {
      stats <- styleDetailed(sw, x, hist, varname, stats) 
    } else {
      stop("Unknown style ",style)
    }           
    return(stats) 
  }
  
  
  template <-  c("\\documentclass[a4paper, portrait ]{article}",
                 "\\setlength{\\parindent}{0in}",
                 "\\usepackage{float}",
                 "\\usepackage[bookmarksopenlevel=section]{hyperref}",
                 "\\hypersetup{bookmarks=true,pdfauthor={PIK Landuse group}}",
                 "\\usepackage{graphicx}",
                 "\\usepackage{rotating}",
                 "\\usepackage[strings]{underscore}",
                 "\\usepackage[margin=2cm]{geometry}",
                 "\\usepackage{fancyhdr}",
                 "\\pagestyle{fancy}",
                 "\\begin{document}",
                 "<<echo=false>>=",
                 "options(width=110)",
                 "@") 
  
  cat("Start preparing data...\n")
  if(is.environment(file)) {
    sw <- file
  } else {
    sw <- swopen(outfile = file, template = template)
    swlatex(sw,c("\\title{MAgPIE Validation}","\\author{HAL 9000}","\\maketitle","\\tableofcontents"))
    on.exit(swclose(sw, clean_output=!debug, engine="knitr"))
  }
  stats <- list()
  if (hideEmptySection)
  {
    vars <- vars[vars$fullname %in% levels(hist$fullname),]
    vars <- droplevels(vars)
  }
  for(l1 in levels(vars$level1)) {
    cat("...",l1,"\n")
    swlatex(sw,"\\clearpage")
    swlatex(sw,paste0("\\part{",prefix,l1,"}"))
    vars1 <- droplevels(vars[vars$level1==l1,])
    stackedbar(sw, vars1, x, level=2, debug=debug)
    for(l2 in levels(vars1$level2)) {
      if(l2!="") {
        cat("  ...",l2,"\n")
        swlatex(sw,paste0("\\section{",l2,"}"))
      }
      vars2 <- droplevels(vars1[vars1$level2==l2,])
      stackedbar(sw, vars2, x, level=3, debug=debug)
      for(l3 in levels(vars2$level3)) {
        vars3 <- droplevels(vars2[vars2$level3==l3,])
        stackedbar(sw, vars3, x, level=4, debug=debug)
        stats <- hascontent(stats,x=x[x$fullname %in% levels(vars3$fullname),],hist=hist[hist$fullname %in% levels(vars3$fullname),])
        if(!stats$hascontent) next
        if(l3!="") {
          cat("    ...",l3,"\n")
          swlatex(sw,paste0("\\subsection{",l3,"}"))
        }
        for(l4 in levels(vars3$level4)) {
          varname <- as.matrix(vars3[vars3$level4==l4,])
          if(dim(varname)[1]>1) warnings("There seems to be a problem with the variable names. Use only first option!")
          varname <- varname[1,]
          stats <- fillcontent(sw,x[x$fullname==varname["fullname"],],hist[hist$fullname==varname["fullname"],],varname, style, stats)
        } 
      }
    }
  }
  if(show_stats & (!is.null(stats$trafficlight) | length(xtrax)>0 | length(xtrahist)>0 | !is.null(stats$ignored_all0)) ) {
    swlatex(sw,"\\clearpage")
    swlatex(sw,paste0("\\part{",prefix,"Statistics}"))  
    if(!is.null(stats$trafficlight)) {
      swlatex(sw,"\\section{Traffic Lights}")
      
      tl      <- stats$trafficlight
      
      tlsummary <- function(tl) {
        tl[is.na(tl)] <- -1
        summary <- data.frame(green = sum(tl$value==2),
                              yellow = sum(tl$value==1),
                              red = sum(tl$value==0),
                              "NA" = sum(tl$value==-1))
        summary <- rbind(summary,round(summary/sum(summary),2))
        rownames(summary) <- c("total","relative")
        return(summary)  
      }
      
      stats$summary <- list()
      for(i in grep(".",levels(tl$Var2),fixed=TRUE,invert=TRUE,value=TRUE)) {
        stats$summary[[i]] <- list(GLO=tlsummary(tl[tl$Var2==i & tl$Var1=="GLO",]),
                                   REG=tlsummary(tl[tl$Var2==i & tl$Var1!="GLO",]))
        swlatex(sw,paste0("\\subsection{",i,"}"))
        tmp <-  stats$summary[[i]][["GLO"]]
        if(any(tmp[1,]>0, na.rm = TRUE)) {
          tmp[2,] <- paste0(tmp[2,]*100,"%")
          swtable(sw,tmp, caption="Global",table.placement="H")
        }
        tmp <-  stats$summary[[i]][["REG"]]
        if(any(tmp[1,]>0, na.rm = TRUE)) {
          tmp[2,] <- paste0(tmp[2,]*100,"%")
          swtable(sw,tmp, caption="Regional",table.placement="H")
        }
      }
    }
    if(!is.null(stats$ignored_all0)) {
      swlatex(sw,"\\section{Ignored data}")
      if(!is.null(stats$ignored_all0$x)) {
        swlatex(sw,"Data contains only a mix of 0 and NA values and is ignored.")
        swR(sw,cat,stats$ignored_all0$x,sep="\n")
      }
      if(!is.null(stats$ignored_all0$hist)) {
        swlatex(sw,"Validation data contains only a mix of 0 and NA values and is ignored.")
        swR(sw,cat,stats$ignored_all0$hist,sep="\n")
      }
    }
    if(length(xtrax)>0 | length(xtrahist)>0) {
      swlatex(sw,"\\section{Non-Matching Data}")
      if(length(xtrax)>0) {
        swlatex(sw,"\\subsection{Model outputs}")
        swR(sw,cat,xtrax,sep="\n")
      }
      if(length(xtrahist)>0) {
        swlatex(sw,"\\subsection{Validation data}")
        swR(sw,cat,xtrahist,sep="\n")
      }
    }
  }
  
  cat("Finished preparing data!\n")
  return(stats)
}

stackedbar <- function(sw, vars, x, level, debug=FALSE) {
  # analyze wether a stacked bar plot should be plotted and prepare data
  tmp <- function(sw, vars, x, level, symbol, debug=FALSE) {
    vars <- droplevels(vars[vars[[paste0("prop",level)]]==symbol,])
    if(dim(vars)[1]==0) return(FALSE)
    x    <-    as.quitte(droplevels(x[x$fullname %in% levels(vars$fullname),]))
    if(all(x==0,na.rm = TRUE)) return(FALSE)
    if(debug) {
      name <- attr(shorten_legend(x$variable, identical_only = TRUE), "front")
      save(x,file = paste0("mipArea_",make.names(name),".Rda"))
    }
    if("GLO" %in% levels(x$region)) swfigure(sw,mipArea,x[x$region=="GLO",])
    if(length(setdiff(levels(x$region),"GLO"))>0) swfigure(sw,mipArea,x[x$region!="GLO",])
    return(TRUE)
  }
  symbol <- "+"
  while(tmp(sw,vars,x,level,symbol,debug)) {
    symbol <- paste0(symbol,"+")
  }
}
