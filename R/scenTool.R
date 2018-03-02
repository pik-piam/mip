#' @title scenTool
#' @description scenTool allows to explore and visualize time series of modelling results. The app is based on shiny opens in an external web brower. For more details: https://github.com/flohump/scenTool
#' 
#' @param file model data in CSV file in MIF format (NULL by default; in this case the user can upload files directly in the tool)
#' @param valfile validation data in CSV file in MIF format (NULL by default; in this case the user can upload files directly in the tool)
#' @author Florian Humpenoeder
#' @examples
#'   \dontrun{
#'     scenTool("testdata.mif")
#'   }
#' 
#' @importFrom ggplot2 ggsave theme_minimal aes_ aes_string ylab scale_color_manual scale_x_continuous xlim stat_summary
#' @importFrom reshape2 melt dcast
#' @importFrom shiny reactiveValues observeEvent updateTextInput observe updateSelectInput reactive hoverOpts uiOutput sliderInput
#' renderPrint renderDataTable downloadHandler fluidPage navbarPage tabPanel sidebarLayout sidebarPanel
#' fileInput tags selectInput mainPanel tabsetPanel wellPanel fluidRow column radioButtons conditionalPanel
#' checkboxInput checkboxGroupInput numericInput textInput downloadButton dataTableOutput h2 verbatimTextOutput
#' shinyApp renderPlot plotOutput renderUI HTML nearPoints updateCheckboxInput updateSliderInput
#' @importFrom plotly as.widget layout plotly plotlyOutput renderPlotly ggplotly
#' @importFrom stats as.formula complete.cases na.omit
#' @importFrom utils write.csv
#' @importFrom data.table fread setcolorder as.data.table data.table setnames
#' @importFrom stats median reshape
#' @export
#'
scenTool <- function(file=NULL,valfile=NULL) {
  Model <- Scenario <- Region <- Year <- Variable <- NULL
  
  #limit for file upload set to 300 MB
  options(shiny.maxRequestSize = 300*1024^2)

  #server function
  server <- function(input,output,session,extFile=file) {
    #function for reading data files
    read.mif <- function(inFile) {
      if (is.null(inFile)) {
        return(NULL)
      } else {
        s <- fread(inFile,sep=";",header=FALSE,nrows=1)

        if (all(names(s) == "V1")) sep <- "," else sep <- ";"
        
        #fread is much faster than read.table
        wide <- fread(inFile,sep=sep,header=TRUE,stringsAsFactors = TRUE,na.strings = "N/A",check.names = FALSE)

        #clean the data table
        del <- which(names(wide) == "")
        if (length(del) > 0) wide <- wide[,-del]
        #convert data from wide to long format
        long <- melt(wide, id.vars=c(1:5),variable.name = "Year",value.name = "Value",na.rm = FALSE)
        
        #reorder columns
        setcolorder(long,c(1,2,3,6,4,5,7))

        #set column names
        names(long) <- c("Model","Scenario","Region","Year","Variable","Unit","Value")
        #merge variable and unit column
        long$Variable <- with(long,paste0(Variable," (",Unit,")"))
        long$Variable <- as.factor(long$Variable)
        long$Unit <- NULL
        #order
        long$Model <- factor(long$Model,levels=unique(long$Model))
        long$Scenario <- factor(long$Scenario,levels=unique(long$Scenario))
        long$Region <- factor(long$Region,levels=unique(long$Region))
        long$Variable <- factor(long$Variable,levels=unique(long$Variable))
        #remove NAs
        long <- na.omit(long)
        #convert years to numeric
        long$Year <- as.numeric(as.character(long$Year))
        #keep only complete cases
        long <- long[complete.cases(long),]
        return(long)
      }
    }
    
    #initialize reactive value
    val <- reactiveValues(a = NULL,sel=NULL,title=NULL,ylab=NULL)
    
    if(is.null(file)) {
      #create dummy data for testing the tool
      model <- factor(c("Model1","Model2","Model3"))
      scenario <- factor(c("Scen1","Scen2","Scen3"))
      region <- factor(c("Region1","Region2","Region3"))
      year <- c(2005,2050,2100)
      variable <- factor(c("Variable1 (Unit1)","Variable2 (Unit2)","Variable3 (Unit3)"))
      long <- expand.grid(model,scenario,region,year,variable,1,KEEP.OUT.ATTRS = FALSE,stringsAsFactors = TRUE)
      names(long) <- c("Model","Scenario","Region","Year","Variable","Value")
      long$Value <- 1:length(long$Value)
      val$a <- as.data.table(long)
    } else val$a <- read.mif(file)
    if(!is.null(valfile)) val$b <- read.mif(valfile)
    
    
    
    #Upload and read in data file if there is a change in input$datafile
    observeEvent(input$datafile, {
      print("read data")
      #assing to reactive value
      val$a <- read.mif(input$datafile$datapath)
      
    })
    
    #Upload and read in data file if there is a change in input$datafile
    observeEvent(input$valfile, {
      print("read val data")
      #assing to reactive value
      val$b <- read.mif(input$valfile$datapath)
      setnames(val$b,"Model","Validation Source")
      #print(str(val$b))
    })
    
    #subsetting the data stepwise is faster than all at once
    observeEvent(c(input$model,input$scenario,input$region,input$year,input$variable,input$normalize,input$auto_val_sel),{
      print("subset data")
      val$sel <- subset(val$a,Model %in% input$model)
      val$sel <- subset(val$sel,Scenario %in% input$scenario)
      val$sel <- subset(val$sel,Region %in% input$region)
      val$sel <- subset(val$sel,Year %in% input$year)
      val$sel <- subset(val$sel,Variable %in% input$variable)
      val$sel <- droplevels(val$sel)
      
      years <- unique(val$sel$Year)
      first_year <- rep(val$sel$Value[val$sel$Year==years[1]],length(years))
      if(input$normalize) {
        print("normalize data")
        val$sel$Value <- val$sel$Value/first_year
      }
      if(input$auto_val_sel){
        print("validation data")
        val$val <- subset(val$b,Region %in% input$region)
        val$val <- subset(val$val,Variable %in% input$variable)
        val$val <- droplevels(val$val)
        #levels(val$val$Variable) <- shorten_legend(levels(val$val$Variable),identical_only = TRUE)
        val$valtmp <- val$val
      } else val$valtmp <- val$b
    })

    #subsetting the data stepwise is faster than all at once
    observeEvent(c(input$valmodel,input$valscenario,input$valregion,input$valyear,input$valvariable),{
      print("subset data")
      val$val <- subset(val$valtmp,Model %in% input$valmodel)
      val$val <- subset(val$val,Scenario %in% input$valscenario)
      val$val <- subset(val$val,Region %in% input$valregion)
      val$val <- subset(val$val,Year %in% input$valyear)
      val$val <- subset(val$val,Variable %in% input$valvariable)
    })
    
    observeEvent(c(input$variable,input$stackshare,input$plottype,input$switchaxis,input$short_legend),{
      if(length(input$variable) == 1) {
        val$title <- strsplit(input$variable[1]," \\(")[[1]][1]
        val$ylab <- strsplit(input$variable[1]," \\(")[[1]][2]
        val$ylab <- substr(val$ylab, 1, nchar(val$ylab)-1)
        if (input$stackshare) val$ylab <- "Share"
      } else if (input$plottype == "scatter") {
        val$title <- ""
        if(input$switchaxis) val$ylab <- input$variable[2] else val$ylab <- input$variable[1]
      } else {
        var_names_old <- NULL
        if(is.null(var_names_old)) var_names_old <- levels(val$sel$Variable)
        if(input$short_legend) {
          var_names <- shorten_legend(levels(val$sel$Variable),identical_only = TRUE)
          levels(val$sel$Variable) <- var_names
          val$title <- gsub("\\|\\+\\|","",attributes(var_names)$front)
          if(length(val$val$title) == 0) {
            #print(val$var_unit)
            # val$var_unit <- strsplit(val$var_unit," \\(")[[1]][2]
            # val$var_unit <- substr(val$var_unit, 1, nchar(val$var_unit)-1)
            val$ylab <- attributes(var_names)$back
          } else val$ylab <- "Unit"
        } else {
          levels(val$sel$Variable) <- var_names_old
          val$title <- "Variable(s)"
          val$ylab <- "Unit"
        }
      }
      updateTextInput(session, "plottitle", value = val$title)
      updateTextInput(session, "ylab", value = val$ylab)
    })
    
    observeEvent(c(input$plottitle,input$ylab),{
      val$title <- input$plottitle
      val$ylab <- input$ylab
    })
    
    observe({
      print("update choices data")
      updateSelectInput(session, "model", choices = levels(val$a$Model),selected = levels(val$a$Model)[1])
      updateSelectInput(session, "scenario", choices = levels(val$a$Scenario),selected = if (length(levels(val$a$Scenario)) > 2) levels(val$a$Scenario)[1:2] else levels(val$a$Scenario))
      updateSelectInput(session, "region", choices = levels(val$a$Region),selected = if ("World" %in% levels(val$a$Region)) "World" else if ("WLD" %in% levels(val$a$Region)) "WLD" else levels(val$a$Region)[1])
      updateSelectInput(session, "year", choices = unique(val$a$Year),selected = unique(val$a$Year))
      #updateSliderInput(session, "year", min = min(unique(val$a$Year)), max = max(unique(val$a$Year)), value=range(unique(val$a$Year)))#step=(max(unique(val$a$Year))-min(unique(val$a$Year)))/length(unique(val$a$Year))
      updateSelectInput(session, "variable", choices = levels(val$a$Variable),selected = levels(val$a$Variable)[1])
      
      years_a <- unique(val$a$Year)
      if(!is.null(val$b)) {
        years_b <- unique(val$b$Year)
        years <- sort(unique(c(years_a,years_b)))
      } else years <- years_a
      updateSliderInput(session, "xlim", min = min(years), max = max(years), value=range(years_a))#step=(max(unique(val$a$Year))-min(unique(val$a$Year)))/length(unique(val$a$Year))
    })

    observe({
      print("update choices validation")
      updateSelectInput(session, "valmodel", choices = levels(val$valtmp$Model),selected = levels(val$valtmp$Model))
      updateSelectInput(session, "valscenario", choices = levels(val$valtmp$Scenario),selected = levels(val$valtmp$Scenario))
      updateSelectInput(session, "valregion", choices = levels(val$valtmp$Region),selected = if ("World" %in% levels(val$valtmp$Region)) "World" else if ("WLD" %in% levels(val$valtmp$Region)) "WLD" else levels(val$valtmp$Region)[1])
      updateSelectInput(session, "valyear", choices = unique(val$valtmp$Year),selected = unique(val$valtmp$Year))
      updateSelectInput(session, "valvariable", choices = levels(val$valtmp$Variable),selected = levels(val$valtmp$Variable)[1])
      })
    
    
    # observe({
    #   print("update presets")
    #   if(input$preset == "None") {
    #     updateSelectInput(session, "variable",selected = levels(val$a$Variable)[1])
    #     updateSelectInput(session, "plottype",selected = "line")
    #   } else if(input$preset=="LandCover") {
    #     updateSelectInput(session, "variable",selected = c("Resources|Land Cover|+|Cropland (million ha)","Resources|Land Cover|+|Pastures and Rangelands (million ha)","Resources|Land Cover|+|Forest (million ha)","Resources|Land Cover|+|Other Land (million ha)","Resources|Land Cover|+|Urban Area (million ha)"))
    #     updateSelectInput(session, "plottype",selected = "area")
    #   } else if(input$preset=="LandCoverChange") {
    #     updateSelectInput(session, "variable",selected = c("Resources|Land Cover Change|+|Cropland (million ha wrt 1995)","Resources|Land Cover Change|+|Pastures and Rangelands (million ha wrt 1995)","Resources|Land Cover Change|+|Forest (million ha wrt 1995)","Resources|Land Cover Change|+|Other Land (million ha wrt 1995)"))
    #     updateSelectInput(session, "plottype",selected = "line")
    #   } else if(input$preset=="EmissionsCO2 annual") {
    #     updateSelectInput(session, "variable",selected = "Emissions|CO2|Land (Mt CO2/yr)")
    #     updateSelectInput(session, "plottype",selected = "line")
    #   } else if(input$preset=="EmissionsCO2 cumulative") {
    #     updateSelectInput(session, "variable",selected = "Emissions|CO2|Land|Cumulative|Land-use Change (Gt CO2)")
    #     updateSelectInput(session, "plottype",selected = "line")
    #   }
    # })
        
    observe({
      print("update plot settings")
      if(input$plottype=="area") {
        updateCheckboxInput(session, "stack",value = TRUE)
        updateCheckboxInput(session, "stackshare",value = FALSE)
        updateSelectInput(session, "fill",selected = "Variable")
        updateSelectInput(session, "facet_x",selected = "Scenario")
        updateSelectInput(session, "facet_y",selected = "NULL")
      } else if(input$plottype=="line") {
        updateCheckboxInput(session, "stack",value = FALSE)
        updateCheckboxInput(session, "stackshare",value = FALSE)
        updateSelectInput(session, "color",selected = "Scenario")
        #updateSelectInput(session, "linetype",selected = NULL)
        updateSelectInput(session, "shape",selected = "Model")
        updateSelectInput(session, "facet_x",selected = NULL)
        updateSelectInput(session, "facet_y",selected = NULL)
      } else if(input$plottype=="scatter") {
        updateCheckboxInput(session, "stack",value = FALSE)
        updateCheckboxInput(session, "stackshare",value = FALSE)
        updateSelectInput(session, "color_scatter",selected = "Scenario")
        #updateSelectInput(session, "linetype_scatter",selected = "Model")
        updateSelectInput(session, "shape_scatter",selected = "Model")
        updateSelectInput(session, "facet_x",selected = "NULL")
        updateSelectInput(session, "facet_y",selected = "NULL")
      } else if(input$plottype=="bar") {
        updateCheckboxInput(session, "stack",value = TRUE)
        updateCheckboxInput(session, "stackshare",value = FALSE)
        updateSelectInput(session, "fill",selected = "Variable")
        updateSelectInput(session, "facet_x",selected = "Scenario")
        updateSelectInput(session, "facet_y",selected = "NULL")
      }
    })
    
    
    plot <- reactive({
      myBreaks <- function(x){
        if(length(unique(x)) <= 3) {
          breaks <- unique(x)
        } else {
          breaks <- c(min(x),round(mean(range(x)),digits = -1),max(x))
        }
        names(breaks) <- attr(breaks,"labels")
        breaks
      }
      
      ggname <- function(x) {
        if (class(x) != "character") {
          return(x)
        }
        y <- sapply(x, function(s) {
          if (!grepl("^`", s)) {
            s <- paste("`", s, sep="", collapse="")
          }
          if (!grepl("`$", s)) {
            s <- paste(s, "`", sep="", collapse="")
          }
        }
        )
        y 
      }
      
      color <- input$color
      fill <- input$fill
      if(input$plottype == 'line') fill <- NULL
      if(input$plottype == 'bar' || input$plottype == 'area') color <- NULL
      #      if(input$normalize) norm <- input$normalizeYear else norm <- NULL
      sel <- val$sel
      if(input$plottype == 'scatter') {
        sel <- reshape(sel, timevar = "Variable", idvar = names(sel)[!(names(sel) %in% c("Value", "Variable"))], direction = "wide")
        names(sel) <- gsub("Value.", "", names(sel))
        if(input$switchaxis) {
          x_var <- input$variable[1]
          y_var <- input$variable[2]
        } else{
          x_var <- input$variable[2]
          y_var <- input$variable[1]
        }
        sel$Year <- as.factor(sel$Year)
        p <- ggplot(data=sel, aes_string(x=ggname(x_var), y=ggname(y_var))) + theme_minimal()
        val$sel_scatter <- sel
        #p <- ggplot(data=sel, aes(x=x_var, y=y_var)) + theme_minimal()
        p <- p + geom_point(aes_string(color=input$color_scatter,shape=input$shape_scatter))
        if(input$showline) p <- p + geom_line(aes_string(color=input$color_scatter))
      } else if(input$plottype == "line") {
        Value <- NULL
        p <- ggplot(data=sel, aes(x=Year, y=Value)) + theme_minimal()
        if(input$show_val) {
          #p <- p + geom_line(data = val$val ,mapping = aes_string(linetype="Model"))
          #p <- p + stat_summary(data = val$val, geom = "ribbon", mapping = aes_string(fill="Model",linetype="Model"), fun.ymin="min", fun.ymax="max", alpha=0.3, show.legend = FALSE)
          p <- p + stat_summary(data = val$val, geom = "ribbon", mapping = aes_string(linetype="Model"), fun.ymin="min", fun.ymax="max", alpha=0.3, show.legend = FALSE)
          p <- p + stat_summary(data = val$val, geom="line", mapping = aes_string(linetype="Model"), fun.y = median)
        }
        if(input$summarize == "None") {
          p <- p + geom_line(aes_string(color=color, shape=input$shape)) + geom_point(aes_string(color=color,shape=input$shape))#linetype=input$linetype
        } else if(input$summarize == "Detail") {
          p <- p + geom_line(aes_string(color=color, shape=input$shape)) + geom_point(aes_string(color=color,shape=input$shape))#linetype=input$linetype
          p <- p + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", aes_string(fill=color), alpha=0.3, show.legend = FALSE)
          #p <- p + stat_summary(geom="line", fun.y = median, aes_string(color=color))
        } else if(input$summarize == "Simple") {
          p <- p + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", aes_string(fill=color), alpha=0.3, show.legend = if(input$stat_dim == "NULL") TRUE else FALSE)
          #p <- p + stat_summary(geom="line", fun.y = median, aes_string(color=color))
        }
        if(input$stat_type != "NULL") p <- p + stat_summary(geom="line", fun.y = input$stat_type, aes_string(color=input$stat_dim))
        
        #p <- p + geom_line(aes_string(color=color,group="Model"))# + geom_point(aes_string(color=color,shape=input$shape))
        
#        p <- p + geom_line(aes_string(color=color,group=paste0("interaction(", paste0(c(color,input$linetype),collapse =  ", "), ")")))# + geom_point(aes_string(color=color,shape=input$shape))
        p <- p + scale_color_manual(values=as.character(plotstyle(as.character(unique(sel[[color]])),out="color")))
        p <- p + scale_x_continuous(breaks = myBreaks(sel$Year))
        p <- p + xlim(input$xlim)
      } else if(input$plottype == "bar") {
        p <- ggplot(data=sel, aes(x=Year, y=Value)) + theme_minimal()
        data_pos <- sel
        data_neg <- sel
        data_pos$Value[data_pos$Value<0] <- 0
        data_neg$Value[data_neg$Value>=0] <- 0
        if (input$stack) {
          if (input$stackshare) {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=data_pos,position='fill',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_bar(data=data_neg,position='fill',stat='identity',aes_string(fill=fill))
          } else {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=data_pos,position='stack',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_bar(data=data_neg,position='stack',stat='identity',aes_string(fill=fill))
          }
        } else p <- p + geom_bar(data=sel,position='dodge',stat='identity',aes_string(fill=fill))
        if(input$stat_type != "NULL") p <- p + stat_summary(geom="point", fun.y = input$stat_type, aes_string(color=input$stat_dim))
      } else if (input$plottype == "area") {
        p <- ggplot(data=sel, aes(x=Year, y=Value)) + theme_minimal()
        data_pos <- sel
        data_neg <- sel
        data_pos$Value[data_pos$Value<0] <- 0
        data_neg$Value[data_neg$Value>=0] <- 0
        if (input$stack) {
          if (input$stackshare) {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=data_pos,position='fill',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_area(data=data_neg,position='fill',stat='identity',aes_string(fill=fill))
          } else {
            if (any(data_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=data_pos,position='stack',stat='identity',aes_string(fill=fill))
            if (any(data_neg$Value < 0,na.rm=TRUE)) p <- p + geom_area(data=data_neg,position='stack',stat='identity',aes_string(fill=fill))
          }
          
        } else p <- p + geom_area(data=sel,position='dodge',stat='identity',aes_string(fill=fill))
        if(input$stat_type != "NULL") p <- p + stat_summary(geom="line", fun.y = input$stat_type, aes_string(color=input$stat_dim))
        p <- p + scale_x_continuous(breaks = myBreaks(sel$Year))
      }
      
      if (!is.null(input$facet_y)) {
        p <- p + facet_grid(as.formula(paste(paste(input$facet_y,collapse = '+'), "~",if(is.null(input$facet_x)) "." else paste(input$facet_x,collapse = '+'))),scales=if(input$free_scale) {"free"} else {"fixed"})
      } else if (!is.null(input$facet_x)) p <- p + facet_wrap(as.formula(paste("~", paste(input$facet_x,collapse = '+'))), ncol=input$ncol, scales=if(input$free_scale) {"free"} else {"fixed"})
      
      p <- p + ylab(val$ylab) + ggtitle(val$title)
#      p <- p + theme(axis.text.x = element_text(angle=90, vjust=0.5),legend.position = "bottom",legend.direction = "vertical")
      if(input$hide_legend) {
        p <- p + theme(legend.position="none")
      } else {
        p <- p + theme(legend.position = "bottom",legend.direction = "vertical")
        p <- p + guides(color = guide_legend(ncol=input$legend_ncol,title.position = "top", byrow = TRUE, order = 3)) +
          guides(fill  = guide_legend(ncol=input$legend_ncol,title.position = "top", byrow = TRUE, order = 4)) +
          guides(shape = guide_legend(ncol=input$legend_ncol,title.position = "top", byrow = TRUE, order = 2)) +
          guides(alpha = guide_legend(ncol=input$legend_ncol,title.position = "top", byrow = TRUE, order = 5)) +
          guides(linetype = guide_legend(ncol=input$legend_ncol,title.position = "top", byrow = TRUE,title = "Validation Source", order = 1)) +
          theme(legend.box.just = "left")
      }

      return(p)
    })
    
    output$plotly <- renderPlotly({
      #layout(p = ggplotly(plot(),tooltip="all"),margin = list(b = 50,l = 50))
      p <- ggplotly(plot(),tooltip=c("y",if(input$plottype == "line") {c("colour","shape","linetype")} else if (input$plottype %in% c("bar","area")) {"fill"} else if (input$plottype == "scatter") {c("x","colour","shape")}))
      #print(p[['x']][['sel']])
      if (input$legend_inside) {
        layout(p = p,margin = list(b = 50,l = 70),legend = list(x = 0.1, y = 0.9, orientation = 'v'))  
      } else layout(p = p,margin = list(b = 50,l = 70))
    })
    
    output$plot <- renderPlot({
      plot()},res = 120)#height = 400, width = 500
    
    output$summary <- renderPrint({
      summary(val$sel$Value)
    })
    output$info <- renderPrint({
      cat(paste(length(levels(val$a$Model)),"Model(s)"),
          paste(length(levels(val$a$Scenario)),"Scenario(s)"),
          paste(length(levels(val$a$Region)),"Region(s)"),
          paste(length(unique(val$a$Year)),"Year(s)"),
          paste(length(levels(val$a$Variable)),"Variable(s)"),sep="\n")
    })
    output$data <- renderDataTable({
      val$sel
    }, options = list(pageLength = 10))
    output$downloadPlot <- downloadHandler(
      filename = function() { paste("export", '.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plot(), device = "pdf",scale=1,width=20,height=13,units="cm",dpi=150)
      }
    )
    output$downloadData <- downloadHandler(
      filename = function() { paste("export", '.csv', sep='') },
      content = function(file) {
        out <- val$sel
        out$Unit = as.character(lapply(strsplit(as.character(out$Variable), split=" \\("), "[", 2))
        out$Unit <- as.factor(substr(out$Unit,1,nchar(out$Unit)-1))
        out$Variable = as.factor(as.character(lapply(strsplit(as.character(out$Variable), split=" \\("), "[", 1)))
        #setcolorder(out,c(1,2,3,4,5,7,6))
        out <- out[,c(1,2,3,4,5,7,6)]
        out <- dcast(out, Model + Scenario + Region + Variable + Unit ~ Year, value.var="Value")
        write.csv(out, file ,row.names = FALSE,quote = FALSE)
      }
    )
    
    output$hover_info <- renderUI({
      hover <- input$plot_hover
      if (input$plottype == "scatter") {
        if(input$switchaxis) {
          hover$mapping$x <- input$variable[1]
          hover$mapping$y <- input$variable[2]
        } else{
          hover$mapping$x <- input$variable[2]
          hover$mapping$y <- input$variable[1]
        }
        point <- nearPoints(val$sel_scatter, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      } else point <- nearPoints(val$sel, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)

      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")

      # actual tooltip created as wellPanel
      if(input$plottype == "scatter") {
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Y Value </b>", round(hover$y,2), "<br/>",
                        "<b> X Value </b>", round(hover$x,2), "<br/>",
                        "<b> Region: </b>", point$Region, "<br/>",
                        "<b> Year: </b>", point$Year, "<br/>",
                        "<b> Scenario: </b>", point$Scenario, "<br/>",
                        "<b> Model: </b>", point$Model, "<br/>"
          )))
        )
      } else {
        wellPanel(
          style = style,
          p(HTML(paste0("<b> Value: </b>", round(point$Value,2), "<br/>",
                        "<b> Variable: </b>", point$Variable, "<br/>",
                        "<b> Region: </b>", point$Region, "<br/>",
                        "<b> Year: </b>", point$Year, "<br/>",
                        "<b> Scenario: </b>", point$Scenario, "<br/>",
                        "<b> Model: </b>", point$Model, "<br/>"
                        # "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
          )))
        )
      }
    })
  }
  
  #client-sided function
  ui <- fluidPage(
                        sidebarLayout(
                          
                          sidebarPanel(
                            tabsetPanel(id="side",type = "tabs",
                            tabPanel("Model Data",
                            fileInput('datafile', 'Upload Data File', accept=c('.mif','.csv')),
#                            selectInput('preset', 'Preset',c("None","LandCover","LandCoverChange","EmissionsCO2 annual","EmissionsCO2 cumulative"),multiple = FALSE),
                            tags$hr(),
                            selectInput('model', 'Model', "Pending upload",multiple = TRUE),
                            selectInput('scenario', 'Scenario', "Pending upload",multiple = TRUE),
                            selectInput('region', 'Region', "Pending upload",multiple = TRUE),
                            selectInput('year', 'Year', "Pending upload",multiple = TRUE),
                            #sliderInput("year", "Year",min=2000,max=2100,value=c(2000,2100),step=10),
                            selectInput('variable', 'Variable', "Pending upload",multiple = TRUE)
                            ),
                            tabPanel("Validation Data",
                                     fileInput('valfile', 'Upload Validation File', accept=c('.csv','.mif')),#'text/csv','text/comma-separated-values,text/plain','.csv','.mif'
                                     tags$hr(),
                                     checkboxInput('auto_val_sel', 'Automatic matching with Model Data', value = FALSE, width = NULL),
                                     selectInput('valmodel', 'Model', "Pending upload",multiple = TRUE),
                                     selectInput('valscenario', 'Scenario', "Pending upload",multiple = TRUE),
                                     selectInput('valregion', 'Region', "Pending upload",multiple = TRUE),
                                     selectInput('valyear', 'Year', "Pending upload",multiple = TRUE),
                                     #sliderInput("year", "Year",min=2000,max=2100,value=c(2000,2100),step=10),
                                     selectInput('valvariable', 'Variable', "Pending upload",multiple = TRUE)
                            )
                            
                            )
                            
                            ,width=3),
                          # sidebarPanel(
                          #   selectInput('color', 'Color', c("Model","Scenario","Region","Variable"),multiple = FALSE,selected = "Scenario")
                          # ),
                          mainPanel(
                            tabsetPanel(id = "main",type = "tabs",
                                        tabPanel("Plot",
                                                 conditionalPanel(condition = "input.display == 'plotly'",plotlyOutput("plotly",width="auto",height="auto")),
                                                 conditionalPanel(condition = "input.display == 'ggplot2'",
                                                                  plotOutput("plot",height = "600px",width = "auto",hover = hoverOpts("plot_hover", delay = 50, delayType = "debounce")),
                                                                  uiOutput("hover_info")),
                                                 #uiOutput("hover_info"),
                                                 wellPanel(
                                                   fluidRow(
                                                     column(2,
                                                            selectInput("plottype", "Plot Type", c("line","bar","area","scatter"), selected = "line"),
                                                            conditionalPanel(condition = "input.plottype == 'line'", checkboxInput('normalize', 'Normalize', value = FALSE, width = NULL)),
                                                            conditionalPanel(condition = "input.plottype == 'line'", checkboxInput('show_val', 'Show Validation', value = FALSE, width = NULL)),
                                                            conditionalPanel(condition = "input.plottype == 'bar' || input.plottype == 'area'", checkboxInput('stack', 'Stack', value = FALSE, width = NULL)),
                                                            conditionalPanel(condition = "input.stack == true", checkboxInput('stackshare', 'Share', value = FALSE, width = NULL)),
                                                            conditionalPanel(condition = "input.plottype == 'scatter'", checkboxInput('switchaxis', 'Switch Axis', value = FALSE, width = NULL)),
                                                            conditionalPanel(condition = "input.plottype == 'scatter'", checkboxInput('showline', 'Show Lines', value = FALSE, width = NULL))
                                                     ),
                                                     column(2,
                                                            conditionalPanel(condition = "input.plottype == 'line'",
                                                                             selectInput('color', 'Line Color', c("Model","Scenario","Region","Variable"),selected = "Variable")),
                                                            conditionalPanel(condition = "input.plottype == 'bar' || input.plottype == 'area'",
                                                                             selectInput('fill', 'Fill', c("Model","Scenario","Region","Variable"),selected = "Variable")),
                                                            conditionalPanel(condition = "input.plottype == 'scatter'",
                                                                             selectInput('color_scatter', 'Point Color', c("Model","Scenario","Region","Year"),selected = "Year")),
                                                            #conditionalPanel(condition = "input.plottype == 'line' && input.summarize != 'Simple'", selectInput('linetype', 'Line Type', c("NULL","Model","Scenario","Region","Variable"),selected = NULL)),
                                                            #conditionalPanel(condition = "input.plottype == 'scatter' && input.showline", selectInput('linetype_scatter', 'Line Type', c("NULL","Model","Scenario","Region","Year"),selected = NULL)),
                                                            conditionalPanel(condition = "input.plottype == 'line' && input.summarize != 'Simple'", selectInput('shape', 'Point Shape', c("NULL","Model","Scenario","Region","Variable"),selected = NULL)),# 
                                                            conditionalPanel(condition = "input.plottype == 'scatter'", selectInput('shape_scatter', 'Point Shape', c("NULL","Model","Scenario","Region","Year"),selected = NULL)),
                                                            conditionalPanel(condition = "input.plottype == 'line'", sliderInput("xlim", "X axis",min=1900,max=2100,value=c(2000,2100),step=10))
                                                            
                                                     ),
                                                     # column(2,
                                                     #        checkboxGroupInput('group', 'Group', c("Model","Scenario","Region","Variable"),selected = NULL)
                                                     # ),
                                                     column(2,
                                                            selectInput('facet_x', 'Panel Horizontal', c("Model","Scenario","Region","Variable"),selected = NULL,multiple = TRUE),
                                                            selectInput('facet_y', 'Panel Vertical', c("Model","Scenario","Region","Variable"),selected = NULL,multiple = TRUE),
                                                            numericInput('ncol', 'Panel Columns', value = 2, min = 1, max = 10,step = 1, width = NULL)
                                                     ),
                                                     column(2,
                                                            textInput('plottitle', 'Plot Title', "Variable(s)"),
                                                            checkboxInput('hide_legend', 'Hide Legend'),
                                                            checkboxInput('short_legend', 'Shorten Legend'),
                                                            conditionalPanel(condition = "input.display == 'plotly'", checkboxInput('legend_inside', 'Legend in Plot', value = FALSE, width = NULL)),
                                                            conditionalPanel(condition = "input.display == 'ggplot2'", numericInput('legend_ncol', 'Legend Columns', value = 2, min = 1, max = 5,step = 1, width = NULL))
                                                     ),
                                                     column(2,
                                                            conditionalPanel(condition = "input.plottype == 'line'", selectInput('summarize', 'Summarize', c("None","Detail","Simple"))),
                                                            conditionalPanel(condition = "input.plottype != 'scatter'", selectInput('stat_type', 'Stat Type', c("NULL","sum","mean","median"))),
                                                            conditionalPanel(condition = "input.plottype != 'scatter'", selectInput('stat_dim', 'Stat Dim', c("NULL","Model","Scenario","Region","Variable")))
                                                     ),
                                                     column(2,
                                                            selectInput('display', "Display", c("plotly","ggplot2"), selected = "plotly"),
                                                            textInput('ylab', 'Y Axis Label', "Unit"),
                                                            checkboxInput('free_scale', 'Free Y Scale'),
                                                            downloadButton('downloadPlot', 'Download')
                                                             )


                                                   )
                                                   
                                                 )
                                        ),
                                        tabPanel("Table", 
                                                 dataTableOutput("data"),
                                                 wellPanel(downloadButton('downloadData', 'Download Data'))
                                        ),
                                        tabPanel("Info", 
                                                 h2("Summary"),
                                                 verbatimTextOutput("summary"),
                                                 h2("General information about the dataset"),
                                                 verbatimTextOutput("info")
                                        )
                            )
                          )
                        )
               )
  
  
  
  #start the app
  shinyApp(ui = ui, server = server)
  }  
