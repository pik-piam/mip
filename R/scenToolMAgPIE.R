#' @title scenToolMAgPIE
#' @description scenToolMAgPIE allows to explore and visualize time series of modelling results. The app is based on shiny opens in an external web brower. For more details: https://github.com/flohump/scenTool
#' 
#' @param file report data. Can be a CSV/MIF file or rda/RData file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool
#' @param valfile validation data. Can be a CSV/MIF file or rda/RData file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool
#' @author Florian Humpenoeder
#' @examples
#'   \dontrun{
#'     scenToolMAgPIE("testdata.mif")
#'   }
#' 
#' @importFrom quitte as.quitte read.quitte
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
#' @importFrom tools file_ext
#' @importFrom trafficlight trafficlight
#' @export
#'
scenToolMAgPIE <- function(file=NULL,valfile=NULL) {
  model <- scenario <- region <- year <- period <- variable <- unit <- NULL
  
  #limit for file upload set to 300 MB
  options(shiny.maxRequestSize = 300*1024^2)

  #server function
  server <- function(input,output,session,extFile=file) {

    #initialize reactive value
    val <- reactiveValues(rep_full=NULL,rep_sel=NULL,val_full=NULL,val_sel=NULL)
    
    if(is.null(file)) {
      #create dummy data for testing the tool
      model <- factor(c("Model1","Model2","Model3"))
      scenario <- factor(c("Scen1","Scen2","Scen3"))
      region <- factor(c("Region1","Region2","Region3"))
      period <- c(2005,2050,2100)
      variable <- factor(c("Variable1","Variable2","Variable3"))
      unit <- factor(c("Unit"))
      long <- expand.grid(model,scenario,region,variable,unit,period,1,KEEP.OUT.ATTRS = FALSE,stringsAsFactors = TRUE)
      names(long) <- c("model","scenario","region","variable","unit","period","value")
      long$value <- 1:length(long$value)
      val$rep_full <- as.quitte(long)
    } else if (file_ext(file) %in% c("RData","rda")) {
      val$rep_full <- readRDS(file)
    } else val$rep_full <- read.quitte(file)
    if(!is.null(valfile)) {
      if (file_ext(valfile) %in% c("RData","rda")) {
        val$val_full <- readRDS(valfile)
      } else val$val_full <- read.quitte(valfile)
    }
    
    
    #Upload and read in data file if there is a change in input$datafile
    observeEvent(input$datafile, {
      print("read data")
      #assing to reactive value
      if (file_ext(input$datafile$datapath) %in% c("RData","rda")) {
        val$rep_full <- readRDS(input$datafile$datapath)
      } else val$rep_full <- read.quitte(input$datafile$datapath)
    })
    
    #Upload and read in data file if there is a change in input$valfile
    observeEvent(input$valfile, {
      print("read val data")
      #assing to reactive value
      if (file_ext(input$valfile$datapath) %in% c("RData","rda")) {
        val$val_full <- readRDS(input$valfile$datapath)
      } else val$val_full <- read.quitte(input$valfile$datapath)
      #setnames(val$val_full,"Model","Validation Source")
      #print(str(val$val_full))
    })
    
    #subsetting the data stepwise is faster than all at once
    observeEvent(c(input$model,input$scenario,input$region,input$year,input$variable,input$normalize,input$valfile),{
      print("subset data")
      val$rep_sel <- subset(val$rep_full,model %in% input$model)
      val$rep_sel <- subset(val$rep_sel,scenario %in% input$scenario)
      val$rep_sel <- subset(val$rep_sel,region %in% input$region)
      val$rep_sel <- subset(val$rep_sel,period %in% input$year)
      val$rep_sel <- subset(val$rep_sel,variable %in% input$variable)
      val$rep_sel <- droplevels(val$rep_sel)
      
      if(!is.null(val$val_full)) {
        print("validation data")
        val$val_sel <- subset(val$val_full,region %in% input$region)
        val$val_sel <- subset(val$val_sel,variable %in% input$variable)
        val$val_sel <- droplevels(val$val_sel)
        if(nrow(val$val_sel) == 0) val$val_sel <- NULL
      } else val$val_sel <- NULL
      
      if(input$normalize) {
        print("normalize data")
        years <- unique(val$rep_sel$period)
        base_year <- val$rep_sel$value[val$rep_sel$period==years[1]]
        val$rep_sel$value <- val$rep_sel$value/rep(base_year,length(years))
        if(!is.null(val$val_sel)) {
          val$val_sel$value <- val$val_sel$value/rep(base_year,length(unique(val$val_sel$period)))
        }
      }
      
    })

    #subsetting the data stepwise is faster than all at once
    observeEvent(c(input$valmodel,input$valscenario,input$valyear),{
      print("subset data")
      val$val_sel <- subset(val$val_sel,model %in% input$valmodel)
      val$val_sel <- subset(val$val_sel,scenario %in% input$valscenario)
      #val$val_sel <- subset(val$val_sel,Region %in% input$valregion)
      val$val_sel <- subset(val$val_sel,period %in% input$valyear)
      #val$val_sel <- subset(val$val_sel,Variable %in% input$valvariable)
    })
    
    observe({
      print("update choices data")
      updateSelectInput(session, "model", choices = levels(val$rep_full$model),selected = levels(val$rep_full$model)[1])
      updateSelectInput(session, "scenario", choices = levels(val$rep_full$scenario),selected = if (length(levels(val$rep_full$scenario)) > 2) levels(val$rep_full$scenario)[1:2] else levels(val$rep_full$scenario))
      updateSelectInput(session, "region", choices = levels(val$rep_full$region),selected = levels(val$rep_full$region))
      updateSelectInput(session, "year", choices = unique(val$rep_full$period),selected = unique(val$rep_full$period))
      updateSelectInput(session, "variable", choices = levels(val$rep_full$variable),selected = levels(val$rep_full$variable)[1])
      
    })

    observe({
      print("update choices validation")
      updateSelectInput(session, "valmodel", choices = levels(val$val_sel$model),selected = levels(val$val_sel$model))
      updateSelectInput(session, "valscenario", choices = levels(val$val_sel$scenario),selected = levels(val$val_sel$scenario))
#      updateSelectInput(session, "valregion", choices = levels(val$valtmp$Region),selected = levels(val$valtmp$Region))
      updateSelectInput(session, "valyear", choices = unique(val$val_sel$period),selected = unique(val$val_sel$period))
#      updateSelectInput(session, "valvariable", choices = levels(val$valtmp$Variable),selected = levels(val$valtmp$Variable)[1])
      })
    
    
    tf <- reactive({
#      print(str(as.magpie(val$rep_sel)))
      if(is.null(val$val_sel)) stop("Validation file needed for trafficlights!")
      else trafficlight(x=as.magpie(val$rep_sel,spatial="region",temporal="period",tidy=TRUE),xc=as.magpie(val$val_sel,spatial="region",temporal="period",tidy=TRUE),detailed=FALSE)
    })
    
    
    lineplot <- reactive({
      p <- mipLineHistorical(x=val$rep_sel,x_hist=if(input$show_val) val$val_sel else NULL,size = 10,ylab = val$rep_sel$unit,title = val$rep_sel$variable,scales = input$scales)
      return(p)
    })
    
    areaplot <- reactive({
      p <- mipArea(x=if(input$exclude_world) val$rep_sel[val$rep_sel$region!="World",] else val$rep_sel)
      return(p)
    })
    
    output$lineplot <- renderPlot({
      lineplot()},res = 120)#height = 400, width = 500
    
    output$areaplot <- renderPlot({
      areaplot()},res = 120)#height = 400, width = 500
    
    output$tf <- renderPlot({
      tf()},res = 120)#height = 400, width = 500
    
    output$summary <- renderPrint({
      summary(val$rep_sel$value)
    })
    output$info <- renderPrint({
      cat(paste(length(levels(val$rep_full$model)),"Model(s)"),
          paste(length(levels(val$rep_full$scenario)),"Scenario(s)"),
          paste(length(levels(val$rep_full$region)),"Region(s)"),
          paste(length(unique(val$rep_full$period)),"Year(s)"),
          paste(length(levels(val$rep_full$variable)),"Variable(s)"),sep="\n")
    })
    output$data <- renderDataTable({
      val$rep_sel
    }, options = list(pageLength = 10))
    output$downloadLinePlot <- downloadHandler(
      filename = function() { paste("export", '.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = lineplot(), device = "pdf",scale=1,width=20,height=18,units="cm",dpi=150)
      }
    )
    output$downloadAreaPlot <- downloadHandler(
      filename = function() { paste("export", '.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = areaplot(), device = "pdf",scale=1,width=20,height=13,units="cm",dpi=150)
      }
    )
    output$downloadData <- downloadHandler(
      filename = function() { paste("export", '.csv', sep='') },
      content = function(file) {
        out <- val$rep_sel
        out <- dcast(out, model + scenario + region + variable + unit ~ period, value.var="value")
        write.csv(out, file ,row.names = FALSE,quote = FALSE)
      }
    )
    
  }
  
  #client-sided function
  ui <- fluidPage(
                        sidebarLayout(
                          
                          sidebarPanel(
                            tabsetPanel(id="side",type = "tabs",
                            tabPanel("Report Data",
                            fileInput('datafile', 'Upload Report File', accept=c('.mif','.csv','.rda','.RData')),
                            tags$hr(),
                            selectInput('model', 'Model', "Pending upload",multiple = TRUE),
                            selectInput('scenario', 'Scenario', "Pending upload",multiple = TRUE),
                            selectInput('region', 'Region', "Pending upload",multiple = TRUE),
                            selectInput('year', 'Year', "Pending upload",multiple = TRUE),
                            #sliderInput("year", "Year",min=2000,max=2100,value=c(2000,2100),step=10),
                            selectInput('variable', 'Variable', "Pending upload",multiple = FALSE)
                            ),
                            tabPanel("Validation Data",
                                     fileInput('valfile', 'Upload Validation File', accept=c('.mif','.csv','.rda','.RData')),
                                     tags$hr(),
                                     #checkboxInput('auto_val_sel', 'Automatic matching with Model Data', value = TRUE, width = NULL),
                                     selectInput('valmodel', 'Model', "Pending upload",multiple = TRUE),
                                     selectInput('valscenario', 'Scenario', "Pending upload",multiple = TRUE),
                                     #selectInput('valregion', 'Region', "Pending upload",multiple = TRUE),
                                     selectInput('valyear', 'Year', "Pending upload",multiple = TRUE)
                                     #sliderInput("year", "Year",min=2000,max=2100,value=c(2000,2100),step=10),
                                     #selectInput('valvariable', 'Variable', "Pending upload",multiple = FALSE)
                            )
                            
                            )
                            
                            ,width=3),
                          mainPanel(
                            tabsetPanel(id = "main",type = "tabs",
                                        tabPanel("LinePlot",
                                                plotOutput("lineplot",height = "800px",width = "auto"),
                                                 wellPanel(
                                                   fluidRow(
                                                     column(2,
                                                            selectInput('scales', 'Scales',c("fixed","free_y","free_x","free"),selected="fixed"),
                                                            checkboxInput('normalize', 'Normalize', value = FALSE, width = NULL),
                                                            conditionalPanel(condition = "input.valfile != NULL", checkboxInput('show_val', 'Show Validation', value = TRUE, width = NULL))
                                                     )
                                                   )
                                                 ),
                                                wellPanel(downloadButton('downloadLinePlot', 'Download Plot'))
                                        ),
                                        tabPanel("AreaPlot",
                                                 plotOutput("areaplot",height = "800px",width = "auto"),
                                                 wellPanel(
                                                   fluidRow(
                                                     column(4,
                                                            checkboxInput('exclude_world', 'Exclude Region "World"', value = TRUE, width = NULL)
                                                     )
                                                   )
                                                 ),
                                                 wellPanel(downloadButton('downloadAreaPlot', 'Download Plot'))
                                        ),
                                        tabPanel("Table", 
                                                 dataTableOutput("data"),
                                                 wellPanel(downloadButton('downloadData', 'Download Data'))
                                        ),
                                        tabPanel("Trafficlight",
                                                 plotOutput("tf",height = "200px",width = "auto")
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
