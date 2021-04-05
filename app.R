library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)

# sample data
data(mtcars)

ui <- fluidPage(
    theme=shinytheme("darkly"),
    # TITLE
    titlePanel("Warehouse Data"),
    # SIDEBAR (OPTIONS)
    sidebarPanel(
        fileInput("file1", "Upload File",
                  multiple=T,
                  accept=c(".csv", "text/csv", "text/comma-separated-values", "text/plain")),
        # Input: Checkbox - header?
        checkboxInput("header", "Header", T),
        # Input: Separator?
        radioButtons("sep", "Separator", choices=c(Comma=",", 
                                                   Semicolon=";",
                                                   Tab="\t"), 
                     selected=","),
        # Input: Options Panel
        # SI #1
        selectInput("my_data", "Data:",
                    choices=list(mtcars="mtcars",
                                 MyFile="inFile"), selected=NULL),
        # SI #2
        selectInput("variable", "Plot Variable:", choices=NULL),
        # SI #3
        selectInput("group", "Additional Option:", choices=NULL),
        # SI #4
        selectInput("plot.type", "Type:",
                    list(Boxplot="Boxplot",
                         Bar="Bar",
                         Histogram="Histogram", 
                         Density="Density",
                         Scatter="Scatter",
                         Smooth="Smooth")
        ),
        checkboxInput("show.points", "Show points", T)
    ),
    # PLOT OUTPUT GOES HERE
    mainPanel(
        h3(textOutput("caption")),
        uiOutput("plot")
    )
)

server <- function(input, output, session){
    observe({
        if(!exists(input$my_data)){return()}
        var.opts <- colnames(get(input$my_data))
        updateSelectInput(session, "variable", choices=var.opts)
        updateSelectInput(session, "group", choices=var.opts)
    })
    # SHOWS PLOT TYPE AS A TITLE
    output$caption <- renderText({
        switch(input$plot.type,
               "Boxplot"="Boxplot",
               "Bar"="Bar Graph",
               "Histogram"="Histogram",
               "Density"="Density Plot",
               "Scatter"="Scatter Plot",
               "Smooth"="Scatter Plot (w/ Loess fit)")
    })
    output$plot <- renderUI({
        plotOutput("pl")
    })
    get_data <- reactive({
        if(!exists(input$my_data)){return()}
        
        check<-function(x){is.null(x) || x==""}
        
        if(check(input$my_data)){return()}
        obj <- list(data=get(input$my_data),
                  variable=input$variable,
                  group=input$group
        )
        if(any(sapply(obj, check))){return()}
        
        check <- function(obj){
            !all(c(obj$variable, obj$group) %in% colnames(obj$data))
        }
        if(check(obj)){return()}
        obj
    })
    output$pl <- renderPlot({
        plot.obj <- get_data()
        if(is.null(plot.obj)){return()}
        if(plot.obj$variable=="" | plot.obj$group==""){return()}
        plot.type <- switch(input$plot.type, 
                          "Boxplot"=geom_boxplot(aes(group=1)), 
                          "Histogram"=geom_histogram(alpha=0.5, position="identity"), 
                          "Density"=geom_density(alpha=0.75), 
                          "Bar"=geom_bar(position="dodge"), 
                          "Scatter"=geom_point(aes_string(x=plot.obj$group, 
                                                          y=plot.obj$variable)),
                          "Smooth"=list(geom_point(aes_string(x=plot.obj$group, 
                                                          y=plot.obj$variable)),  
                                        geom_smooth(formula=y~x, method="loess")))
        # see https://www.statsdirect.com/help/nonparametric_methods/loess.htm 
        # for more on LOESS Curve Fitting (I chose this as an alternative to a 
        # a linear model fit); I chose loess since it is known to perform better
        # at the boundaries, this may confer an advantage for business forecasting
        
        if(input$plot.type=="Boxplot"){
            pl <- ggplot(plot.obj$data,
                      aes_string(x=plot.obj$group, 
                                 y=plot.obj$variable, 
                                 fill=plot.obj$group
                      )) + plot.type
            
            if(input$show.points==T){
                pl <- pl + geom_point(color="black", alpha=0.5, position="jitter")
            }
        
        } else if(input$plot.type=="Scatter"){
            pl <- ggplot(plot.obj$data,
                         aes_string(x=plot.obj$group,
                                    y=plot.obj$variable)) + plot.type
        } else if(input$plot.type=="Smooth"){
            pl <- ggplot(plot.obj$data,
                         aes_string(x=plot.obj$group,
                                    y=plot.obj$variable)) + plot.type
        } else {
            pl <- ggplot(plot.obj$data,
                      aes_string(
                          x=plot.obj$variable,
                          fill=plot.obj$group,
                          group=plot.obj$group
                      )) + plot.type
            }
        # add labels
        pl <- pl + labs(
            fill=input$group,
            x=input$group,
            y=input$variable
        ) + theme_igray() + theme(axis.text.x=element_text(angle=90, hjust=0))
        print(pl)
    })
        upload_data <- reactive({
            inFile <- input$file1
            if(is.null(inFile)){return(NULL)}
            
            read.csv(inFile$datapath, header=input$header)
        })
        observeEvent(input$file1, {inFile <<- upload_data()})   
    }

shinyApp(ui, server)
