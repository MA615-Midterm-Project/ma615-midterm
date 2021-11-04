#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(shinycssloaders)
library(shinythemes)
library(tidyr)
library(dplyr)
library(DT)
#To make the app run smoothly, we will use ag_data_strawberry.Rmd to tidy the dataset.
#The RMarkdown file will generate a .csv file names bberry.csv
#If you want to consume the script into the app, uncomment the following line.
#Make sure the origin data file, berries.csv is in your working directory.
#source("ag_data_strawberry.Rmd")
strawb1 <-read.csv2("strawb1.csv",header=TRUE,sep=",")
strawb1$value <- log(as.numeric(strawb1$value))
strawb1$year <- (as.factor(strawb1$year))
toxin <- read.csv2("Toxin.csv",header=TRUE,sep=",")

source("Function.R")

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"


# Define UI
ui <- fluidPage(
  titlePanel("A Story about Strawberry"), # set for page design
  hr(),
  tags$h5("Authors: Yuanming LENG, Mi ZHANG, Nancy SHEN, Peng LIU"),
  hr(),
  navbarPage("Quick Stats for Strawberries",theme=shinytheme("lumen"),
             tabPanel("Dataset Overview",fluid=TRUE,icon=icon("table"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Characteristics"),
                          
                          #select measurement
                          fluidRow(column(7,
                                          selectInput(inputId="TypeFinder",
                                                      label = "Select Type",
                                                      choices=unique(strawb1$measurement.s.),
                                                      selected = "MEASURED IN LB"
                                          )),
                                   
                                   #select State
                                   column(7,
                                          checkboxGroupInput(inputId="StateFinder",
                                                             label="Select State(s):",
                                                             choices=unique(strawb1$state),
                                                             selected="CALIFORNIA")
                                   )),
                          hr(),
                          #select year
                          fluidRow(column(7,
                                          checkboxGroupInput(inputId = "YearFinder",
                                                             label = "Select Time:",
                                                             choices=unique(strawb1$year),
                                                             selected = "2015",
                                                             width="220px"),
                          # select chemical.type
                                          checkboxGroupInput(inputId="ChemFinder",
                                                             label="Select Chemical:",
                                                             choices=unique(strawb1$`chemical.type`),
                                                             selected="ORGANIC STATUS"))
                                   
                          )
                        ),
                        # set the size of output table 
                        mainPanel(
                          fluidRow(column(12,
                                          withSpinner(dataTableOutput(outputId = "Table"))
                          ))
                        )      
                      )),
             ##Summary data Analysis
             tabPanel("Dataset Summary",fluid=TRUE,icon=icon("table"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Characteristics"),
                          
                          #select measurment
                          fluidRow(column(7,
                                          selectInput(inputId="TypeFinder1",
                                                      label = "Select Type",
                                                      choices=unique(strawb1$measurement.s.),
                                                      selected = "MEASURED IN LB",
                                                      width="220px"
                                                      
                                          )),
                                   hr()),
                          #select variable
                          fluidRow(column(7,
                                          selectInput(inputId = "VariFinder",
                                                      label = "Select Variable(s):",
                                                      choices= c("year","state","chemical","chemical.type"),
                                                      selected="state",
                                                      width="220px"))
                          )
                        ),
                        #set panel size of output table
                        mainPanel(
                          fluidRow(column(12,
                                          withSpinner(dataTableOutput(outputId = "Table3"))
                          )),
                          fluidRow(column(12,tags$h4("Comparison Plot"),
                                          plotOutput("Plot")
                                          
                                          
                          ))
                        )      
                      ))
             
             
  )
)


# Define server logic required to draw a histogram
server <- function(session,input, output) {
  observe({
    x<-BerryFinder()%>%select("measurement.s.")%>%unique()
    updateSelectInput(session,"MeasureFinder",label="Select Measurement",choices=x,selected=tail(x,1))
  })
  
  BerryFinder<-reactive({
    req(input$TypeFinder)
    req(input$StateFinder)
    req(input$YearFinder)
    req(input$ChemFinder)
    filter(strawb1,measurement.s. %in% input$TypeFinder)%>%
      filter(state %in% input$StateFinder)%>%
      filter(year %in% input$YearFinder)%>%
      filter(chemical.type %in% input$ChemFinder)
  })# extract data from parameters
  
  BerryFinder1<-reactive({
    req(input$TypeFinder1)
    req(input$VariFinder)
    filter(strawb1,measurement.s. %in% input$TypeFinder1)%>%
      select(c(input$VariFinder, (value)))
  })# extract data from parameters
  
  # function of output table
  output$Table<-renderDataTable({
    datatable(BerryFinder())
  })
  # code for output plot
  output$Plot <- renderPlot({
    n<-length(BerryFinder1()[1])*100
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    GGally::ggpairs(BerryFinder1(), columns=c(input$VariFinder, "value"), aes(color=BerryFinder1()[,1], alpha = 0.8))
  }) # data EDA
  
  output$Table3 <- renderDataTable({
    Group(BerryFinder1()[,c(input$VariFinder, "value")]) #within group comparision
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")