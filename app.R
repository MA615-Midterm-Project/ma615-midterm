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
  titlePanel("Berries Project"),
  hr(),
  tags$h5("Authors: Yuanming LENG, Nancy SHEN, Mi ZHANG, Peng LIU "),
  hr(),
  navbarPage("Strawberries in America",theme=shinytheme("lumen"),
             tabPanel("Dataset Overview",fluid=TRUE,icon=icon("table"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("CONDITION"),
                          
                          #select type
                          fluidRow(column(6,
                                          selectInput(inputId="TypeFinder",
                                                      label = "Select Type",
                                                      choices=unique(strawb1$measurement.s.),
                                                      selected = "MEASURED IN LB"
                                                      )),
                                   
                                   #select State
                                   column(6,
                                          checkboxGroupInput(inputId="StateFinder",
                                                             label="Select State(s):",
                                                             choices=unique(strawb1$state),
                                                             selected="CALIFORNIA")
                                   )),
                          hr(),
                          fluidRow(column(6,
                                          sliderInput(inputId = "YearFinder",
                                                      label = "Select Time Range:",
                                                      min=2015,
                                                      max=2019,
                                                      value=c(2015,2016),
                                                      width="220px")),
                                   hr(),
                                   column(6,
                                          checkboxGroupInput(inputId="ChemFinder",
                                                             label="Select Chemical:",
                                                             choices=unique(strawb1$`chemical.type`),
                                                             selected="ORGANIC STATUS"))
                                   
                          )
                        ),
                        mainPanel(
                          fluidRow(column(12,
                                          withSpinner(dataTableOutput(outputId = "Table"))
                          ))
                        )      
                      )), 
             
             
             
             ##Chemical Comparison
             tabPanel("Chemical Comparison",fluid=TRUE,icon=icon("bong"),tags$style(button_color_css),
                      tags$h2(p(icon("chart-pie"),"Comparison of Chemical on Strawberries")),
                      hr(),
                      fluidRow(
                        column(6,tags$h3("Condition A")),
                        column(6,tags$h3("Condition B"))
                      ),
                      fluidRow(
                        column(6,
                               wellPanel(
                                 fluidRow(column(6,
                                                 selectInput(inputId="TypeFinder1",
                                                             label = "Select Type",
                                                             choices=unique(strawb1$measurement.s.),
                                                             selected = "MEASURED IN LB",
                                                             width="220px"
                                                 ),
                                                 checkboxGroupInput(inputId="StateFinder1",
                                                                    label="Select State(s):",
                                                                    choices=unique(strawb1$state),
                                                                    selected="CALIFORNIA")),
                                          column(6,
                                                 sliderInput(inputId = "YearFinder1",
                                                             label = "Select Time Range:",
                                                             min=2015,
                                                             max=2019,
                                                             value=c(2015,2016),
                                                             width="220px"),
                                                 checkboxGroupInput(inputId="ChemFinder1",
                                                                    label="Select Chemical:",
                                                                    choices=unique(strawb1$chemical.type),
                                                                    selected="ORGANIC STATUS"))
                                          
                                 )
                               )),
                        column(6,
                               wellPanel(
                                 fluidRow(
                                   column(6,
                                          selectInput(inputId="TypeFinder2",
                                                     label = "Select Type",
                                                     choices=unique(strawb1$measurement.s.),
                                                     selected = "MEASURED IN LB",
                                                     width="220px"
                                          ),
                                          checkboxGroupInput(inputId="StateFinder2",
                                                             label="Select State(s):",
                                                             choices=unique(strawb1$state),
                                                             selected="CALIFORNIA")),
                                   column(6,
                                          sliderInput(inputId = "YearFinder2",
                                                      label = "Select Time Range:",
                                                      min=2015,
                                                      max=2019,
                                                      value=c(2015,2016),
                                                      width="220px"),
                                          checkboxGroupInput(inputId="ChemFinder1",
                                                             label="Select Chemical:",
                                                             choices=unique(strawb1$chemical.type),
                                                             selected="ORGANIC STATUS"))
                                   
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(6,tags$h4("Plot1 for Condition A"),
                               plotOutput("a_Hist")),
                        column(6,tags$h4("Plot2 for Condition B"),
                               plotOutput("b_Hist"))
                      ),
                      fluidRow(
                        column(6,tags$h4("Plot1 for Condition A"),
                               plotOutput("a_Pie")),
                        column(6,tags$h4("Plot2 for Condition B"),
                               plotOutput("b_Pie"))
                      )
             )
              
             
             
             
             
             
             
  ))

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
      filter(year >= input$YearFinder[1], year <= input$YearFinder[2])%>%
      filter(chemical.type %in% input$ChemFinder)
  })
  BerryFinder1<-reactive({
    req(input$TypeFinder1)
    req(input$StateFinder1)
    req(input$YearFinder1)
    req(input$ChemFinder1)
    filter(strawb1,measurement.s. %in% input$TypeFinder)%>%
      filter(state %in% input$StateFinder)%>%
      filter(year >= input$YearFinder[1], year <= input$YearFinder[2])%>%
      filter(chemical.type %in% input$ChemFinder)
  })
  BerryFinder2<-reactive({
    req(input$TypeFinder2)
    req(input$StateFinder2)
    req(input$YearFinder2)
    req(input$ChemFinder2)
    filter(strawb1,measurement.s. %in% input$TypeFinder)%>%
      filter(state %in% input$StateFinder)%>%
      filter(year >= input$YearFinder[1], year <= input$YearFinder[2])%>%
      filter(chemical.type %in% input$ChemFinder)
  })
  
  output$Table<-renderDataTable({
    datatable(BerryFinder())
  })
  
  # output$a_Hist<-renderPlot({
  #   ggplot(BerryFinder1(),aes(x= value,color= chemical.type, fill=chemical.type))+geom_density()
  # }
  # )
  # output$b_Hist<-renderPlot({
  #   ggplot(BerryFinder2(),aes(x=value,color=chemical.type,fill=chemical.type))+geom_density()
  # }
  # )
  # 
  # output$a_Pie<-renderPlot({
  #   n<-length(BerryFinder1()[1])*100
  #   blank_theme <- theme_minimal()+
  #     theme(
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       panel.border = element_blank(),
  #       panel.grid=element_blank(),
  #       axis.ticks = element_blank(),
  #       plot.title=element_text(size=14, face="bold")
  #     )
  #   ggplot(BerryFinder1(),aes(x="killer",color=Chem,fill=killer))+geom_bar(stat="count",position="stack",width=1)+
  #     coord_polar(theta="y",start=0)+labs(x='',y='',title='')+geom_text(stat="count",aes(label = scales::percent(..count../sum(..count..))), size=5, position=position_stack(vjust = 0.5),col="black")
  # })
  # output$b_Pie<-renderPlot({
  #   n<-length(BerryFinder2()[1])*100
  #   blank_theme <- theme_minimal()+
  #     theme(
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       panel.border = element_blank(),
  #       panel.grid=element_blank(),
  #       axis.ticks = element_blank(),
  #       plot.title=element_text(size=14, face="bold")
  #     )
  #   ggplot(BerryFinder2(),aes(x="killer",color=Chem,fill=killer))+geom_bar(stat="count",position="stack",width=1)+
  #     coord_polar(theta="y",start=0)+labs(x='',y='',title='')+geom_text(stat="count",aes(label = scales::percent(..count../sum(..count..))), size=5, position=position_stack(vjust = 0.5),col="black")
  # })
  # 
  
  # finder<-reactive({
  #     sberry%>%subset(sberry$Measure == input$BoxMeasure)
  # })
  # output$Boxplot<-renderPlot({
  #         ggplot(data=finder(),mapping=aes(x=State,y=Value))+geom_boxplot(outlier.colour=NA)
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

