library(plotly)
library(shinythemes)
library(shinydashboard)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(dygraphs)
library(treemap)
#ADD8E6
#49C9BA

shinyUI(
  navbarPage(
    
    theme = shinytheme("flatly"),
            "Vizualizacija",
    # tabPanel("Player Statistics",
    #                   h2("Player Statistics"),
    #                   sidebarLayout(
    #                     sidebarPanel(
    #                       uiOutput("statistike")
    #                     ),
    #                     # Show a plot of the generated distribution
    #                     mainPanel(
    #                       #textOutput('tekstime'),
    #                       DT::dataTableOutput('stat')
    #                     )
    #                   )
    # ),
             tabPanel("Družbe",
                      dashboardPage(
                        skin="yellow",
                        dashboardHeader(
                          title="Osnovni podatki o družbah",
                          titleWidth = 350
                        ),
                        dashboardSidebar(
                        
                          uiOutput("statistike")
                          
                        ),
                        dashboardBody(
                          uiOutput("head2head")

                        )
                      )),

             tabPanel("Spremembe cen delnic",
                      titlePanel("Primerjava sprememb cen delnic podjetij v FTSE 100",
                                 list(tags$head(tags$style("body {background-color: #ADD8E6; }")))),
                      br(),
                      fluidRow(
                        column(7,plotlyOutput("graf")),
                        column(5,wellPanel(tags$style(".well {background-color: coral;}"),
                        fluidRow(column(7,radioButtons("izbira","Tip spremembe cene",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
                               conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -1"))),
                               column(5,radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"))),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               column(12,h6("Za koliko se je spremenila cena delnice podjetja vključenega v indeks FTSE 100 v slednjem časovnem obdobju (v %)."))),
                        checkboxInput("prim","Primerjava podjetij"),
                        conditionalPanel(condition = "input.prim ",selectInput("podjetja","Podjetja",choices = price$Epic,multiple=TRUE)),
                        h6("Izberi podjetja, ki jih želiš primerjati. Primerjava temelji na spremembi cen v določenem časovnem odbdobju. Tip spremembe cene pusti na: Vse, časovno obdobje lahko spreminjaš"))
                        )
                        ),
                      br(),
                      fluidRow(
                      column(7,plotOutput("hist")),
                      column(5,wellPanel(
                        sliderInput("bins","Število stolpcev:",min=10,max=100,value=10),
                             checkboxInput("gostota","Prikaži gostoto",value=FALSE))))
                               
      
             )
))
