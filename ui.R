library(plotly)
library(shinythemes)
#ADD8E6
#49C9BA

shinyUI(
  navbarPage(
    
    theme = shinytheme("flatly"),
            "Vizualizacija",
             tabPanel("Spremembe",
                      titlePanel("Primerjava sprememb cen delnic podjetij v FTSE 100",
                                 list(tags$head(tags$style("body {background-color: #ADD8E6; }")))),
                      br(),
                      fluidRow(
                        column(7,plotlyOutput("graf")),
                        column(5,wellPanel(tags$style(".well {background-color: springgreen;}"),
                        fluidRow(column(7,radioButtons("izbira","Tip spremembe cene",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
                               conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -1"))),
                               column(5,radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"))),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               column(12,helpText("Za koliko se je spremenila cena delnice podjetja vključenega v indeks FTSE 100 v slednjem časovnem obdobju (v %)."))),
                        checkboxInput("prim","Primerjava podjetij"),
                        conditionalPanel(condition = "input.prim ",selectInput("podjetja","Podjetja",choices = price$Epic,multiple=TRUE)),
                        helpText("Izberi podjetja, ki jih želiš primerjati. Primerjava temelji na spremembi cen v določenem časovnem odbdobju. Tip spremembe cene pusti na: Vse, časovno obdobje lahko spreminjaš"))
                        )
                        ),
                      br(),
                      fluidRow(
                      column(7,plotOutput("hist")),
                      column(5,wellPanel(
                        sliderInput("bins","število stolpcev:",min=10,max=100,value=10),
                             checkboxInput("gostota","Prikaži gostoto",value=FALSE))))
                               
      
             ),
            tabPanel("Aplikacija-Neža")
  )
)
