library(plotly)
library(shinythemes)
shinyUI(
  navbarPage(theme = shinytheme("flatly"),
            "Vizualizacija",
             tabPanel("Spremembe",
                      titlePanel("Primerjava sprememb cen delnic podjetij v FTSE 100"),
                      br(),
                      fluidRow(
                        column(5,wellPanel(
                        fluidRow(column(6,radioButtons("izbira","Tip spremembe cene",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
                               conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -0.5"))),
                               column(6,radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"))),
                               br(),
                               helpText("za koliko se je spremenila cena delnice podjetja vključenega v indeks FTSE 100 v slednjem časovnem obdobju (v %)"))
                        )
                        ),
                      
                        column(7,plotlyOutput("graf"))
                        ),
                      fluidRow(column(5),
                      column(7,plotOutput("hist"),
                               radioButtons("bins","število stolpcev:",choices = c(10,15,20,25,30,50,100),selected=10,inline=TRUE),
                               checkboxInput("gostota","Prikaži gostoto",value=FALSE)))
      
             ),
            tabPanel("Primerjava",
                     titlePanel("Primerjava sprememb cen delnic"),
                     br(),
                     radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"),inline=TRUE)
                     )
  )
)
