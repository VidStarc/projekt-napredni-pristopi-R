library(plotly)
library(shinythemes)
shinyUI(
  navbarPage(theme = shinytheme("superhero"),
            "Vizualizacija",
             tabPanel("Spremembe",
                      titlePanel("Primerjava sprememb cen delnic podjetij v FTSE 100"),
                      column(4,wellPanel(
                        fluidRow(
                          column(5,radioButtons("izbira","Tip spremembe cene",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
      #checkboxInput("poz","Samo pozitivne spremembe"),
      #checkboxInput("neg","Samo negativne spremembe"),
                            conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -0.5"))
      #helpText("Današnje spremembe cen delnic"))
                        ),
                          column(6,radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"))
      
      )),
      helpText("za koliko se je spremenila cena delnice podjetja vključenega v indeks FTSE 100 v slednjem časovnem obdobju (v %)"))),
      
    column(8,plotlyOutput("graf"),hr(),
              plotOutput("hist"),
              radioButtons("bins","število stolpcev:",choices = c(10,15,20,25,30,50,100),selected=10,inline=TRUE),
              checkboxInput("gostota","Prikaži gostoto",value=FALSE)
              )
    
  )
)
)
