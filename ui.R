library(plotly)
shinyUI(
  
  pageWithSidebar(
    
    
    headerPanel("Primerjava sprememb cen delnic podjetij v FTSE 100"),
    
    
    sidebarPanel(
      radioButtons("izbira","Tip spremembe cene",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
      #checkboxInput("poz","Samo pozitivne spremembe"),
      #checkboxInput("neg","Samo negativne spremembe"),
      conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -0.5")),
      #helpText("Današnje spremembe cen delnic")
      hr(),
      radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"))
      ),
    
      
    mainPanel(plotlyOutput("graf"),
              plotOutput("hist"),
              radioButtons("bins","število stolpcev:",choices = c(10,15,20,25,30,50,100),selected=10,inline=TRUE),
              checkboxInput("gostota","Prikaži gostoto",value=FALSE)
              )
    
  )
)