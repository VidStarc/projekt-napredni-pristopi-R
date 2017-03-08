shinyUI(
  
  pageWithSidebar(
    
    
    headerPanel("Primerjava sprememb cen delnic podjetij v FTSE 100"),
    
    
    sidebarPanel(
      radioButtons("izbira","Današnje spremembe cen delnic",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
      #checkboxInput("poz","Samo pozitivne spremembe"),
      #checkboxInput("neg","Samo negativne spremembe"),
      conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -0.5"))
      #helpText("Današnje spremembe cen delnic")
      
      ),
    
      
    mainPanel(plotlyOutput("graf"))
    
  )
)