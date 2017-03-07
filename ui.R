shinyUI(
  
  pageWithSidebar(
    
    
    headerPanel("Primerjava sprememb cen delnic podjetij v FTSE 100"),
    
    
    sidebarPanel(
      checkboxInput("poz","Samo pozitivne spremembe"),
      checkboxInput("neg","Samo negativne spremembe"),
      conditionalPanel(condition = "input.neg == true",checkboxInput("neg1","večje od -0.5")),
      helpText("Današnje spremembe cen delnic")
      
      ),
    
      
    mainPanel(plotOutput("histogram"))
    
  )
)