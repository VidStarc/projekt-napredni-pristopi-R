library(shiny)
library(plotly)

shinyServer(function(input, output) {
  
  output$graf<-renderPlotly(
    if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`TodayChg(%)`,type='bar',
                                    name="Današnje spremembe različnih delnic")}
    else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`TodayChg(%)`>=0)],
                                                              y=price$`TodayChg(%)`[price$`TodayChg(%)`>=0],type='bar',
                                                              name="Današnje spremembe različnih delnic")}
      else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5)],
                                                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5],
                                                     type='bar',name="Današnje spremembe različnih delnic")}

        else{plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0)],y=price$`TodayChg(%)`[price$`TodayChg(%)`<0],type='bar',
                     name="Današnje spremembe različnih delnic")}}}

   
      
  )
  
})