library(shiny)

shinyServer(function(input, output) {
  
  output$graf<-renderPlot(
    if(input$izbira=='Vse'){barplot(price$`TodayChg(%)`,main="Današnje spremembe različnih delnic", ylim=c(-8,2),
                                                  names.arg=price$Epic,las=2, cex.names=0.5)}
    else{if(input$izbira=='Samo pozitivne spremembe'){barplot(price$`TodayChg(%)`[price$`TodayChg(%)`>=0],
                main="Današnje spremembe različnih delnic", ylim=c(0,2),
                names.arg=price$Epic[which(price$`TodayChg(%)`>=0)],las=2)}
      else{if(input$neg1==TRUE){barplot(price$`TodayChg(%)`[price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5],
                                  main="Današnje spremembe različnih delnic", ylim=c(-1,0),
                                  names.arg=price$Epic[which(price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5)],
                                  las=2,cex.names=0.8)}
        else{barplot(price$`TodayChg(%)`[price$`TodayChg(%)`<0],
                                       main="Današnje spremembe različnih delnic", ylim=c(-8,0),
                                       names.arg=price$Epic[which(price$`TodayChg(%)`<0)],
                   las=2,cex.names=0.8)}}}
   
      
  )
  
})