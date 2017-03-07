library(shiny)

shinyServer(function(input, output) {
  
  output$histogram<-renderPlot(
    if(input$poz==TRUE & input$neg==TRUE){barplot(price$`TodayChg(%)`,main="Današnje spremembe različnih delnic", ylim=c(-8,2),
                                                  names.arg=price$Epic,las=2, cex.names=0.5)}
    else{if(input$poz==FALSE){
     if(input$neg==FALSE){
       barplot(price$`TodayChg(%)`,main="Današnje spremembe različnih delnic", ylim=c(-8,2),
            names.arg=price$Epic,las=2, cex.names=0.5)}
     else{if(input$neg1==TRUE){barplot(price$`TodayChg(%)`[price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5],
                                       main="Današnje spremembe različnih delnic", ylim=c(-1,0),
                                       names.arg=price$Epic[which(price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5)],las=2,cex.names=0.8)}
       else{barplot(price$`TodayChg(%)`[price$`TodayChg(%)`<0],
                main="Današnje spremembe različnih delnic", ylim=c(-8,0),
                names.arg=price$Epic[which(price$`TodayChg(%)`<0)],las=2,cex.names=0.8)}}
     }
   else{barplot(price$`TodayChg(%)`[price$`TodayChg(%)`>=0],
                main="Današnje spremembe različnih delnic", ylim=c(0,2),
                names.arg=price$Epic[which(price$`TodayChg(%)`>=0)],las=2)}}
      
  )
  
})