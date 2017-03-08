library(shiny)
library(plotly)

shinyServer(function(input, output) {
  
  output$graf<-renderPlotly(
    if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`TodayChg(%)`,type='bar',marker=list(color='red')) %>% 
                            layout(title="Današnje spremembe različnih delnic")}
    else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`TodayChg(%)`>=0)],
                                                              y=price$`TodayChg(%)`[price$`TodayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
                                                      layout(title="Današnje spremembe različnih delnic")}
      else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5)],
                                                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5],
                                                     type='bar',marker=list(color='red'))%>% 
                                                      layout(title="Današnje spremembe različnih delnic")}

        else{plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0)],
                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                      layout(title="Današnje spremembe različnih delnic")}}}
      
  )
  
  output$hist<-renderPlot({
    x<-price$`TodayChg(%)`
    hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability = TRUE,
         main="Histogram",col = 'blue',border = 'white')
    if(input$gostota){lines(density(x),col='black',lwd=1.5)}
  })
  
})