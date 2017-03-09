library(shiny)
library(plotly)
#library(showtext)
#font.add.google("Gochi Hand", "gochi")
#showtext.auto()
#set.seed(123)

shinyServer(function(input, output) {
  
  output$graf<-renderPlotly(
    if(input$izbira2=='današnja'){
      if(input$izbira=="Vse"){plot_ly(x=price$Epic,y=price$`TodayChg(%)`,type='bar',marker=list(color='red')) %>% 
                            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"))}
        else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`TodayChg(%)`>=0)],
                                                              y=price$`TodayChg(%)`[price$`TodayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
                                                      layout(title="Današnje spremembe različnih delnic")}
          else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5)],
                                                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-0.5],
                                                     type='bar',marker=list(color='red'))%>% 
                                                      layout(title="Današnje spremembe različnih delnic")}

            else{plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0)],
                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                      layout(title="Današnje spremembe različnih delnic")}}}} 
    else{
      if(input$izbira2=='7 dnevna'){
        if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`7DayChg(%)`,type='bar',marker=list(color='red')) %>% 
          layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"))}
          
          else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`7DayChg(%)`>=0)],
                                                                y=price$`7DayChg(%)`[price$`7DayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
          layout(title="Današnje spremembe različnih delnic")}
        else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`7DayChg(%)` <0 & price$`7DayChg(%)`>-0.5)],
                                          y=price$`7DayChg(%)`[price$`7DayChg(%)` <0 & price$`7DayChg(%)`>-0.5],
                                          type='bar',marker=list(color='red'))%>% 
            layout(title="Današnje spremembe različnih delnic")}
          
          else{plot_ly(x=price$Epic[which(price$`7DayChg(%)`<0)],
                       y=price$`7DayChg(%)`[price$`7DayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
              layout(title="Današnje spremembe različnih delnic")}}}
      }
      else{if(input$izbira2=='30 dnevna'){
        if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`30DayChg(%)`,type='bar',marker=list(color='red')) %>% 
          layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"))}
        
        else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`30DayChg(%)`>=0)],
                                                                  y=price$`30DayChg(%)`[price$`30DayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
            layout(title="Današnje spremembe različnih delnic")}
          else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`30DayChg(%)` <0 & price$`30DayChg(%)`>-0.5)],
                                            y=price$`30DayChg(%)`[price$`30DayChg(%)` <0 & price$`30DayChg(%)`>-0.5],
                                            type='bar',marker=list(color='red'))%>% 
              layout(title="Današnje spremembe različnih delnic")}
            
            else{plot_ly(x=price$Epic[which(price$`30DayChg(%)`<0)],
                         y=price$`30DayChg(%)`[price$`30DayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                layout(title="Današnje spremembe različnih delnic")}}}}
        else{
          if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`6MonthChg(%)`,type='bar',marker=list(color='red')) %>% 
            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"))}
          
          else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`6MonthChg(%)`>=0)],
                                                                    y=price$`6MonthChg(%)`[price$`6MonthChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
              layout(title="Današnje spremembe različnih delnic")}
            else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`6MonthChg(%)` <0 & price$`6MonthChg(%)`>-0.5)],
                                              y=price$`6MonthChg(%)`[price$`6MonthChg(%)` <0 & price$`6MonthChg(%)`>-0.5],
                                              type='bar',marker=list(color='red'))%>% 
                layout(title="Današnje spremembe različnih delnic")}
              
              else{plot_ly(x=price$Epic[which(price$`6MonthChg(%)`<0)],
                           y=price$`6MonthChg(%)`[price$`6MonthChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                  layout(title="Današnje spremembe različnih delnic")}}}}
        }
      
    }
      
  )
  
  output$hist<-renderPlot({
    if(input$izbira2=='današnja'){x<-price$`TodayChg(%)`
    yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
    maks<-max(max(density(x)$y),max(yhist$density))
    hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
         main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
    title("Histogram",font=1)
    if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
    else{
      if(input$izbira2=='7 dnevna'){x<-price$`7DayChg(%)`
      yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
      maks<-max(max(density(x)$y),max(yhist$density))
      hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
           main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
      title("Histogram",font=1)
      if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
      
      else{
        if(input$izbira2=='30 dnevna'){x<-price$`30DayChg(%)`
        yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
        maks<-max(max(density(x)$y),max(yhist$density))
        hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
             main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
        title("Histogram",font=1)
        if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
        
        else{x<-price$`6MonthChg(%)`
        yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
        maks<-max(max(density(x)$y),max(yhist$density))
        hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
             main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
        title("Histogram",font=1)
        if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
      }
    }

  })
  
})