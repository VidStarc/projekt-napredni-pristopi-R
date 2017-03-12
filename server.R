library(shiny)
library(plotly)
library(shinydashboard)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(dygraphs)
library(treemap)
#library(showtext)
#font.add.google("Gochi Hand", "gochi")
#showtext.auto()
#set.seed(123)

shinyServer(function(input, output) {
  
  output$graf<-renderPlotly(
    if(input$prim==FALSE){
    if(input$izbira2=='današnja'){
      if(input$izbira=="Vse"){plot_ly(x=price$Epic,y=price$`TodayChg(%)`,type='bar',marker=list(color='red')) %>% 
                            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %")
                                   ,plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
        else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`TodayChg(%)`>=0)],
                                                              y=price$`TodayChg(%)`[price$`TodayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
                                                      layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
          else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-1)],
                                                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0 & price$`TodayChg(%)`>-1],
                                                     type='bar',marker=list(color='red'))%>% 
                                                      layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}

            else{plot_ly(x=price$Epic[which(price$`TodayChg(%)`<0)],
                     y=price$`TodayChg(%)`[price$`TodayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                      layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}}}} 
    else{
      if(input$izbira2=='7 dnevna'){
        if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`7DayChg(%)`,type='bar',marker=list(color='red')) %>% 
          layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
          
          else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`7DayChg(%)`>=0)],
                                                                y=price$`7DayChg(%)`[price$`7DayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
          layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
        else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`7DayChg(%)` <0 & price$`7DayChg(%)`>-1)],
                                          y=price$`7DayChg(%)`[price$`7DayChg(%)` <0 & price$`7DayChg(%)`>-1],
                                          type='bar',marker=list(color='red'))%>% 
            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
          
          else{plot_ly(x=price$Epic[which(price$`7DayChg(%)`<0)],
                       y=price$`7DayChg(%)`[price$`7DayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
              layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}}}
      }
      else{if(input$izbira2=='30 dnevna'){
        if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`30DayChg(%)`,type='bar',marker=list(color='red')) %>% 
          layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
        
        else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`30DayChg(%)`>=0)],
                                                                  y=price$`30DayChg(%)`[price$`30DayChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
          else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`30DayChg(%)` <0 & price$`30DayChg(%)`>-1)],
                                            y=price$`30DayChg(%)`[price$`30DayChg(%)` <0 & price$`30DayChg(%)`>-1],
                                            type='bar',marker=list(color='red'))%>% 
              layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
            
            else{plot_ly(x=price$Epic[which(price$`30DayChg(%)`<0)],
                         y=price$`30DayChg(%)`[price$`30DayChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}}}}
        else{
          if(input$izbira=='Vse'){plot_ly(x=price$Epic,y=price$`6MonthChg(%)`,type='bar',marker=list(color='red')) %>% 
            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
          
          else{if(input$izbira=='Samo pozitivne spremembe'){plot_ly(x=price$Epic[which(price$`6MonthChg(%)`>=0)],
                                                                    y=price$`6MonthChg(%)`[price$`6MonthChg(%)`>=0],type='bar',marker=list(color='red'))%>% 
              layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
            else{if(input$neg1==TRUE){plot_ly(x=price$Epic[which(price$`6MonthChg(%)` <0 & price$`6MonthChg(%)`>-1)],
                                              y=price$`6MonthChg(%)`[price$`6MonthChg(%)` <0 & price$`6MonthChg(%)`>-1],
                                              type='bar',marker=list(color='red'))%>% 
                layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
              
              else{plot_ly(x=price$Epic[which(price$`6MonthChg(%)`<0)],
                           y=price$`6MonthChg(%)`[price$`6MonthChg(%)`<0],type='bar',marker=list(color='red'))%>% 
                  layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}}}}
        }
      
    }}
   else{tab<-price
          if (length(input$podjetja) == 1) {
            tab <- tab %>% filter(Epic == input$podjetja)
          } else {
            tab <- tab %>% filter(Epic %in% input$podjetja)
          }
     if(input$izbira2=='današnja'){
          plot_ly(x=input$podjetja,y=tab$`TodayChg(%)`,type='bar',marker=list(color='red'))%>% 
            layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')
     }
   else{
     if(input$izbira2=='7 dnevna'){
       plot_ly(x=input$podjetja,y=tab$`7DayChg(%)`,type='bar',marker=list(color='red'))%>% 
         layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')
     }
     else{
       if(input$izbira2=='30 dnevna'){
         plot_ly(x=input$podjetja,y=tab$`30DayChg(%)`,type='bar',marker=list(color='red'))%>% 
           layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')
       }
       else{plot_ly(x=input$podjetja,y=tab$`6MonthChg(%)`,type='bar',marker=list(color='red'))%>% 
           layout(title="Spremembe cen delnic podjetij",yaxis=list(title="Sprememba cene delnice v %"),plot_bgcolor='#ADD8E6',paper_bgcolor='#ADD8E6')}
     }
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