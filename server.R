
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

shinyServer(function(input, output, session) {
  
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
    par(bg="#ADD8E6")
    hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
         main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
    title("Histogram",font=1)
    if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
    else{
      if(input$izbira2=='7 dnevna'){x<-price$`7DayChg(%)`
      yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
      maks<-max(max(density(x)$y),max(yhist$density))
      par(bg="#ADD8E6")
      hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
           main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
      title("Histogram",font=1)
      if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
      
      else{
        if(input$izbira2=='30 dnevna'){x<-price$`30DayChg(%)`
        yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
        maks<-max(max(density(x)$y),max(yhist$density))
        par(bg="#ADD8E6")
        hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
             main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
        title("Histogram",font=1)
        if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
        
        else{x<-price$`6MonthChg(%)`
        yhist<-hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),plot=FALSE)
        maks<-max(max(density(x)$y),max(yhist$density))
        par(bg="#ADD8E6")
        hist(x,breaks = seq(min(x), max(x), length.out = as.integer(input$bins) + 1),probability=TRUE,
             main="",col = 'blue',border = 'white',ylab="gostota",ylim=c(0,maks))
        title("Histogram",font=1)
        if(input$gostota){lines(density(x),col='black',lwd=1.5)}}
      }
    }
    
  })
  
  
  output$statistike <- renderUI({
    checkboxGroupInput(inputId='izberi_stat', label='Izberi družbo:',
                       choices=setNames(fin$Name, fin$Name))
  })
  
  
  #output$sta <- renderPrint(fin[1:6,2])
  #output$sta1 <- renderPrint(input$izberi_stat)
  

  
  output$stat <- DT::renderDataTable({
    # Naredimo poizvedbo 
    fin <- data.frame(fin[,-6])
    fin <- data.frame(fin,fund[,c(-1,-2)])
    rownames(fin)<-fin$Epic
    fin$link <-createLink(fin$Name)
    validate(need(!is.null(input$izberi_stat), ""))
    if (!is.null(input$izberi_stat)) {
      fin <- fin[which(fin$Name %in% input$izberi_stat),] %>% data.frame()
    }
    validate(need(nrow(fin) > 0, " "))
    fin <- t(fin[,-1])
    rownames(fin) <- c("Name", "Revenue [v mio]", "Pre-tax profit [v mio]", "Earnings Per Share [delež]", "Cash & Equivity", "Market Cap [v mio]", "Shares in issue [v mio]", "P/E ration", "Dividend Yield [v %]", "Link")
    fin}, escape=FALSE) 
  
  h2hPanel <- fluidRow(
    
    box(title=a(href='http://www.angloamerican.com/', fin[1,2]), status='warning', solidHeader = TRUE,
        img(src="anglo-american-logo.png", height = 80, width = 160), width=2),
    box(title=a(href="http://www.abf.co.uk/", fin[2,2]), status='warning', solidHeader = TRUE,
        img(src="Associated.png", height = 80, width = 150), width=2),
    box(title=a(href="https://admiralgroup.co.uk/", fin[3,2]), status='warning', solidHeader = TRUE,
        img(src="admiral.jpg", height = 80, width = 150), width=2),
    box(title=a(href="http://www.ashtead-group.com/", fin[4,2]), status='warning', solidHeader = TRUE,
        img(src="ash.jpg", height = 80, width = 150), width=2),
    box(title=a(href="http://www.antofagasta.co.uk/", fin[5,2]), status='warning', solidHeader = TRUE,
        img(src="logo.png", height = 80, width = 150), width=2),
    box(title=a(href="http://www.aviva.co.uk/", fin[6,2]), status='warning', solidHeader = TRUE,
        img(src="aviva.png", height = 80, width = 150), width=2),
    box(title=a(href="http://www.aviva.co.uk/", fin[7,2]), status='warning', solidHeader = TRUE,
        img(src="astra.jpg", height = 75, width = 150), width=2),
    box(title=a(href="http://www.baesystems.com/en/home", fin[8,2]), status='warning', solidHeader = TRUE,
        img(src="bae.png", height = 75, width = 170), width=2),
    box(title=a(href="https://www.babcockinternational.com/", fin[9,2]), status='warning', solidHeader = TRUE,
        img(src="bab.jpg", height = 60, width = 150), width=2),
    box(title=a(href="http://www.barclays.co.uk/PersonalBanking/P1242557947640", fin[10,2]), status='warning', solidHeader = TRUE,
        img(src="bar.jpg", height = 78, width = 170), width=2),
    box(title=a(href="http://www.bat.com/global", fin[11,2]), status='warning', solidHeader = TRUE,
        img(src="britis.jpg", height = 78, width = 150), width=2),
    
    box(title=a(href="http://www.barrattdevelopments.co.uk/", fin[12,2]), status='warning', solidHeader = TRUE,
        img(src="Barratt.jpg", height = 78, width = 170), width=2),
    box(title=a(href="http://www.britishland.com/", fin[13,2]), status='warning', solidHeader = TRUE,
        img(src="prenos.jpg", height = 60, width = 160), width=2),
    box(title=a(href="http://www.bhpbilliton.com/", fin[14,2]), status='warning', solidHeader = TRUE,
        img(src="bhp.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.bunzl.com/", fin[15,2]), status='warning', solidHeader = TRUE,
        img(src="bunzl.jpg", height = 60, width = 160), width=2),
    box(title=a(href="http://www.bp.com/", fin[16,2]), status='warning', solidHeader = TRUE,
        img(src="bplogo.jpg", height = 60, width = 160), width=2),
    box(title=a(href="http://www.burberryplc.com/", fin[17,2]), status='warning', solidHeader = TRUE,
        img(src="burb.png", height = 60, width = 130), width=2),
    box(title=a(href="http://www.btplc.com/", fin[18,2]), status='warning', solidHeader = TRUE,
        img(src="bt.jpg", height = 60, width = 160), width=2),
    
    box(title=a(href="http://coca-colahellenic.com/", fin[19,2]), status='warning', solidHeader = TRUE,
        img(src="coca-cola.png", height = 60, width = 160), width=2),
    box(title=a(href="https://www.carnival.com/", fin[20,2]), status='warning', solidHeader = TRUE,
        img(src="Carnival.png", height = 60, width = 160), width=2),
    box(title=a(href="https://www.centrica.com/", fin[21,2]), status='warning', solidHeader = TRUE,
        img(src="centrica.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.compass-group.com/", fin[22,2]), status='warning', solidHeader = TRUE,
        img(src="Compass.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.capita.com/", fin[23,2]), status='warning', solidHeader = TRUE,
        img(src="capita.jpg", height = 60, width = 160), width=2),
    box(title=a(href="http://www.croda.com/en-gb", fin[24,2]), status='warning', solidHeader = TRUE,
        img(src="Croda.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.crh.com/", fin[25,2]), status='warning', solidHeader = TRUE,
        img(src="CRH.jpg", height = 60, width = 160), width=2),
    
    box(title=a(href="https://www.convatecgroup.com/", fin[26,2]), status='warning', solidHeader = TRUE,
        img(src="conv.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.dixonscarphone.com/", fin[27,2]), status='warning', solidHeader = TRUE,
        img(src="dc.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.dcc.ie/", fin[28,2]), status='warning', solidHeader = TRUE,
        img(src="DCC.png", height = 74, width = 160), width=2),
    box(title=a(href="http://www.diageo.com/en-ie/pages/default.aspx", fin[29,2]), status='warning', solidHeader = TRUE,
        img(src="Diageo.jpg", height = 74, width = 160), width=2),
    
    box(title=a(href="http://www.directlinegroup.com/", fin[30,2]), status='warning', solidHeader = TRUE,
        img(src="Direc.jpg", height = 74, width = 160), width=2),
    
    box(title=a(href="http://www.experian.com/", fin[31,2]), status='warning', solidHeader = TRUE,
        img(src="exp.png", height = 60, width = 160), width=2),
    box(title=a(href="http://corporate.easyjet.com/", fin[32,2]), status='warning', solidHeader = TRUE,
        img(src="easy.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.fresnilloplc.com/", fin[33,2]), status='warning', solidHeader = TRUE,
        img(src="Fresnillo.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.gkn.com/", fin[34,2]), status='warning', solidHeader = TRUE,
        img(src="gkn.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.glencore.com/", fin[35,2]), status='warning', solidHeader = TRUE,
        img(src="glen.png", height = 60, width = 160), width=2),
    box(title=a(href="http://www.gsk.com/", fin[36,2]), status='warning', solidHeader = TRUE,
        img(src="GSK.jpg", height = 60, width = 160), width=2),
    
    
    box(title=a(href="https://www.google.si/search?newwindow=1&espv=2&q=Hikma+Pharmaceuticals+plc&oq=Hikma+Pharmaceuticals+plc&gs_l=serp.3...586047.586634.0.586785.4.4.0.0.0.0.113.429.0j4.4.0....0...1c.1.64.serp..0.0.0.k9ABFu8djAo", fin[37,2]), status='warning', solidHeader = TRUE,
        img(src="Hikma.jpg", height = 55, width = 160), width=2),
    box(title=a(href="http://www.hl.co.uk/", fin[38,2]), status='warning', solidHeader = TRUE,
        img(src="ha.jpg", height = 75, width = 160), width=2),
    box(title=a(href="http://www.hammerson.com/", fin[39,2]), status='warning', solidHeader = TRUE,
        img(src="ham.png", height = 75, width = 160), width=2),
    box(title=a(href="http://www.hsbc.com/", fin[40,2]), status='warning', solidHeader = TRUE,
        img(src="hsbc.png", height = 64, width = 160), width=2),
    
    box(title=a(href="http://www.iairgroup.com/phoenix.zhtml?c=240949&p=index", fin[41,2]), status='warning', solidHeader = TRUE,
        img(src="IAG.jpg", height = 50, width = 160), width=2),
    box(title=a(href="https://www.ihgplc.com/", fin[42,2]), status='warning', solidHeader = TRUE,
        img(src="Inte.png", height = 68, width = 160), width=2),
    box(title=a(href="http://www.3i.com/", fin[43,2]), status='warning', solidHeader = TRUE,
        img(src="3i.png", height = 70, width = 160), width=2),
    box(title=a(href="https://www.google.si/search?q=Imperial+Brands&oq=Imperial+Brands&aqs=chrome..69i57j0l5.343j0j7&sourceid=chrome&ie=UTF-8", fin[44,2]), status='warning', solidHeader = TRUE,
        img(src="imperial.png", height = 70, width = 160), width=2),
    
    box(title=a(href="http://informa.com/", fin[45,2]), status='warning', solidHeader = TRUE,
        img(src="Informa.png", height = 70, width = 160), width=2),
    box(title=a(href="https://www.intugroup.co.uk/en/", fin[46,2]), status='warning', solidHeader = TRUE,
        img(src="intu.jpg", height = 70, width = 160), width=2),
    box(title=a(href="http://www.intertek.com/", fin[47,2]), status='warning', solidHeader = TRUE,
        img(src="intertek.jpg", height = 70, width = 160), width=2),
    box(title=a(href="http://www.itv.com/", fin[48,2]), status='warning', solidHeader = TRUE,
        img(src="ITV.png", height = 70, width = 160), width=2),
    
    box(title=a(href="http://www.matthey.com/", fin[49,2]), status='warning', solidHeader = TRUE,
        img(src="JM.jpg", height = 70, width = 160), width=2),
    box(title=a(href="http://www.kingfisher.com/", fin[50,2]), status='warning', solidHeader = TRUE,
        img(src="wkingfisher.jpg", height = 70, width = 160), width=2),
    box(title=a(href="http://www.landsecurities.com/", fin[51,2]), status='warning', solidHeader = TRUE,
        img(src="Land.png", height = 70, width = 170), width=2),
    
    box(title=a(href="http://www.legalandgeneralgroup.com/", fin[52,2]), status='warning', solidHeader = TRUE,
        img(src="legal.jpg", height = 70, width = 170), width=2),
    box(title=a(href="http://www.lloydsbankinggroup.com/our-group/", fin[53,2]), status='warning', solidHeader = TRUE,
        img(src="Lloyds.png", height = 70, width = 180), width=2),
    box(title=a(href="http://www.lseg.com/", fin[54,2]), status='warning', solidHeader = TRUE,
        img(src="lseg.jpg", height = 70, width = 180), width=2),
    
    box(title=a(href="https://www.microfocus.com/", fin[55,2]), status='warning', solidHeader = TRUE,
        img(src="micro.jpg", height = 70, width = 170), width=2),
    box(title=a(href="http://ir.mediclinic.com/phoenix.zhtml?c=145797&p=irol-irhome", fin[56,2]), status='warning', solidHeader = TRUE,
        img(src="Med.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.merlinentertainments.biz/", fin[57,2]), status='warning', solidHeader = TRUE,
        img(src="merlin.jpg", height = 70, width = 170), width=2),
    box(title=a(href="http://corporate.marksandspencer.com/", fin[58,2]), status='warning', solidHeader = TRUE,
        img(src="MS.png", height = 70, width = 170), width=2),
    
    box(title=a(href="https://www.mondigroup.com/en/home/", fin[59,2]), status='warning', solidHeader = TRUE,
        img(src="Mondi.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.theice.com/products/38716842/Morrison-Wm-Supermarkets-Plc", fin[60,2]), status='warning', solidHeader = TRUE,
        img(src="morrisons.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.nationalgridus.com/Default", fin[61,2]), status='warning', solidHeader = TRUE,
        img(src="National.jpg", height = 70, width = 170), width=2),
    
    
    box(title=a(href="http://www.nextplc.co.uk/", fin[62,2]), status='warning', solidHeader = TRUE,
        img(src="Next.jpg", height = 70, width = 170), width=2),
    box(title=a(href="http://www.oldmutualplc.com/", fin[63,2]), status='warning', solidHeader = TRUE,
        img(src="old.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.providentfinancial.com/", fin[64,2]), status='warning', solidHeader = TRUE,
        img(src="PFG.jpg", height = 70, width = 170), width=2),
    box(title=a(href="https://www.paddypowerbetfair.com/", fin[65,2]), status='warning', solidHeader = TRUE,
        img(src="paddd.jpg", height = 70, width = 170), width=2),
    box(title=a(href="https://www.prudential.com/", fin[66,2]), status='warning', solidHeader = TRUE,
        img(src="prud.png", height = 70, width = 170), width=2),
    box(title=a(href="http://corporate.persimmonhomes.com/", fin[67,2]), status='warning', solidHeader = TRUE,
        img(src="Persimmon.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.pearsoned.co.uk/", fin[68,2]), status='warning', solidHeader = TRUE,
        img(src="pear.png", height = 70, width = 170), width=2),
    
    box(title=a(href="https://www.rb.com/", fin[69,2]), status='warning', solidHeader = TRUE,
        img(src="reckitt.jpg", height = 70, width = 170), width=2),
    box(title=a(href="http://www.rbs.com/", fin[70,2]), status='warning', solidHeader = TRUE,
        img(src="royal.png", height = 60, width = 170), width=2),
    box(title=a(href="http://www.shell.com/", fin[71,2]), status='warning', solidHeader = TRUE,
        img(src="shell.png", height = 80, width = 170), width=2),
    box(title=a(href="http://www.relx.com/Pages/Home.aspx", fin[73,2]), status='warning', solidHeader = TRUE,
        img(src="RELX.png", height = 80, width = 170), width=2),
    box(title=a(href="http://www.riotinto.com/", fin[74,2]), status='warning', solidHeader = TRUE,
        img(src="Rio.png", height = 70, width = 170), width=2),
    
    
    box(title=a(href="http://www.royalmail.com/", fin[75,2]), status='warning', solidHeader = TRUE,
        img(src="Royal_Mail.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.rolls-royce.com/", fin[76,2]), status='warning', solidHeader = TRUE,
        img(src="rr.jpg", height = 70, width = 170), width=2),
    box(title=a(href="http://www.randgoldresources.com/", fin[77,2]), status='warning', solidHeader = TRUE,
        img(src="rand.jpg", height = 70, width = 170), width=2),
    box(title=a(href="https://www.rsagroup.com/", fin[78,2]), status='warning', solidHeader = TRUE,
        img(src="RSA.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.j-sainsbury.co.uk/", fin[79,2]), status='warning', solidHeader = TRUE,
        img(src="sa.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.schroders.com/", fin[80,2]), status='warning', solidHeader = TRUE,
        img(src="Schroders.jpg", height = 70, width = 170), width=2),
    
    box(title=a(href="http://www.thesagegroup.com/", fin[81,2]), status='warning', solidHeader = TRUE,
        img(src="sage.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.shire.com/", fin[82,2]), status='warning', solidHeader = TRUE,
        img(src="shire.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.google.si/search?newwindow=1&q=Smurfit+Kappa+Group+plc&oq=Smurfit+Kappa+Group+plc&gs_l=serp.3..0i19k1l8j0i22i30i19k1l2.579548.580314.0.580440.4.3.0.1.1.0.320.442.0j1j0j1.2.0....0...1c.1.64.serp..1.3.446...0j0i22i30k1.6wKmkctJdgk", fin[83,2]), status='warning', solidHeader = TRUE,
        img(src="Smurfit.jpg", height = 70, width = 170), width=2),
    box(title=a(href="https://corporate.sky.com/", fin[84,2]), status='warning', solidHeader = TRUE,
        img(src="sky.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.standardlife.com/dotcom/index.page", fin[85,2]), status='warning', solidHeader = TRUE,
        img(src="Standard.png", height = 70, width = 170), width=2),
    
    box(title=a(href="https://www.smiths.com/", fin[86,2]), status='warning', solidHeader = TRUE,
        img(src="Smiths.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.smith-nephew.com/", fin[87,2]), status='warning', solidHeader = TRUE,
        img(src="Sm.png", height = 70, width = 170), width=2),
    box(title=a(href="https://sse.co.uk/home", fin[88,2]), status='warning', solidHeader = TRUE,
        img(src="SSE.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.sc.com/en/", fin[89,2]), status='warning', solidHeader = TRUE,
        img(src="sch.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.sjp.co.uk/", fin[90,2]), status='warning', solidHeader = TRUE,
        img(src="st.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.stwater.co.uk/", fin[91,2]), status='warning', solidHeader = TRUE,
        img(src="seven.png", height = 70, width = 170), width=2),
    
    box(title=a(href="https://www.tescoplc.com/", fin[92,2]), status='warning', solidHeader = TRUE,
        img(src="te.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.tuigroup.com/de-de", fin[93,2]), status='warning', solidHeader = TRUE,
        img(src="tui.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.taylorwimpey.co.uk/", fin[94,2]), status='warning', solidHeader = TRUE,
        img(src="taylor.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.unilever.com/", fin[95,2]), status='warning', solidHeader = TRUE,
        img(src="uni.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.unitedutilities.com/", fin[96,2]), status='warning', solidHeader = TRUE,
        img(src="uu.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.vodafone.com/content/index.html", fin[97,2]), status='warning', solidHeader = TRUE,
        img(src="voda.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.wolseley.com/", fin[98,2]), status='warning', solidHeader = TRUE,
        img(src="wo.png", height = 70, width = 170), width=2),
    box(title=a(href="http://investors.worldpay.com/", fin[99,2]), status='warning', solidHeader = TRUE,
        img(src="wor.png", height = 70, width = 170), width=2),
    box(title=a(href="http://www.wpp.com/wpp/", fin[100,2]), status='warning', solidHeader = TRUE,
        img(src="wpp.png", height = 70, width = 170), width=2),
    box(title=a(href="https://www.whitbread.co.uk/homepage.html", fin[101,2]), status='warning', solidHeader = TRUE,
        img(src="whit.png", height = 70, width = 170), width=2)
    
  )
  
  
  

  playerPanel <-fluidPage(fluidRow(column(12, (DT::dataTableOutput("stat")
                                                                    )
                                                                   )
                                               )
  )
  
 
  #x <-isolate(input$izberi_stat)
  
  # observe({
  #   if (input$selectall > 0) {
  #     updateCheckboxGroupInput(session=session, inputId="izberi_stat",
  #                              choices = setNames(fin$Name, fin$Name),
  #                              selected = C())}
  #   
  # })
  
  
  output$head2head <- renderUI({ 
    input$goButton
    isolate({
    if (is.null(input$izberi_stat)) {
      out <- h2hPanel}
    else{
      out <- playerPanel
      #input$izberi_stat<-NULL
      }
    out})
    #dropDownizberi_stat =NULL
  })
  
  
  createLink <- function(val) {
    sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
  }

  })
