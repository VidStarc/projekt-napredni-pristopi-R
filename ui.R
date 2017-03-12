library(plotly)
library(shinythemes)
library(shinydashboard)
library(devtools)
library(xts)
library(dplyr)
library(googleVis)
library(dygraphs)
library(treemap)
#ADD8E6
#49C9BA

shinyUI(
  navbarPage(
    
    theme = shinytheme("flatly"),
            "Vizualizacija",
             tabPanel("Spremembe",
                      titlePanel("Primerjava sprememb cen delnic podjetij v FTSE 100",
                                 list(tags$head(tags$style("body {background-color: #ADD8E6; }")))),
                      br(),
                      fluidRow(
                        column(7,plotlyOutput("graf")),
                        column(5,wellPanel(tags$style(".well {background-color: springgreen;}"),
                        fluidRow(column(7,radioButtons("izbira","Tip spremembe cene",c("Vse","Samo pozitivne spremembe","Samo negativne spremembe")),
                               conditionalPanel(condition = "input.izbira == 'Samo negativne spremembe'",checkboxInput("neg1","večje od -1"))),
                               column(5,radioButtons("izbira2","Časovno obdobje spremembe",c("današnja","7 dnevna","30 dnevna","6 mesečna"))),
                               br(),br(),br(),br(),br(),br(),br(),br(),br(),
                               column(12,helpText("Za koliko se je spremenila cena delnice podjetja vključenega v indeks FTSE 100 v slednjem časovnem obdobju (v %)."))),
                        checkboxInput("prim","Primerjava podjetij"),
                        conditionalPanel(condition = "input.prim ",selectInput("podjetja","Podjetja",choices = price$Epic,multiple=TRUE)),
                        helpText("Izberi podjetja, ki jih želiš primerjati. Primerjava temelji na spremembi cen v določenem časovnem odbdobju. Tip spremembe cene pusti na: Vse, časovno obdobje lahko spreminjaš"))
                        )
                        ),
                      br(),
                      fluidRow(
                      column(7,plotOutput("hist")),
                      column(5,wellPanel(
                        sliderInput("bins","število stolpcev:",min=10,max=100,value=10),
                             checkboxInput("gostota","Prikaži gostoto",value=FALSE))))
                               
      
             ),
            tabPanel("Aplikacija-Neža",
    dashboardPage(
      skin="yellow",
      dashboardHeader(
        title="Playing with Google Analytics Data",
        titleWidth = 450
      ),
      dashboardSidebar(
        radioButtons("radio", "Choose visualization period:",
                     c("Last 3 months" = "90",
                       "Last 6 months" = "180"))
        #sidebarMenu(
        #)
      ),
      dashboardBody(
        # #boxes to be put in a row (or column)
        # fluidRow(
        #   valueBoxOutput(img(src="kljucneobrestnemere.jpg", height = 80, width = 80)),
        #   valueBoxOutput("goalBox"),
        #   valueBoxOutput("goalCRBox")
        # ),
        
        fluidRow(
          box(title=actionLink("compute", fin[1,2]), status='warning', solidHeader = TRUE,
              img(src="anglo-american-logo.png", height = 80, width = 160), width=4),
          box(title=actionLink("compute", fin[2,2]), status='warning', solidHeader = TRUE,
              img(src="Associated.png", height = 60, width = 150), width=4),
          box(title=actionLink("compute", fin[3,2]), status='warning', solidHeader = TRUE,
              img(src="admiral.jpg", height = 80, width = 150), width=4),
          box(title=actionLink("compute", fin[4,2]), status='warning', solidHeader = TRUE,
              img(src="ash.jpg", height = 80, width = 150), width=4),
          box(title=actionLink("compute", fin[5,2]), status='warning', solidHeader = TRUE,
              img(src="logo.png", height = 80, width = 150), width=4),
          box(title=actionLink("compute", fin[6,2]), status='warning', solidHeader = TRUE,
              img(src="aviva.png", height = 80, width = 150), width=4),
          box(title=actionLink("compute", fin[7,2]), status='warning', solidHeader = TRUE,
              img(src="astra.jpg", height = 75, width = 150), width=4),
          box(title=actionLink("compute", fin[8,2]), status='warning', solidHeader = TRUE,
              img(src="bae.png", height = 75, width = 170), width=4),
          box(title=actionLink("compute", fin[9,2]), status='warning', solidHeader = TRUE,
              img(src="bab.jpg", height = 75, width = 150), width=4),
          box(title=actionLink("compute", fin[10,2]), status='warning', solidHeader = TRUE,
              img(src="bar.jpg", height = 60, width = 170), width=4),
          box(title=actionLink("compute", fin[11,2]), status='warning', solidHeader = TRUE,
              img(src="britis.jpg", height = 60, width = 150), width=4),
      
          box(title=actionLink("compute", fin[12,2]), status='warning', solidHeader = TRUE,
              img(src="Barratt.jpg", height = 60, width = 170), width=4),
          box(title=actionLink("compute", fin[13,2]), status='warning', solidHeader = TRUE,
              img(src="prenos.jpg", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[14,2]), status='warning', solidHeader = TRUE,
              img(src="bhp.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[15,2]), status='warning', solidHeader = TRUE,
              img(src="bunzl.jpg", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[16,2]), status='warning', solidHeader = TRUE,
              img(src="bplogo.jpg", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[17,2]), status='warning', solidHeader = TRUE,
              img(src="burb.png", height = 60, width = 130), width=4),
          box(title=actionLink("compute", fin[18,2]), status='warning', solidHeader = TRUE,
              img(src="bt.jpg", height = 60, width = 160), width=4),
          
          box(title=actionLink("compute", fin[19,2]), status='warning', solidHeader = TRUE,
              img(src="coca-cola.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[20,2]), status='warning', solidHeader = TRUE,
              img(src="Carnival.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[21,2]), status='warning', solidHeader = TRUE,
              img(src="centrica.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[22,2]), status='warning', solidHeader = TRUE,
              img(src="Compass.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[23,2]), status='warning', solidHeader = TRUE,
              img(src="capita.jpg", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[24,2]), status='warning', solidHeader = TRUE,
              img(src="Croda.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[25,2]), status='warning', solidHeader = TRUE,
              img(src="CRH.jpg", height = 60, width = 160), width=4),
          
          box(title=actionLink("compute", fin[26,2]), status='warning', solidHeader = TRUE,
              img(src="conv.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[27,2]), status='warning', solidHeader = TRUE,
              img(src="dc.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[28,2]), status='warning', solidHeader = TRUE,
              img(src="DCC.png", height = 74, width = 160), width=4),
          box(title=actionLink("compute", fin[29,2]), status='warning', solidHeader = TRUE,
              img(src="Diageo.jpg", height = 74, width = 160), width=4),
   
          box(title=actionLink("compute", fin[30,2]), status='warning', solidHeader = TRUE,
              img(src="Direc.jpg", height = 74, width = 160), width=4),
          
          box(title=actionLink("compute", fin[31,2]), status='warning', solidHeader = TRUE,
              img(src="exp.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[32,2]), status='warning', solidHeader = TRUE,
              img(src="easy.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[33,2]), status='warning', solidHeader = TRUE,
              img(src="Fresnillo.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[34,2]), status='warning', solidHeader = TRUE,
              img(src="gkn.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[35,2]), status='warning', solidHeader = TRUE,
              img(src="glen.png", height = 60, width = 160), width=4),
          box(title=actionLink("compute", fin[36,2]), status='warning', solidHeader = TRUE,
              img(src="GSK.jpg", height = 60, width = 160), width=4),
          
          
          box(title=actionLink("compute", fin[37,2]), status='warning', solidHeader = TRUE,
              img(src="Hikma.jpg", height = 55, width = 160), width=4),
          box(title=actionLink("compute", fin[38,2]), status='warning', solidHeader = TRUE,
              img(src="ha.jpg", height = 75, width = 160), width=4),
          box(title=actionLink("compute", fin[39,2]), status='warning', solidHeader = TRUE,
              img(src="ham.png", height = 75, width = 160), width=4),
          box(title=actionLink("compute", fin[40,2]), status='warning', solidHeader = TRUE,
              img(src="hsbc.png", height = 64, width = 160), width=4),
          
          box(title=actionLink("compute", fin[41,2]), status='warning', solidHeader = TRUE,
              img(src="IAG.jpg", height = 50, width = 160), width=4),
          box(title=actionLink("compute", fin[42,2]), status='warning', solidHeader = TRUE,
              img(src="Inte.png", height = 68, width = 160), width=4),
          box(title=actionLink("compute", fin[43,2]), status='warning', solidHeader = TRUE,
              img(src="hsbc.png", height = 73, width = 160), width=4),
          box(title=actionLink("compute", fin[44,2]), status='warning', solidHeader = TRUE,
              img(src="hsbc.png", height = 60, width = 160), width=4),
          box(
            status="primary",solidHeader = TRUE,img(src="kljucneobrestnemere.jpg", height = 80, width = 80), width=2),
          box(
            status="primary",solidHeader = TRUE,img(src="kljucneobrestnemere.jpg", height = 80, width = 80))
          
  )
)
))))
