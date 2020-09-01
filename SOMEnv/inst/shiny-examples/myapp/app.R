##App077############### FUNZIONA (solo Run da R) !!!!!!!!!!!!!!!! Da 076 #

#ProjForPlot dipende da check52 (quindi si pu? passare avanti indietro a vedere i grafici del calcolo appena fatto)
#oppure di un Projection.RData caricato

require(shiny)
require(shinycssloaders)
require(shinycustomloader)

#---- Upload file fino a 100 MB consentito:

options(shiny.maxRequestSize = 100*1024^2) # per consentire upload files fino a 100 MB

#--------------------------- CSS STYLE -----------------------------------------

CSSHeader<-'font-family: "Palatino Linotype", "Book Antiqua", Palatino, serif;
font-size: 0.8em;
color: #000000;
text-decoration: none;
font-style: normal;
font-variant: normal;
text-transform: none'

CSS11<-"[type = 'number'] {font-size:1em; }
      [type = 'text'] {font-size:1em; }
#      [type = 'label'] { font-size:80%}
      "
CSS12<-'font-size:1em'
CSS13<-''
CSS14<-'height:1em;font-size:1em'
CSS15<-''
CSS16<-'overflow-x: scroll'
#------------------------------------------------------
CSS21<-"[type = 'number'] {font-size:1em; height:2em}
      [type = 'text'] {font-size:1em; height:2em}
#      [type = 'label'] { font-size:80%}
"
CSS22<-''
CSS23<-'font-size:1em'
CSS24<-'height:1em;font-size:1em'
CSS25<-''
CSS26<-'font-size:1em;overflow-x: auto;overflow-y: auto'
#------------------------------------------------------
CSS31<-"[type = 'number'] {font-size:1em; height:2em;}
      [type = 'text'] {font-size:1em; height:2em;}
#      [type = 'label'] { font-size:80%}
"
CSS32<-'font-size:1em'
CSS33<-'align:left;vertical-align: middle'
CSS331<-'font-size:1em;vertical-align: middle;align: center'
CSS34<-'font-size:1em;text-align: center'
CSS35<-'overflow-y: scroll'
#------------------------------------------------------
CSS41<-'overflow-x: scroll;overflow-y: scroll'
CSS42<-'font-size:12px;overflow-x: scroll;overflow-y: scroll'
#------------------------------------------------------
CSS51<-''

##########################################################################################################################################################
#
#                                                             PAGE DEFINITION
#
##########################################################################################################################################################

ui <-fluidPage(

  headerPanel(div(style=CSSHeader,"SOMEnv Graphical User Interface (v 0.0)"),windowTitle = "SOMEnv GUI"),

  div(style="background: white; width:40%;" , sidebarPanel(

               fluidRow(style='align:left;',
                        img(src='Logo.jpg', width = '95%')#---------------------------------------------------------------------------------------- DA RIVEDERE
               ),# fluidRow END

               fluidRow(style='overflow-y:scroll;background: #E3BAC6;',
                        h5("An R package dedicated to the analysis of multivariate environmental high frequency data
                  by Self-Organizing Map and k-means clustering algorithms"),
                        h5("Large datasets up to 100 MB are allowed!"),
                        h5("Authors: S. Licen, P. Barbieri"),
                        h5("Licence: GPL-2.0")
               ),# fluidRow END

               fluidRow(style='overflow-y:scroll;background: #FDE8E9;',
                        h4("Help"),
                        br(),
                        h5("Loading data"),
                        h5("Load data and check if the data are correctly loaded, the header is shown below")
               )# fluidRow END

  )),# END sidebarPanel

  div(style="width:100%; background: white" , mainPanel(

            tabsetPanel(
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
              #                                                                    FIRST TAB
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

              tabPanel("Load data", fluid = TRUE,
                       #              tags$style(CSS21),
                       fluidRow(width='80%', style='background: aliceblue;',
                                # Input: File input and help ----
                                column(5,div(style= 'font-size:1em',fileInput("Experimental", label="Load experimental data:", accept=c("txt"),buttonLabel=div(style= CSS24,"Browse..."),width="100%")),
                                       actionButton(inputId = "go11",label = "Load")
                                ),# column END
                                column(3,
                                       div(style= 'font-size:1em',textInput("text11", label="Date format:","%Y-%m-%d %H:%M:%S",width="100%")),
                                       div(style='font-size:1em; font-style:italic; align: left',"Variable date must be in the first column"),
                                       div(style='font-size:1em; font-style:italic; align: left',"See openair import function for date format input")
                                ),# column END
                                column(2,
                                       div(style= 'font-size:1em',textInput("text12", label="Separator:",",",width="100%")),
                                       div(style= 'font-size:1em',textInput("text13", label="Decimal:",".",width="100%")),
                                       div(style= 'font-size:1em',checkboxInput("check11", label="Header",value=TRUE))
                                )# column END
                       ),# fluidRow END

                       fluidRow(style='background: white;',
                                column(5,verticalLayout(h4("Experimental data header"),
                                                        div(style= CSS26,tableOutput("contents11")),
                                                        div(style= 'font-weight: bold',br()),
                                                        div(style= 'font-weight: bold',textOutput("texto11")),
                                                        div(style= 'font-weight: bold',textOutput("texto12")))),
                                column(5,verticalLayout(h4("Experimental data tail"),
                                                        div(style= CSS26,tableOutput("contents12")),
                                                        div(style= 'font-weight: bold; color:red',textOutput("texto13"))))
                       )# fluidRow END
              ), # tabPanel END
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
              #                                                                    SECOND TAB
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

              tabPanel("SOM training", fluid = TRUE,
                       #              tags$style(CSS21),
                       fluidRow(style='height:20vh;background: aliceblue;',
                                tags$style(type='text/css', ".selectize-input { padding: 1%; min-height: 0;} .selectize-dropdown { line-height: 1em; }"),
                                # Input: SOM map dimensions ----
                                uiOutput("UIinput21"),
                                column(2,div(style= 'font-size:1em',selectInput("Size", "Map size:", c("regular","small","big"), selected = "small",width="90%")),
                                       div(style= 'font-size:1em;font-weight: bold',textOutput("texto21"))),
                                # Input: SOM training parameters ----
                                uiOutput("UIinput22"),
                                # Input: SOM training parameters ----
                                column(2,div(style= 'font-size:1em;',selectInput("Neigh", "Neighborhood:", c("bubble","gaussian"), selected = "gaussian",width="90%")),
                                       uiOutput("UIinput23")),
                                column(2,div(style="padding-top:30%;",actionButton(inputId = "go21",label = "Train SOM")))
                       ),# fluidRow END

                       fluidRow(style='height:60vh;background: white;',
                                column(5,verticalLayout(h4("Codebook header"),
                                                        div(style= CSS26,tableOutput("contents21")))),
                                column(4,verticalLayout(h4("Summary"),
                                                        div(style= 'font-size:1em;',verbatimTextOutput("texto22",placeholder=TRUE)),
                                                        downloadButton("down21","Download SOM output")))
                       )# fluidRow END
              ), # tabPanel END
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
              #                                                                    THIRD TAB
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

              tabPanel("SOM Map", fluid = TRUE,
                       #tags$style(CSS31),
                       #tags$style(type='text/css', ".butt { min-height: 0;font-size:1em}"),

                       fluidRow(style='background: aliceblue;',
                                # Input: File input and help ----
                                column(5,div(style= 'font-size:1em',fileInput("SOMoutput", label="Load SOM output:", accept=c("RData"),buttonLabel=div(style= CSS24,"Browse..."),width="80%"),
                                       actionButton(inputId = "go31",label = "Load"))
                                ),# column END
                                column(5,div(style="padding-top:5%; ", splitLayout(cellWidths = c("20%","80%"),
                                                     h4("Summary:"),
                                                     div(style= 'font-size:1em;',verbatimTextOutput("texto31",placeholder=TRUE)))))
                       ),# fluidRow END

                      fluidRow(style='overflow-x:scroll;background:white;',
                                # Graphs: SOM map and cluster profiles ----
                                column(7,verticalLayout(splitLayout(cellWidths = c("20%","10%","10%","50%","10%"),
                                                                    h4("Heatmaps"),
                                                                    div(style="padding-top:10%;",actionButton(inputId = "go32",label = "Plot")),
                                                                    h4(""),
                                                                    uiOutput("UIinput31"),
                                                                    h4("")),
                                                        div(withSpinner(plotOutput("plot31"),type=6,color="green")),

                                                        splitLayout(cellWidths = c("10%","10%","5%","10%","10%","5%","25%","25%"),
                                                                    div(style= 'font-size:1em; font-weight: bold',"Height(in):"),
                                                                    div(style= CSS32,numericInput("h31", label=NULL, 5,width="100%")),
                                                                    h4(""),
                                                                    div(style= 'font-size:1em; font-weight: bold',"Width(in):"),
                                                                    div(style= CSS32,numericInput("w31", label=NULL, 7,width="100%")),
                                                                    h4(""),
                                                                    downloadButton("down30","download table",class="butt"),
                                                                    downloadButton("down31","download pdf",class="butt")
                                                        )
                                )),

                                column(3,div(verticalLayout(h4("SOM map features"),
                                                        div(withSpinner(plotOutput("plot32"),type=6,color="green"))
                                ))),
                                column(2,div(class="row", verticalLayout(h5("__________________"),
                                                        div(style= 'font-size:1em;',selectInput("menu31", label="Feature", c(" ","umat","hits","qerrs"), selected = " ",width="80%")),
                                                        uiOutput("UIinput32"),
                                                        div(style= 'font-size:1em; font-weight: bold',numericInput("h32", "Height(in):", 3,width="50%")),
                                                        div(style= 'font-size:1em; font-weight: bold',numericInput("w32", "Width(in):", 2,width="50%")),
                                                        downloadButton("down32","download pdf",class="butt"),
                                                        h5(""),
                                                        downloadButton("down33","download Hits bs",class="butt"),
                                                        h5(""),
                                                        downloadButton("down34","download Qerrs bs",class="butt")
                                )))
                       )# fluidRow END
              ), # tabPanel END

              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
              #                                                                    FOURTH TAB
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------


              tabPanel("Kmeans clustering", fluid = TRUE,
                       #              tags$style(CSS31),
                       fluidRow(style='background: aliceblue;',
                                # Input: File input and help ----
                                column(4,
                                       div(style= 'font-size:1em; font-weight: bold',numericInput("n41", "Max clusters:", 8,min=2,width="80%")),
                                       div(style= 'font-size:1em; font-weight: bold',numericInput("n42", "Iterating times (x 100 epochs):", 5,min=1,width="80%")) ,
                                       div(splitLayout(cellWidths = c("25%","90%"),
                                                   actionButton(inputId = "go41",label = "Run"),
                                                   downloadButton("down41","Download Kmeans output",class="butt"))),


                                div(column(4,verticalLayout(
                                  splitLayout(cellWidths = c("100%","80%"),
                                              div(style= 'font-size:1em',radioButtons(inputId = "choice41",label = "Set seed?",choices=c("Yes","No"),selected="No",inline=TRUE,width="100%")),
                                              conditionalPanel(condition = "input.choice41 == 'Yes'",
                                                               div(style= 'font-size:1em',numericInput("Seed", "", value=7,width="100%"))))))),

                                ),# column END

                                div(column(6,verticalLayout(splitLayout(cellWidths = c("25%","55%","90%"),
                                                                    checkboxInput(inputId = "check41",label = "Load file?",value=FALSE),
                                                                    conditionalPanel(condition = "input.check41 == 1",
                                                                                     div(style= 'font-size:1em',fileInput("KmeansOutput", label="Load Kmeans output:", accept=c("RData"),buttonLabel=div(style= CSS24,"Browse..."),width="90%"))),
                                                                    conditionalPanel(condition = "input.check41 == 1",
                                                                                     div(style=CSS33,actionButton(inputId = "go42",label = div(style=CSS331,"Load")))),
                                                                    uiOutput("UIinput41")), # splitLayout END
                                                        div(style='font-size:1em; font-style:italic; color:red',"(Remember to load SOM output in SOM map tab!)"),
                                                        uiOutput("UIinput42")
                                )# verticalLayout END
                                ))# column END
                       ),# fluidRow END

                       fluidRow(style='width:100%; overscroll-x:auto; background: white;',
                                # Input: File input and help ----
                                div(column(3,verticalLayout(
                                  h4("DB-index plot"),plotOutput("plot41"))
                                ),# column END
                                column(3,verticalLayout(h4("SOM map"),
                                                        withSpinner(plotOutput("plot42"),type=5,color="gray")
                                )),
                                column(6,verticalLayout(fluidRow(column(5,h4("Cluster profiles")),
                                                                 column(5,div(style= 'font-size:1em',selectInput("menu41", label=NULL, c("by cluster","by variable","Exp by cluster","Exp by variable"), selected = "by cluster",width="90%")))),
                                                        withLoader(plotOutput("plot43"),type="text",loader=list(marquee("Calculating...",
                                                                                                                        direction="down", behavior = "scroll",width = "90%",style="font-size:15px; font-weight: bold; color:gray")))
                                ))
                       )),# fluidRow END

                       fluidRow(style='height:5vh;background: white;',
                                # Input: File input and help ----
                                div(column(4,splitLayout(cellWidths = c("10%","15%","10%","15%","5%","20%"),
                                                     h4(style= 'font-size:1em; font-weight: bold',"H(in):"),
                                                     numericInput("h41", label=NULL, 5),
                                                     h4(style= 'font-size:1em; font-weight: bold',"W(in):"),
                                                     numericInput("w41", label=NULL, 7),
                                                     h4(""),
                                                     downloadButton("down42","pdf",class="butt")
                                )
                                )),
                                div(column(1,splitLayout(
                                                       downloadButton("down43","pdf",class="butt")
                                                     )
                                )),
                                uiOutput("UIinput43")

                       )# fluidRow END
              ), # tabPanel END



              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
              #                                                                    FIFTH TAB
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

              tabPanel("Projection", fluid = TRUE,
                       #              tags$style(CSS31),
                       fluidRow(style='height:30vh;background: aliceblue;',
                                # Input: File input and help ----
                                column(3,div(style= 'font-size:12px',fileInput("Other", label="Load dataset:", accept=c("txt"),buttonLabel=div(style= CSS24,"Browse..."),width="100%")),
                                       fluidRow(column(4,actionButton(inputId = "go51",label = "Load")),
                                                column(8,div(style='font-size:12px; font-style:italic; color:red',"(Remember to load SOM output in SOM map tab!)"))),
                                       div(style= 'font-size:10px;font-weight: bold',br()),
                                       div(style= 'font-size:12px;font-weight: bold',textOutput("texto51")),
                                       div(style= 'font-size:12px;font-weight: bold',textOutput("texto52"))
                                ),# column END
                                column(2,
                                       div(style= 'font-size:12px',textInput("text51", label="Date format:","%Y-%m-%d %H:%M:%S",width="100%")),
                                       div(style='font-size:12px; font-style:italic; align: left',"Variable date must be in the first column"),
                                       div(style='font-size:12px; font-style:italic; align: left',"See openair import function for date format input")
                                ),# column END
                                column(1,
                                       div(style= 'font-size:12px',textInput("text52", label="Separator:",",",width="100%")),
                                       div(style= 'font-size:12px',textInput("text53", label="Decimal:",".",width="100%")),
                                       div(style= 'font-size:12px',checkboxInput("check51", label="Header",value=TRUE))
                                ),# column END
                                column(style='background: white',3,verticalLayout(div(style= 'font-size:12px; font-weight: bold',"Dataset header"),
                                                                                  div(style= 'font-size:11px;overflow: auto;height:25vh',tableOutput("contents51")))
                                ),# column END
                                column(style='background: white',3,verticalLayout(div(style= 'font-size:12px; font-weight: bold',"Dataset tail"),
                                                                                  div(style= 'font-size:11px;overflow: auto;height:25vh',tableOutput("contents52")))
                                ),# column END
                       ),# fluidRow END

                       fluidRow(style='height:60vh;background: white;',

                                column(3,style='background: aliceblue',verticalLayout(actionButton(inputId = "go52",label = "Projection"),
                                                                                      h4("Summary"),
                                                                                      div(style= 'font-size:12px;',verbatimTextOutput("texto53",placeholder=TRUE)),
                                                                                      downloadButton("down51","Download Projection",class="butt"),
                                                                                      br(),
                                                                                      checkboxInput(inputId = "check52",label = "Load projection?",value=FALSE),
                                                                                      conditionalPanel(condition = "input.check52== 1",
                                                                                                       div(style= 'font-size:12px',fileInput("OtherOutput", label="Load Projection file:", accept=c("RData"),buttonLabel=div(style= CSS24,"Browse..."),width="90%"))),
                                                                                      conditionalPanel(condition = "input.check52== 1",
                                                                                                       div(style=CSS33,actionButton(inputId = "go53",label = div(style=CSS331,"Load")))),
                                                                                      conditionalPanel(condition = "input.check52== 1",br(),
                                                                                                       div(style= 'font-size:12px;',verbatimTextOutput("texto54",placeholder=TRUE)))
                                )),
                                column(2,verticalLayout(h4("SOM (projection)"),
                                                        div(withSpinner(plotOutput("plot51"),type=6,color="green"))
                                )),
                                column(2,verticalLayout(h5("__________________"),
                                                        div(style= 'font-size:12px;',selectInput("menu51", label="Feature", c(" ","hits","qerrs"), selected = " ",width="80%")),
                                                        uiOutput("UIinput51"),
                                                        div(style= 'font-size:12px; font-weight: bold',numericInput("h51", "Height(in):", 3,width="80%")),
                                                        div(style= 'font-size:12px; font-weight: bold',numericInput("w51", "Width(in):", 2,width="80%")),
                                                        downloadButton("down52","download pdf",class="butt"),
                                                        h5(""),
                                                        downloadButton("down53","download Hits bs",class="butt"),
                                                        h5(""),
                                                        downloadButton("down54","download Qerrs bs",class="butt")
                                )),
                                column(5,verticalLayout(fluidRow(column(5,h4("Clusters (projection)")),
                                                                 column(5,div(style= 'font-size:12px',selectInput("menu53", label=NULL, c("Proj by cluster","Proj by variable"), selected = "Proj by cluster",width="90%")))),
                                                        withLoader(plotOutput("plot52"),type="text",loader=list(marquee("Calculating...",
                                                                                                                        direction="down", behavior = "scroll",width = "90%",style="font-size:15px; font-weight: bold; color:gray")))),
                                       downloadButton("down55","download pdf",class="butt"))

                       )# fluidRow END

              ), # tabPanel END


              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
              #                                                                    SIXTH TAB
              #---------------------------------------------------------------------------------------------------------------------------------------------------------------------


              tabPanel("Daily profiles", fluid = TRUE,
                       tags$style(CSS31), # Non toccare!!! Se viene tolto salta formattazione in tab 3 e altre parti (mah...)
                       fluidRow(style='height:25vh;background: aliceblue;',
                                # Input: File input and help ----
                                column(3,div(style= 'font-size:1em;',selectInput("menu61", label="Dataset", c("training","projection"), selected = "training",width="80%")),
                                       uiOutput("UIinput63")
                                ),# column END
                                column(3,
                                       uiOutput("UIinput61")
                                ),# column END
                                column(4,
                                       uiOutput("UIinput62"),
                                       verbatimTextOutput("texto61", placeholder = TRUE)
                                )# column END
                       ),# fluidRow END
                       fluidRow(style='height:60vh;background: white;',
                                column(10,verticalLayout(splitLayout(cellWidths = c("66%","3%","14%","17%"),
                                                                    h4("Daily graph"),
                                                                    h4(""),
                                                                    div(style= 'font-size:1em; font-weight: bold',"Xlab dim:"),
                                                                    div(style= CSS32,numericInput("x61", label=NULL, 0.75,width="100%"))),
                                                        div(withSpinner(plotOutput("plot61"),type=6,color="green")),
                                                        splitLayout(cellWidths = c("7%","12%","7%","12%","9%","11%","3%","19%","20%"),
                                                                    div(style= 'font-size:1em; font-weight: bold',"H(in):"),
                                                                    div(style= CSS32,numericInput("h61", label=NULL, 5,width="100%")),
                                                                    div(style= 'font-size:1em; font-weight: bold',"W(in):"),
                                                                    div(style= CSS32,numericInput("w61", label=NULL, 7,width="100%")),
                                                                    downloadButton("down61","pdf",class="butt"),
                                                                    downloadButton("down62","table",class="butt"),
                                                                    h4(""),
                                                                    downloadButton("down63","overall tab",class="butt"),
                                                                    downloadButton("down64","monthly tab",class="butt")
                                                        ))
                                ),# column END
                                column(6,verticalLayout(
                                  h4("Sample profile"),
                                  plotOutput("plot62")),
                                  downloadButton("down65","pdf",class="butt")
                                )# column END
                       )# fluidRow END


              ) # tabPanel END






              #-----------------------------------------
            ) # tabsetPanel END
  )) # mainPanel END
) # pageWithSidebar END

##########################################################################################################################################################
#
#                                                            INPUT & OUTPUT
#
##########################################################################################################################################################

# Define server logic to read selected file ----

server <- function(input, output,session) {

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #                                                                server: FIRST TAB
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

  #------Da qui richiamo input:

  Header<-reactive({if (input$check11==TRUE) {1} else {NA}})
  Experimental <- eventReactive(input$go11,{import(input$Experimental$datapath,date="date",date.format=input$text11,
                                                   file.type = "txt",header = Header(),sep = input$text12,dec=input$text13)})
  #--- Elimination of NA values:

  ExpClean<-reactive({na.omit(data.frame(Experimental()))})


  #------Da qui output:


  output$contents11 <- renderTable({head(data.frame(date=as.character(ExpClean()[,1]),ExpClean()[,-1]))},spacing="xs")
  output$contents12 <- renderTable({tail(data.frame(date=as.character(ExpClean()[,1]),ExpClean()[,-1]))},spacing="xs")

  output$texto11<-renderText({paste("Number of uploaded rows =",as.character(nrow(ExpClean())))})
  output$texto12<-renderText({paste("Number of deleted NA rows =",as.character(nrow(Experimental())-nrow(ExpClean())))})

  CheckNum<-reactive({sum(sapply(ExpClean()[,c(2:ncol(ExpClean()))], is.numeric))==length(c(2:ncol(ExpClean())))})

  output$texto13<-renderText({if (CheckNum()==FALSE){
    paste0("WARNING: one or more variables are non-numeric, check your data!")}
  })

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #                                                                server: SECOND TAB
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------


  #--- Centering and scaling of experimental data:
  prepare<-reactive({scale(ExpClean()[,-1],center = TRUE,scale=TRUE)})

  CenterVal<-reactive({attr(prepare(),"scaled:center")})
  ScaleVal<-reactive({attr(prepare(),"scaled:scale")})

  dataset<-reactive({prepare()}) # matrice da usare per il calcolo della SOM

  #--- Default SOM map dimensions:

  DimsD<-reactive({som_dimR(dataset(),type=input$Size)}) #QUESTO dipende da un input, quindi non serve "Restore Button", basta ricambiare "Size" e si torna ai default!!!!

  output$UIinput21 <- renderUI({if(input$go11==0) {
    tagList(
      column(3,div(style= 'font-size:1em',textInput("txtfake21", "Number of SOM map rows:", value="Waiting for file input...",width="100%")),
             div(style= 'font-size:1em',textInput("txtfake22", "Number of SOM map cols:", value="Waiting for file input...",width="100%"))))
  } else {
    tagList(
      column(3,div(style= 'font-size:1em',numericInput("Row", "Number of SOM map rows:", value=DimsD()$Row,min=2,width="90%")),
             div(style= 'font-size:1em',numericInput("Col", "Number of SOM map cols:", value=DimsD()$Col,min=2,width="90%"))))}
  })


  #--- SOM map dimensions evaluated from inputs:

  Dims<-reactive({list(Row=input$Row,Col=input$Col,munits=input$Row*input$Col)})

  output$texto21<-renderText({paste("Number of units =",as.character(Dims()$munits))})

  #--- Parameters evaluated from inputs:

  Init<-reactive({som_initR(dataset(),Dims()$Row,Dims()$Col,Dims()$munits)})
  dlen<-reactive({nrow(dataset())})
  mpd<-reactive({Dims()$munits/dlen()})

  #--- Default parameters evaluated from inputs:

  EpochsD<-reactive({max(2,(ceiling(mpd()*10)+ceiling(mpd()*40)))}) # da rough+finetune phase by Vesanto
  radMaxD<-reactive({max(1,ceiling(max(c(Dims()$Row,Dims()$Col,na.rm=T))/8))}) # da finetune phase by Vesanto
  RadDtext<-reactive({paste(radMaxD(),",",1,sep="")})

  output$UIinput22 <- renderUI({tagList(
    column(2,div(style= 'font-size:1em',numericInput("Epochs", "Number of epochs:", value=EpochsD(),min=2,width="100%")))
  )})

  output$UIinput23 <- renderUI({tagList(
    div(style= 'font-size:1em',textInput("Rad", "Neigh.radius:", RadDtext(),width="100%"))
  )})

  #--- Parameters rearranged from inputs:

  Rad<-reactive({as.numeric(unlist(strsplit(input$Rad, ',')))})


  ###------------- SOM MAKE

  #------- SOM grid

  som_grid <- eventReactive(input$go21,{somgrid(xdim = input$Row, ydim=input$Col, topo="hexagonal", toroidal = FALSE,neighbourhood.fct = input$Neigh)})

  #--- SOM training:

  som_model <- eventReactive(input$go21,{som(dataset(),
                                             grid=som_grid(),
                                             radius=Rad(),
                                             rlen=input$Epochs,
                                             init=Init(),
                                             dist.fcts="euclidean", #fixed
                                             mode="pbatch",#fixed
                                             keep.data = F)})

  #--- Training data projection:

  Training<-reactive({map(som_model(), dataset())})

  Bmus<-reactive({Training()$unit.classif})
  Qerrs<-reactive({Training()$distances})

  #--- Hits

  BmusFreq<-reactive({data.frame(table(Bmus()))})
  Hits<-reactive({BmusFreq()$Freq})

  #--- Denormalized codebook

  codebook<-reactive({data.frame(som_model()$codes)})
  DeCod <- reactive({data.frame(t(apply(codebook(), 1, function(r)r*ScaleVal()+ CenterVal())))})

  #--- Prototype x and y coordinates
  Coord<-eventReactive(input$go21,{CodeCoord(input$Row,input$Col)})

  #--- Umatrix

  Umat<-reactive({som_umatR(codebook(), input$Row,input$Col)})


  #--- Output:

  output$contents21 <- renderTable({head(codebook())},spacing="xs")
  output$texto22<-renderPrint({
    cat("SOM of size ", Dims()$Row, "x", Dims()$Col, sep = "")
    cat("\n Number of samples used for training: ",nrow(ExpClean()),sep = "")
    cat("\n Number of prototypes(units): ",nrow(codebook()),sep = "")
    cat("\n Topology: hexagonal",sep = "")
    cat("\n Neighbourhood function: ",input$Neigh, sep = "")
    cat("\n Distance measure used: Euclidean",sep = "")
    cat("\n Learning algorithm: pbatch",sep = "")
    cat("\n Number of training epochs: ",input$Epochs,sep = "")
  })


  #--- Save list:

  OutputRdata<-reactive({list(ExpClean=ExpClean(),dataset=dataset(),ScaleVal=ScaleVal(),CenterVal=CenterVal(),Epochs=input$Epochs,som_model=som_model(),
                              Neigh=input$Neigh,Dims=Dims(),Coord=Coord(),codebook=codebook(),DeCod=DeCod(),Umat=Umat(),Hits=Hits(),Bmus=Bmus(),Qerrs=Qerrs())})


  output$down21<-downloadHandler(filename=function() {paste0("SOM_output_",format(Sys.time(),format="%Y-%m-%d_%H:%M"),".RData")},
                                 content = function(file) {
                                   SOMoutput<-OutputRdata()
                                   list.save(SOMoutput,file=file)}
  )

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #                                                                server: THIRD TAB
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

  SOMoutput <- eventReactive(input$go31,{list.load(input$SOMoutput$datapath)})

  output$texto31<-renderPrint({
    cat("SOM of size ", SOMoutput()$Dims$Row, "x", SOMoutput()$Dims$Col, sep = "")
    cat("\n Number of samples used for training: ",nrow(SOMoutput()$ExpClean),sep = "")
    cat("\n Number of variables: ",ncol(SOMoutput()$codebook),sep = "")
    cat("\n Neighbourhood function: ",SOMoutput()$Neigh, sep = "")
    cat("\n Number of training epochs: ",SOMoutput()$Epochs,sep = "")
  })


  #-- Heatmaps

  MxRow<-eventReactive(input$go31,{if (ncol(SOMoutput()$codebook)<=14) {2} else {3} })

  MxCol<-eventReactive(input$go31,{
    if(ncol(SOMoutput()$codebook)%%MxRow()==0) {ncol(SOMoutput()$codebook)%/%MxRow()
    } else {ncol(SOMoutput()$codebook)%/%MxRow()+1}
  })

  output$UIinput31 <- renderUI({tagList(splitLayout(style= 'overflow-x: hidden',cellWidths = c("25%","25%","25%","25%"),
                                                    div(style= 'font-size:1em; font-weight: bold',"Map x row:"),
                                                    div(style= CSS32,numericInput("n31", label=NULL, MxRow(),width="80%")),
                                                    div(style= 'font-size:1em; font-weight: bold',"Map x col:"),
                                                    div(style= CSS32,numericInput("n32", label=NULL, MxCol(),width="80%"))
  )
  )})


  Table30<-function() {SumTable<-paramQuant(SOMoutput()$DeCod[,1]); ##################################----- de-normalized!
  for (i in c(2:ncol(SOMoutput()$DeCod))) {
    P<-paramQuant(SOMoutput()$DeCod[,i]);SumTable<-cbind(SumTable,P[,2])}
  colnames(SumTable)<-c("Statistic",colnames(SOMoutput()$DeCod))
  SumTable<-data.frame(SumTable)
  return(SumTable)
  }

  output$down30 <- downloadHandler(filename ="Codebook_basic statistics.txt",
                                   content = function(file) {
                                     write.table(Table30(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )

  Plot31<- function() {if (is.null(input$SOMoutput)) {return(NULL)
  } else {HexagonsVar(c(input$n31,input$n32),SOMoutput()$codebook,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)}}

  output$plot31<- renderPlot({if (input$go32[[1]] == 0) return(); Plot31()})


  output$down31 <- downloadHandler(filename ="Heatmaps.pdf",
                                   content = function(file) {
                                     pdf(file,height=input$h31,width=input$w31)
                                     Plot31()
                                     dev.off()}
  )

  #-- SOM map features

  output$UIinput32 <- renderUI({if(input$menu31==" ") {
    tagList(div(style= 'font-size:1em;',selectInput("menu32", label="Plot type", c(" "), selected = " ",width="80%")))
  } else if (input$menu31=="umat"){tagList(div(style= 'font-size:12px;',selectInput("menu32", label="Plot type", c("bw","gs"), selected = "gs",width="80%")))
  } else if (input$menu31=="hits" | input$menu31=="qerrs"){tagList(div(style= 'font-size:1em;',
                                                                       selectInput("menu32", label="Plot type", c("grayscale","black filling"), selected = "grayscale",width="80%")))}
  })

  Switch3<-reactive({paste0(input$menu31,input$menu32)})

  Plot32<- function() {switch(Switch3(),
                              "  "={return(NULL)},
                              "umatgs"={UmatGraph(SOMoutput()$Umat,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col,colorscale="gs")},
                              "umatbw"={UmatGraph(SOMoutput()$Umat,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col,colorscale="bw")},
                              "hitsgrayscale"={HexaHitsQuant(SOMoutput()$Hits,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)},
                              "hitsblack filling"={HexaHits(SOMoutput()$Hits,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)},
                              "qerrsgrayscale"={HexaQerrsQuant(SOMoutput()$Bmus,SOMoutput()$Qerrs,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)},
                              "qerrsblack filling"={HexaQerrs(SOMoutput()$Bmus,SOMoutput()$Qerrs,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)})
  }

  output$plot32<- renderPlot({Plot32()})


  output$down32 <- downloadHandler(filename =function() {paste0("SOM_",input$menu31,"_",input$menu32,".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h32,width=input$w32)
                                     Plot32()
                                     dev.off()}
  )
  #-- Basic statistics

  Table31<-function() {paramQuant(SOMoutput()$Hits)}

  output$down33 <- downloadHandler(filename ="Hits_basic statistics.txt",
                                   content = function(file) {
                                     write.table(Table31(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )

  Table32<-function() {paramQuant(SOMoutput()$Qerrs)}

  output$down34 <- downloadHandler(filename ="Qerrs_basic statistics.txt",
                                   content = function(file) {
                                     write.table(Table32(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )

  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #                                                                server: FOURTH TAB
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------


  # su dati caricati in Third Tab!!!!

  Switch41<-function() {switch(input$choice41,
                               "Yes"={paste0("Yes")},
                               "No"={paste0("No")})
  }

  Kmeans <- eventReactive(input$go41,{if (Switch41()=="No"){kmeans_clustersRProg(SOMoutput()$codebook,k=input$n41,times=input$n42)
  } else {kmeans_clustersRProg(SOMoutput()$codebook,k=input$n41,times=input$n42,seed=input$Seed) }
  })

  #-- Risultati aggiuntivi:

  BCentr<-reactive({BmusCentr(Kmeans()$centroids,SOMoutput()$som_model,input$n41)})

  ClusExp<-function() {D<-rep(0,nrow(SOMoutput()$ExpClean))
  for (i in c(2:input$n41)) {d<-BmusClus(SOMoutput()$Bmus,Kmeans()$clusNum[,i]);
  D<-cbind(D,d)}; return(data.frame(D))}

  #-- Output:

  OutputKdata<-reactive({list(centroids=Kmeans()$centroids,clusNum=data.frame(Kmeans()$clusNum),ind=Kmeans()$ind,
                              err=Kmeans()$err,seed=Kmeans()$seed,BCentr=BCentr(),ClusExp=ClusExp())})

  output$down41<-downloadHandler(filename=function() {paste0("Kmeans_output_",format(Sys.time(),format="%Y-%m-%d_%H:%M"),".RData")},
                                 content = function(file) {
                                   KmeansOutput<-OutputKdata()
                                   list.save(KmeansOutput,file=file)}
  )
  #-- If KmeansOutput file is loaded:

  Kmeansoutput <- eventReactive(input$go42,{list.load(input$KmeansOutput$datapath)})

  KmeansForPlot<-reactive({if(input$check41==FALSE){OutputKdata()} else {Kmeansoutput()} })


  #-- Aggiornamento input in base a cluster migliore:

  BestClus<-reactive({if(is.null(KmeansForPlot())) {5} else {which(KmeansForPlot()$ind==min(KmeansForPlot()$ind,na.rm=T))+1}})

  output$UIinput42 <- renderUI({tagList(
    fluidRow(
      column(2,div(style= 'font-size:1em',numericInput("n43", label="N.clusters:", BestClus(),min=2,width="80%"))),
      column(1,colourInput("col40", label=div(style= CSS34,"Cl 1"), "red",showColour="background",returnName=T,palette="limited")),
      column(1,colourInput("col41", label=div(style= CSS34,"Cl 2"), "yellow",showColour="background",returnName=T,palette="limited")),
      conditionalPanel("input.n43>=3",column(1,colourInput("col42", label=div(style= CSS34,"Cl 3"), "green",showColour="background",returnName=T,palette="limited"))),
      conditionalPanel("input.n43>=4",column(1,colourInput("col43", label=div(style= CSS34,"Cl 4"), "cyan",showColour="background",returnName=T,palette="limited"))),
      conditionalPanel("input.n43>=5",column(1,colourInput("col44", label=div(style= CSS34,"Cl 5"), "darkorange1",showColour="background",returnName=T,palette="limited"))),
      conditionalPanel("input.n43>=6",column(1,colourInput("col45", label=div(style= CSS34,"Cl 6"), "magenta",showColour="background",returnName=T,palette="limited"))),
      conditionalPanel("input.n43>=7",column(1,colourInput("col46", label=div(style= CSS34,"Cl 7"), "aquamarine",showColour="background",returnName=T,palette="limited"))),
      conditionalPanel("input.n43>=8",column(1,colourInput("col47", label=div(style= CSS34,"Cl 8"), "sandybrown",showColour="background",returnName=T,palette="limited"))),
      column(2,div(style=CSS33,actionButton(inputId = "go43",label = div(style=CSS331,"Plot"))))
    ) # fluidRow END
  )})


  TextClus<-reactive({if(is.null(KmeansForPlot())) {paste(c(1:5),collapse=",",sep="")} else {paste(c(1:input$n43),collapse=",",sep="")}})

  output$UIinput41 <- renderUI({tagList(
    div(style= CSS32,textInput(inputId = "text41",label = "Cluster numbers:",value=TextClus(),width="90%"))
  )})

  #-- Output DB-index plot:



  Plot41<-function(){if (is.null(KmeansForPlot())) {return(NULL)
  } else {
    par(mar=c(4,3,1,1),oma=c(1,1,1,1))
    plot(c(2:(length(KmeansForPlot()$ind)+1)),KmeansForPlot()$ind,col="gray",xlim=c(1,length(KmeansForPlot()$ind)+2),type="h",
         ylim=c(min(KmeansForPlot()$ind,na.rm=T)-0.05*min(KmeansForPlot()$ind,na.rm=T),max(KmeansForPlot()$ind,na.rm=T)+0.05*max(KmeansForPlot()$ind,na.rm=T)),
         xlab="Cluster number",ylab="DB-index",xaxt="n")
    text(c(2:(length(KmeansForPlot()$ind)+1)),KmeansForPlot()$ind,round(KmeansForPlot()$ind,digit=2),cex=0.75,pos=3)
    axis(1,at=seq(2,length(KmeansForPlot()$ind)+1,1),labels=NA,tcl=-0.3,cex.axis=0.6)
    axis(1,at=seq(2,length(KmeansForPlot()$ind)+1,1),labels=seq(2,length(KmeansForPlot()$ind)+1,1),lwd=0,line=-0.2,cex.axis=1)}
  }

  output$plot41<-renderPlot({if (is.null(KmeansForPlot())) {return(NULL)
  } else {Plot41()}})


  output$down42 <- downloadHandler(filename =function() {paste0("DB-index_plot",".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h41,width=input$w41)
                                     Plot41()
                                     dev.off()}
  )


  #-- Output SOM cluster map plot:

  colSeq4<-reactive({rep_len(c(input$col40,input$col41,input$col42,input$col43,input$col44,input$col45,
                               input$col46,input$col47), input$n43)})

  numSeq41<-reactive({as.numeric(unlist(strsplit(input$text41, ',')))})
  numSeq42<-reactive({as.numeric(unlist(strsplit(input$text42, ',')))})
  numSeq43<-reactive({c(input$y41,input$y42)})


  Centroids4<-reactive({data.frame(KmeansForPlot()$centroids[[input$n43]][order(numSeq41()),])})
  clusNum4<-reactive({NClusChange(KmeansForPlot()$clusNum[,input$n43],numSeq41())})
  BCentr4<-reactive({KmeansForPlot()$BCentr[[input$n43]][order(numSeq41())]})
  ClusExp4<-reactive({NClusChange(KmeansForPlot()$ClusExp[,input$n43],numSeq41())})


  Plot42<- function() {if (is.null(KmeansForPlot())) {return(NULL)
  } else if (is.null(SOMoutput())) {return(NULL)
  } else if (length(numSeq41())!=input$n43) {return(NULL)
  } else {HexagonsClus(Centroids4(), clusNum4(), BCentr4(),
                       SOMoutput()$Coord, SOMoutput()$Dims$Row, SOMoutput()$Dims$Col, colSeq = colSeq4())}
  }


  output$plot42<-renderPlot({if (is.null(KmeansForPlot())|input$go43==0 ) {return(NULL)
  } else if (is.null(SOMoutput())) {return(NULL)
  } else {Plot42()}
  })


  output$down43 <- downloadHandler(filename =function() {paste0("SOM map",".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h41,width=input$w41)
                                     Plot42()
                                     dev.off()}
  )

  #-- Output cluster profiles plot:


  output$UIinput43 <- renderUI({if(input$menu41=="by cluster"| input$menu41=="Exp by cluster") {
    tagList(
      column(7,splitLayout(cellWidths = c("8%","7%","10%","7%","10%","7%","12%","10%","12%","20%"),
                           h5(""),
                           div(style= 'font-size:1em; font-weight: bold',"Y min:"),
                           div(style= CSS32,numericInput("y41", label=NULL, -3,width="100%")),
                           div(style= 'font-size:1em; font-weight: bold',"Y max:"),
                           div(style= CSS32,numericInput("y42", label=NULL, 3,width="100%")),
                           div(style= 'font-size:1em; font-weight: bold',"Y grid:"),
                           div(style= CSS32,textInput(inputId = "text42",label=NULL,value="-1,0,1",width="100%")),
                           div(style= 'font-size:1em; font-weight: bold',"Xlab dim:"),
                           div(style= CSS32,numericInput("x41", label=NULL, 0.75,width="100%")),
                           downloadButton("down44","pdf",class="butt")))
    )
  } else if(input$menu41=="by variable"| input$menu41=="Exp by variable"){
    tagList(
      column(7,splitLayout(cellWidths = c("15%","15%","15%","15%","15%","25%"),
                           h5(""),
                           div(style= 'font-size:1em; font-weight: bold',"Map x row:"),
                           div(style= CSS32,numericInput("n44", label=NULL, MxRow(),width="80%")),
                           div(style= 'font-size:1em; font-weight: bold',"Map x col:"),
                           div(style= CSS32,numericInput("n45", label=NULL, MxCol(),width="80%")),
                           downloadButton("down44","pdf",class="butt")))
    )}
  })


  Plot43<- function() {if (is.null(KmeansForPlot())) {return(NULL)
  } else if (is.null(SOMoutput())) {return(NULL)
  } else if (length(numSeq41())!=input$n43) {return(NULL)
  } else {switch(input$menu41,
                 "by cluster"={BoxUnits(SOMoutput()$codebook,clusNum4(),Ylim=numSeq43(),pitch=numSeq42(),xdim=input$x41)},
                 "by variable"={BoxClus(c(input$n44,input$n45),SOMoutput()$DeCod,clusNum4())},
                 "Exp by cluster"={BoxUnits(SOMoutput()$dataset,ClusExp4(),Ylim=numSeq43(),pitch=numSeq42(),xdim=input$x41)},
                 "Exp by variable"={BoxClus(c(input$n44,input$n45),SOMoutput()$ExpClean[,-1],ClusExp4())})}
  }

  output$plot43<-renderPlot({if (is.null(KmeansForPlot())|input$go43==0 ) {return(NULL)
  } else if (is.null(SOMoutput())) {return(NULL)
  } else {Plot43()}
  })

  output$down44 <- downloadHandler(filename =function() {paste0("Cluster profiles",".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h41,width=input$w41)
                                     Plot43()
                                     dev.off()}
  )



  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #                                                                server: FIFTH TAB
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------

  #------Da qui richiamo input Other:

  HeaderO<-reactive({if (input$check51==TRUE) {1} else {NA}})
  Other <- eventReactive(input$go51,{import(input$Other$datapath,date="date",date.format=input$text51,
                                            file.type = "txt",header = HeaderO(),sep = input$text52,dec=input$text53)})
  #--- Elimination of NA values:

  OtherClean<-reactive({na.omit(data.frame(Other()))})


  #------Da qui output:
  output$contents51 <- renderTable({head(data.frame(date=as.character(OtherClean()[,1]),OtherClean()[,-1]))},spacing="xs")
  output$contents52 <- renderTable({tail(data.frame(date=as.character(OtherClean()[,1]),OtherClean()[,-1]))},spacing="xs")

  output$texto51<-renderText({paste("Number of uploaded rows =",as.character(nrow(OtherClean())))})
  output$texto52<-renderText({paste("Number of deleted NA rows =",as.character(nrow(Other())-nrow(OtherClean())))})

  #--- Data scaling & projection:

  newdata<-eventReactive(input$go52,{as.matrix(scale(OtherClean()[,-1],center = SOMoutput()$CenterVal, scale = SOMoutput()$ScaleVal))})

  Projection<-eventReactive(input$go52,{if (ncol(OtherClean())!=ncol(SOMoutput()$ExpClean)) {return(NULL)} else {map(SOMoutput()$som_model, newdata())}})

  #--- Projection Outputs

  BmusPROJ<-reactive({Projection()$unit.classif})
  QerrsPROJ<-reactive({Projection()$distances})

  BmusFreqPROJ<-reactive({data.frame(table(BmusPROJ()))})
  HitsPROJ<-reactive({BmusFreqPROJ()$Freq})


  output$texto53<-renderPrint({if (input$go52==0) {cat("", sep = "")
  } else if (ncol(OtherClean())!=ncol(SOMoutput()$ExpClean)) {cat("Upload a file with ", ncol(SOMoutput()$ExpClean), " columns!", sep = "")
  } else {    cat("Projection of ", length(BmusPROJ()), " samples on", sep = "")
    cat("\n SOM of size ", SOMoutput()$Dims$Row, "x", SOMoutput()$Dims$Col, sep = "")
    cat("\n Topology: hexagonal",sep = "")
    cat("\n Neighbourhood function: ",SOMoutput()$Neigh, sep = "")
    cat("\n Distance measure used: Euclidean",sep = "")
    cat("\n Number of training epochs: ",SOMoutput()$Epochs,sep = "")}
  })

  #---- Cluster assignment (respect to Kmeans graphs in third tab!)

  ClusProj<-function() {D<-rep(0,length(BmusPROJ()))
  for (i in c(2:length(KmeansForPlot()$ind))) {d<-BmusClus(BmusPROJ(),KmeansForPlot()$clusNum[,i]);
  D<-cbind(D,d)}; return(data.frame(D))}



  #--- Save Projection Outputs

  OutputPdata<-reactive({list(OtherClean=OtherClean(),newdata=newdata(),HitsPROJ=HitsPROJ(),BmusPROJ=BmusPROJ(),QerrsPROJ=QerrsPROJ(),ClusProj=ClusProj())})


  output$down51<-downloadHandler(filename=function() {paste0("Projection_output_",format(Sys.time(),format="%Y-%m-%d_%H:%M"),".RData")},
                                 content = function(file) {
                                   SOMoutput<-OutputPdata()
                                   list.save(SOMoutput,file=file)}
  )

  #-- If ProjectionOutput file is loaded:

  ProjectionOutput <- eventReactive(input$go53,{list.load(input$OtherOutput$datapath)})

  ProjForPlot<-reactive({if(input$check52==FALSE){OutputPdata()} else {ProjectionOutput()} })

  output$texto54<-renderPrint({if(input$go53==0){cat("", sep = "")} else {cat("Projection of ", length(ProjForPlot()$BmusPROJ), " samples loaded", sep = "")}
  })


  #-- SOM map features (Projection)

  output$UIinput51 <- renderUI({if(input$menu51==" ") {
    tagList(div(style= 'font-size:12px;',selectInput("menu52", label="Plot type", c(" "), selected = " ",width="80%")))
  } else if (input$menu51=="hits" | input$menu51=="qerrs"){tagList(div(style= 'font-size:12px;',
                                                                       selectInput("menu52", label="Plot type", c("grayscale","black filling"), selected = "grayscale",width="80%")))}
  })

  Switch5<-reactive({paste0(input$menu51,input$menu52)})

  Plot51<- function() {switch(Switch5(),
                              "  "={return(NULL)},
                              "hitsgrayscale"={HexaHitsQuant(ProjForPlot()$HitsPROJ,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)},
                              "hitsblack filling"={HexaHits(ProjForPlot()$HitsPROJ,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)},
                              "qerrsgrayscale"={HexaQerrsQuant(ProjForPlot()$BmusPROJ,ProjForPlot()$QerrsPROJ,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)},
                              "qerrsblack filling"={HexaQerrs(ProjForPlot()$BmusPROJ,ProjForPlot()$QerrsPROJ,SOMoutput()$Coord,SOMoutput()$Dims$Row,SOMoutput()$Dims$Col)})
  }

  output$plot51<- renderPlot({Plot51()})


  output$down52 <- downloadHandler(filename =function() {paste0("SOM_projection_",input$menu51,"_",input$menu52,".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h51,width=input$w51)
                                     Plot51()
                                     dev.off()}
  )

  #-- Basic statistics

  Table51<-function() {paramQuant(ProjForPlot()$HitsPROJ)}

  output$down53 <- downloadHandler(filename ="Hits_projection_basic statistics.txt",
                                   content = function(file) {
                                     write.table(Table51(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )

  Table52<-function() {paramQuant(ProjForPlot()$QerrsPROJ)}

  output$down54 <- downloadHandler(filename ="Qerrs_projection_basic statistics.txt",
                                   content = function(file) {
                                     write.table(Table52(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )

  #-- Cluster profiles (projection) --> respect to fourth tab parameters!!!


  ClusProj4<-reactive({NClusChange(ProjForPlot()$ClusProj[,input$n43],numSeq41())})



  Plot52<- function() {if (is.null(ProjForPlot())) {return(NULL)
  } else if (is.null(ProjForPlot())) {return(NULL)
  } else if (length(numSeq41())!=input$n43) {return(NULL)
  } else {switch(input$menu53,
                 "Proj by cluster"={BoxUnits(ProjForPlot()$newdata,ClusProj4(),Ylim=numSeq43(),pitch=numSeq42(),xdim=input$x41)},
                 "Proj by variable"={BoxClus(c(input$n44,input$n45),ProjForPlot()$OtherClean[,-1],ClusProj4())})}
  }

  output$plot52<-renderPlot({if (is.null(ProjForPlot())) {return(NULL)
  } else {Plot52()}
  })

  output$down55 <- downloadHandler(filename =function() {paste0("Cluster profiles_projection",".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h51,width=input$w51)
                                     Plot52()
                                     dev.off()}
  )


  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #                                                                server: SIXTH TAB
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------




  #--- Scelta dataset

  ForDailyGraph<- function() {switch(input$menu61,
                                     "training"={data.frame(date=SOMoutput()$ExpClean$date,Cluster=ClusExp4(),Bmus=SOMoutput()$Bmus,Qerrs=SOMoutput()$Qerrs)},
                                     "projection"={data.frame(date=ProjForPlot()$OtherClean$date,Cluster=ClusProj4(),Bmus=ProjForPlot()$BmusPROJ,Qerrs=ProjForPlot()$QerrsPROJ)})
  }

  #--- Guess Obs/Day

  Guess<-reactive({format(ForDailyGraph()$date,"%d/%m/%Y")})
  Guess2<-reactive({ordered(Guess(), levels = unique(Guess()))})
  Count<-reactive({count(Guess2())})
  Total<-reactive({round(median(Count()$freq,na.rm=T),digit=0)})

  output$UIinput63 <- renderUI({tagList(div(style= CSS32,numericInput("n61", label="Obs/Day", Total(),width="80%"))
  )})

  #--- Scelta intervallo dati

  dateStart61<-function() {paste(                     #--------------------------------------------------># QUA SISTEMARE CON req FUNCTION!!!!!
    substr(ForDailyGraph()[1,"date"], 9, 10),"/",
    substr(ForDailyGraph()[1,"date"], 6, 7),"/",
    substr(ForDailyGraph()[1,"date"], 1, 4),sep="")}

  dateEnd61<-function() {paste(
    substr(ForDailyGraph()[nrow(ForDailyGraph()),"date"], 9, 10),"/",
    substr(ForDailyGraph()[nrow(ForDailyGraph()),"date"], 6, 7),"/",
    substr(ForDailyGraph()[nrow(ForDailyGraph()),"date"], 1, 4),sep="")}

  output$UIinput61 <- renderUI({tagList(
    div(style= CSS32,textInput(inputId = "text61",label = "Starting date:",value=dateStart61(),width="80%")),
    div(style= CSS32,textInput(inputId = "text62",label = "Ending date:",value=dateEnd61(),width="80%"))
  )})

  Selection<-reactive({selectByDate(ForDailyGraph(),start=input$text61,end=input$text62)})


  #--- Daily Graph

  Plot61<- function() {DailyBar(Selection(),Selection()$Cluster,colSeq=colSeq4(),Total=input$n61,xdim=input$x61,ydim=0.8)}

  output$plot61<- renderPlot({Plot61()})


  output$down61 <- downloadHandler(filename =function() {paste0("Daily graph",".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h61,width=input$w61)
                                     Plot61()
                                     dev.off()}
  )

  #--- Download tables:

  Table61<-function() {FreqD(Selection()$date,Selection()$Cluster,Total=input$n61)}

  output$down62 <- downloadHandler(filename ="Interval selection.txt",
                                   content = function(file) {
                                     write.table(Table61(), file, row.names = T,col.names=T,sep="\t",quote=F)}
  )

  Table62<-function() {switch(input$menu61,
                              "training"={Freq(ClusExp4())},
                              "projection"={Freq(ClusProj4())})
  }

  output$down63 <- downloadHandler(filename ="Overall.txt",
                                   content = function(file) {
                                     write.table(Table62(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )

  Table63<-function() {FreqM(ForDailyGraph()$date,ForDailyGraph()$Cluster)}

  output$down64 <- downloadHandler(filename ="Monthly.txt",
                                   content = function(file) {
                                     write.table(Table63(), file, row.names = F,col.names=T,sep="\t",quote=F)}
  )


  #--- Single sample profile

  output$UIinput62 <- renderUI({tagList(
    div(style= CSS32,textInput(inputId = "text63",label = "Select sample:",value=as.character(ForDailyGraph()[2,"date"]),width="100%"))
  )})




  #--- Starting plot

  SelDay<-reactive({ForDailyGraph()[which(as.character(ForDailyGraph()$date)==input$text63),]})

  output$texto61<-renderPrint({    cat(" BMU = ", SelDay()[,"Bmus"],sep = "")
    cat("\n Cluster = ", SelDay()[,"Cluster"], sep = "")
    cat("\n Qe = ", round(SelDay()[,"Qerrs"],digit=3), sep = "")
  })


  SelExpClean<- function() {switch(input$menu61,
                                   "training"={data.frame(date=SOMoutput()$ExpClean$date,SOMoutput()$dataset)},
                                   "projection"={data.frame(date=ProjForPlot()$OtherClean$date,ProjForPlot()$newdata)})
  }
  SelBOX<-reactive({SelExpClean()[which(as.character(SelExpClean()$date)==input$text63),]})


  SingleSampPlot<- function() {

    SLN<-SelDay()[,3]
    CLS<-SelDay()[,2]
    CexLy<-0.9
    CexAxy<-0.85
    Ylab<-c("Cluster")
    Ymin<-0.5
    Ymax<-input$n43+0.5
    nClus<-input$n43
    Names<-colnames(SOMoutput()$codebook)
    Xlim<-c(1,ncol(SOMoutput()$codebook))
    Ylim<-numSeq43()
    pitch<-numSeq42()
    Xdim<-input$x41
    i<-1
    #---qua il primo grafico:
    par(fig=c(0, 0.5, 0, 1),oma=c(1,1,1,1),mar=c(1,1.2,1,1.2),xpd=TRUE,pty="m",family="serif");
    HexagonsClus(Centroids4(), clusNum4(), BCentr4(),
                 SOMoutput()$Coord, SOMoutput()$Dims$Row, SOMoutput()$Dims$Col, colSeq = colSeq4())
    mtext(SelDay()[,"date"][i],side=3,lin=0.5);
    #--qua ultimo dato:
    points(SOMoutput()$Coord$X[SLN[i]], SOMoutput()$Coord$Y[SLN[i]],pch=16,cex=1,col="black")
    mtext(paste0("Qe=", round(SelDay()[,4][i],digit=3)),side=1,lin=0.5);

    #--------qua parte destra con boxplot:
    #-------------------senza whiskers!!!!--------------------------------
    StepB<-1/nClus;
    SeqB<-abs(seq(-1,0,StepB));
    NULLO<-data.frame(matrix(-100000,nrow=nClus,ncol=ncol(SOMoutput()$codebook)))
    NULLO[SelDay()[i,"Cluster"],]<-SelBOX()[i,-1]
    for (w in c(1:nClus)) {
      par(fig=c(0.5, 1, SeqB[w+1], SeqB[w]),mar=c(1.4,1,1.7,0.5),oma=c(0.5,0.5,0,0.5),xpd=FALSE,pty="m",family="serif");
      par(new=TRUE);
      boxplot(SOMoutput()$codebook[which(clusNum4()==w),1],range=0,xaxt="n",yaxt="n",xlim=Xlim,ylim=Ylim,boxwex=0.9,at=1,whisklty = 0, staplelty = 0);
      abline(h=pitch,col="gray");
      for (f in c(1:ncol(SOMoutput()$codebook)))
      {boxplot(SOMoutput()$codebook[which(clusNum4()==w),f],range=0,xaxt="n",yaxt="n",boxwex=1,at=f,add=TRUE,whisklty = 0, staplelty = 0)};
      axis(1,at=seq(1,ncol(SOMoutput()$codebook),1),labels=NA,tcl=-0.3,cex.axis=Xdim);
      axis(1,at=seq(1,ncol(SOMoutput()$codebook),1),labels=Names,lwd=0,line=-0.5,cex.axis=Xdim,las=2);
      axis(2,at=pitch,labels=NA,tcl=-0.3,cex.axis=0.6);
      axis(2,at=pitch,labels=pitch,lwd=0,line=-0.7,cex.axis=0.6,las=2);
      mtext(paste("Cluster ",w,sep=""),line = 0.2,side=3,cex=0.75,family="serif");
      points(seq(1,ncol(SOMoutput()$codebook),1),NULLO[w,], pch=16,col="red",cex=0.7)
    }
  } # END SingleSampPlot


  output$plot62<- renderPlot({SingleSampPlot()})

  output$down65 <- downloadHandler(filename =function() {paste0("Single sample",".pdf")},
                                   content = function(file) {
                                     pdf(file,height=input$h61,width=input$w61)
                                     SingleSampPlot()
                                     dev.off()}
  )



  #------------------------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })



} # server END

#---------------------------SHINY APP BUILT UP -------------------------------------


shinyApp(ui, server)






