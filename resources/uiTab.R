uiTab <- dashboardPage(skin="blue"  ## "blue", "black", "purple", "green", "red", "yellow"
                      ,title="Accenter"
  # fluidPage(title="Flipper - design and review"
   ,dashboardHeader(title=paste("Accenter: Geospatial analysis tools for querying"
                               ,c("ArcNet's PACs and")[integer()]
                               ,"Marxan output")
                   ,disable=!TRUE
                   ,titleWidth=c(550,450)[2]
                   ##~ ,dropdownMenu(type = "messages",badgeStatus = "success"
                                ##~ ,messageItem("Support Team"
                                            ##~ ,"This is the content of a message."
                                            ##~ ,time = "5 mins"
                                            ##~ )
                                ##~ )
                   )
   ,dashboardSidebar(NULL
      ,tags$link(rel="stylesheet",type="text/css",href="./custom.css")
     # ,tags$head(HTML(css))
      ,collapsed=FALSE
      ,disable=FALSE
      ,width=140
      ,sidebarMenu(id="tabs"
        # ,HTML("<center>")
         ,img(src=switch(Sys.getenv("COMPUTERNAME")
                       # ,MARLIN="http://sevin-expedition.ru/netcat_files/img/logo-sev.gif"
                        ,"https://new.wwf.ru/assets/img/logo.svg")
             ,width=140
             ,style="opacity:0.9;padding: 10px")
        # ,HTML("</center>")
         ,br()
         ,sidebarMenuOutput("external")
         ,br()
         ##~ ,absolutePanel(id = 'clear_panel', bottom = 6, left = 510,
               ##~ shinyWidgets::dropdownButton(icon = icon('question'), size = 'sm', up = TRUE, width = '700px', inputId = 'help',
                   ##~ h2('Map isolines for anywhere in the world!'),
                   ##~ hr(),
                   ##~ div(style = 'display: inline-block; vertical-align: bottom; min-width: 100%;',
                       ##~ column(6, style = 'padding-left: 0',
                           ##~ h4('What is an isoline?', style = 'margin-top: 0;'),
                           ##~ p('The prefix "iso", draws its meaning from Ancient Greek meaning "same" or "equal". Each line \
                             ##~ on an isoline map shares the same value. An isochrone is a special type of isoline that \
                             ##~ visualizes travel time. This application shows lines of equal travel time or equal network \
                             ##~ distance for mode choices of driving or walking. Click anywhere on the map and see \
                             ##~ isochrones!', style = 'font-size: 14px;color: black;')
                       ##~ ),
                       ##~ column(6,
                           ##~ img(src = 'example.png', style = 'height: 250px; display: block; margin-bottom: 20px; \
                                                            ##~ margin-right: auto; margin-left: auto;')
                       ##~ )
                   ##~ )
               ##~ )
         ##~ )
         ,br()
         ,menuItem(text="Main panel",tabName="main",icon=icon("chart-bar")
                  ,selected=TRUE
                  )
         ,menuItem(text="Documentation",tabName="question",icon=icon("question")
                  )
         ,menuItem(text="Info",tabName="info", icon = icon("info")
                  )
         ,br()
         ,br()
         ,br()
         ,br()
         ##~ ,a(href="https://orcid.org/0000-0001-7196-7882"
           ##~ ,img(src="https://cran.rstudio.com/web/orcid.svg",height=16,alt="Nikita Platonov"
               ##~ ,style="padding-left: 15px;"))
         ,menuItem(text="Nikita Platonov"
                  ,href="https://orcid.org/0000-0001-7196-7882",icon=icon("user"))
         ,menuItem(text=" ",icon=icon("github")
                  ,href="https://github.com/nplatonov/accenter/"
                  )
      )
   )
   ,dashboardBody(id="resetable"
     # ,tags$script(HTML("$('body').addClass('sidebar-mini');"))
      ,tabItems(
          tabItem(tabName="info",uiOutput("about")
            ##~ ,fluidRow(NULL 
               ##~ ,box(width=12
                  ##~ ,column(1)
                  ##~ ,column(6,uiOutput("about"))
                  ##~ ,column(5)
               ##~ )
            ##~ )
         )
         # tabItem(tabName="info",infoTab())
        # ,tabItem(tabName="question",questionTabUnused())
         ,tabItem(tabName="question"
            ,fluidRow(NULL 
               ,box(width=12
                  ,column(1)
                  ,column(11,uiOutput("question"))
               )
            )
         )
         ,tabItem(tabName="main"
            ,fluidRow(NULL
               ,tabBox(width=12,title="",id="tabset1",selected=c("map","details","geopu")[1]
                  ,tabPanel(title=if (shortTab) "Map" else uiOutput("tabMap")
                           #title=ifelse(shortTab,"Map","Select an area on the map")
                           ,value="map",icon=icon("globe")
                     ,fluidRow(NULL
                        ,column(8
                           ,uiOutput("uiMap") %>% withSpinner() ## use it
                          # ,leafletOutput("viewerLeaflet") ## only viewer
                          # ,editModUI("editor") ## only editor
                        )
                        ,column(4
                           ,selectInput("rpath","Selection"
                                       ##~ ,list('Custom'=rname1[1],'Preselected'=rname1[-1])
                                       ##~ ,selected=ifelse(length(rname1)==1,rname1,sample(rname1,1))
                                       ,initName
                                       ,selected=initName
                                      # ,selected=grep("INTERACTIVE",rname1,value=TRUE,ignore.case=TRUE)
                                       ,width="500px"
                                       )
                           ,selectInput("spath","Scenario"
                                       ,initName
                                       ,selected=initName
                                       ##~ ,sname0
                                       ##~ ,selected=sample(sname0,1)
                                      # ,selected=grep("sc08",sname0,value=TRUE)
                                       ,width="500px"
                                       )
                           ,selectInput("epsg","Projection"
                                       ,initName
                                       ,selected=initName
                                       ##~ ,epsgList
                                       ##~ ,selected=3575
                                       ,width="160px"
                                       )
                           ,selectInput("freq","Marxan output"
                                       ,c("Frequency","Best run","Frequency >= 0.5","Frequency >= 0.75") ## ≥
                                       ,selected="Frequency"
                                      # ,selected="Frequency >= 0.5"
                                       ##~ ,epsgList
                                       ##~ ,selected=3575
                                       ,width="160px"
                                       )
                        )
                     ) ## fluidRow
                  ) ## tabPanel
                  ,tabPanel(title=if (shortTab) "Details" else uiOutput("tabDetails")
                          # title=ifelse(shortTab,"Details","Conservation features in Selection")
                           ,value="details",icon=icon("th-list")
                     ,fluidRow(NULL
                        ,column(12
                          # ,h3("Targets Achievement"),
                           ,DT::DTOutput("tbl")
                        )
                     ) ## fluidRow
                  ) ##tabPanel
                  ,tabPanel(title=if (shortTab) "Review" else uiOutput("tabReview")
                          #title=ifelse(shortTab,"Review","Selection results overview")
                           ,value="review",icon=icon("dashboard")
                     ,fluidRow(NULL
                        ,column(6
                           ,leafletOutput("selectLeaflet")
                        )
                        ,column(6
                           ,fluidRow(NULL
                              ,column(6
                                 ,h4("Planning Unit Selection Frequency Distribution")
                                 ,textOutput("cells")
                                 ,br()
                                 ,textOutput("species")
                                 ,br()
                                 ,verbatimTextOutput("dt_verbatim")
                                 ,br()
                              )
                              ,column(6
                                 ,plotlyOutput("plotlyBox")#,width="300px")
                              )
                           )
                           ,fluidRow(NULL
                              ,plotlyOutput("plotlyHist")#,width="300px")
                           )
                           ##~ ,fluidRow(NULL
                              ##~ ,textOutput("plotlyDesc")
                           ##~ )
                        )
                     ) ## fluidRow
                  ) ## tabPanel
                  ,tabPanel(title=ifelse(shortTab,"Props","Scenario properties")
                           ,value="geopu",icon=icon("map")
                     ,fluidRow(NULL
                        ,column(8
                           ,uiOutput("geopu") %>% withSpinner()
                          # ,DT::DTOutput("tbl")
                        )
                        ,column(2
                           ,htmlOutput("selectedScenario")
                           ,br()
                           ,br()
                           ,htmlOutput("selectedEPSG")
                           ,br()
                           ,br()
                           ,selectInput("puprm","Property"
                                       ,c("Data coverage","Layer overlap"
                                         ,"Representative","Distinctive")
                                       ,selected="Data coverage")
                           ,br()
                           ,p("Units are layers per cell")
                        ,column(2)
                        )
                     ) ## fluidRow
                  ) ##tabPanel
                 # ,tabPanel(title="About",value="about",icon=icon("info-circle")
                 #    ,aboutTab()
                 # ) ## tabPanel
               ) ## tabBox
            ) ## fluidRow
         ) ## tabItem
      ) ## tabItems
   ) ## dashboardBody
)
