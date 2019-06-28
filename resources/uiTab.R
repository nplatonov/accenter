uiTab <- dashboardPage(skin="blue"  ## "blue", "black", "purple", "green", "red", "yellow"
  # fluidPage(title="Flipper - design and review"
   ,dashboardHeader(title="Accenter: Imagine Marxan",disable=TRUE,titleWidth=300)
   ,dashboardSidebar(NULL
     # ,tags$link(rel="stylesheet",type="text/css",href="./custom.css")
      ,tags$head(HTML(css))
      ,collapsed=FALSE
      ,disable=FALSE
      ,width = 40
      ,sidebarMenu(id="tabs"
        # ,HTML("<center>")
         ,img(src=switch(Sys.getenv("COMPUTERNAME")
                       # ,MARLIN="http://sevin-expedition.ru/netcat_files/img/logo-sev.gif"
                        ,"https://new.wwf.ru/assets/img/logo.svg")
             ,width=40,style="opacity:0.5;")
        # ,HTML("</center>")
         ,br()
         ,br()
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
         ,menuItem(text="Main panel",tabName="main",icon=icon("bars")
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
          tabItem(tabName="info",infoTab())
        # ,tabItem(tabName="question",questionTabUnused())
         ,tabItem(tabName="question"
            ,fluidRow(NULL 
               ,box(width=12
                  ,column(2)
                  ,column(8,uiOutput("question"))
                  ,column(2)
               )
            )
         )
         ,tabItem(tabName="main"
            ,fluidRow(NULL
               ,tabBox(width=12,title="",id="tabset1",selected="map"
                  ,tabPanel(title="Map",value="map",icon=icon("globe")
                     ,fluidRow(NULL
                        ,column(8
                           ,uiOutput("ui")
                          # ,editModUI("editor")
                        )
                        ,column(4
                           ,selectInput("rpath","Selection"
                                       ##~ ,list('Manual'=rname1[1],'Preselected'=rname1[-1])
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
                           ,selectInput("epsg","Projection (EPSG code)"
                                       ,initName
                                       ,selected=initName
                                       ##~ ,epsgList
                                       ##~ ,selected=3575
                                       ,width="500px"
                                       )
                        )
                     ) ## fluidRow
                  ) ## tabPanel
                  ,tabPanel(title="Review",value="review",icon=icon("dashboard")
                     ,fluidRow(NULL
                        ,column(4
                           ,textOutput("cells")
                           ,br()
                           ,textOutput("species")
                           ,br()
                           ,verbatimTextOutput("dt_verbatim")
                        )
                        ,column(4
                           ,plotOutput("selectstat",width="300px")
                        )
                        ,column(4
                           ,leafletOutput("selectLeaflet")
                        )
                     ) ## tabRow
                  ) ## tabPanel
                  ,tabPanel(title="Details",value="details",icon=icon("th-list")
                     ,fluidRow(NULL
                        ,column(12
                          # ,h3("Targets Achievement"),
                           ,DT::DTOutput("tbl")
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
