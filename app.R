seed <- sample(100:999,1) ## 239 823
print(c(seed=seed))
set.seed(seed)
epsgList <- 3571:3576
height <- "612px"
editName <- c("*** INTERACTIVE ***","Preselected (dummy)")[1]
devel <- length(grep("^[A-Z]\\:/platt",Sys.getenv("USER")))>0 & !interactive() #.argv0()=="flipper-pampan.R"
tryToRefuseLeafletRendering <- FALSE && devel
css <- character()
if (file.exists("www/custom.css")) {
   css <- readLines("www/custom.css")
   css <- paste(c("<style>",paste("   ",css),"</style>"),collapse="\n")
}
regionName <- c("Pan-Arctic area")
'.argv0' <- function() {
   arglist <- commandArgs(FALSE)
   if (length(ind <- grep("^--file=.+",arglist,ignore.case=FALSE))==1)
      return(basename(strsplit(arglist[ind],"=")[[1]][2]))
   if (length(ind <- grep("^-f$",arglist,ignore.case=FALSE))==1)
      return(basename(arglist[ind+1L]))
   ""
}
'.elapsedTime' <- function(message="",reset=FALSE,toPrint=FALSE)
{
   startTime <- getOption("ursaTimeStart")
   deltaTime <- getOption("ursaTimeDelta")
   if (is.null(startTime))
      startTime <- proc.time()
   if (is.null(deltaTime))
      deltaTime <- startTime
   if (message=="")
      message <- paste(as.character(Sys.time()),"***")
   else
      message <- paste(message,":",sep="")
   mytext <- sprintf("*** %s %.2f(%.2f) seconds ***"
                   # ,as.character(Sys.time())
                    ,message,(proc.time()-startTime)[3]
                    ,(proc.time()-deltaTime)[3])
   if (reset)
      options(ursaTimeStart=proc.time())
   options(ursaTimeDelta=proc.time())
   if (toPrint)
      print(mytext)
   return (message(mytext))
}
'infoTab' <- function() {
   opW <- options(warn=2) ## 2
   if (inherits(ret <- try(includeMarkdown("./accenter-info.md")),"try-error"))
      ret <- helpText("Should be provided in './accenter-info.md' file")
   options(opW)
   ret
}
'questionTabUnused' <- function() {
   if (F) {
      opW <- options(warn=2) ## 2
      if (inherits(ret <- try(includeMarkdown("./accenter-question.md")),"try-error"))
         ret <- helpText("Should be provided in './accenter-question.md' file")
      options(opW)
   }
   else if (F) {
      ret <- HTML(markdown::markdownToHTML(knitr::knit('question.Rmd',quiet=FALSE)))
   }
   else {
      a1 <- "res1.html" # tempfile()
      rmarkdown::render('question.Rmd'
                       ,output_format=rmarkdown::html_fragment()
                      # ,output_format=rmarkdown::html_vignette(css=NULL)
                       ,output_file=a1,quiet=TRUE
                      # ,params=list(prm=analysis(),kind=1)
                       )
      a2 <- scan(a1,what=character(),encoding="UTF-8",quiet=TRUE)
     # file.remove(a1)
      ret <- HTML(a2)
   }
   ret
}
dpath <- "."
mpath <- file.path(dpath,"common")
print(c(dpath=dpath,mpath=mpath),quote=FALSE)
gpath <- file.path(dpath,"results")
rpath <- file.path(dpath,"predefined")
   p <- proc.time()
   options(ursaTimeStart=p,ursaTimeDelta=p)
   rm(p)
if (devel)
   .elapsedTime("package load -- start")
suppressMessages({
   require(shiny)
   require(shinydashboard)
  # require(shinyjs)
  # require(sf)
   require(leaflet)
  # require(mapview)
   require(mapedit)
  # require(ursa)
   require(DT)
   requireNamespace("lwgeom")
})
if (devel)
   .elapsedTime("package load -- finish")
rname0 <- dir(path=rpath,pattern="\\.(geojson|sqlite|shp)(\\.(zip))*$",full.names=TRUE)
rname1 <- c(editName,gsub("\\.shp(\\.zip)*","",basename(rname0)))#[1]
spath0 <- list.dirs(gpath,full.names=FALSE,recursive=FALSE)#[-1]
spath0 <- grep("sc\\d{2}(-\\d{8})*",spath0,value=TRUE)
spath0 <- grep("(spf|blm|numitns|hide|prev|delete|remove)"
              ,spath0,value=TRUE,invert=TRUE,ignore.case=TRUE)
spath0 <- spath0[nchar(spath0)>0]
#spath0 <- basename(dirname(dir(path=file.path(gpath,spath0)
#                             ,pattern="^freq.shp(\\.zip)*$",full.names=TRUE)))
#str(spath0)
if (!length(spath0))
   stop("Scenarios dir is empty")
mdname <- dir(path=file.path(dpath,"scenarios"),pattern="\\.xlsx$",full.names=TRUE)
if (googleDrive <- length(mdname)!=1) {
   gsheetFile <- "./scenarios/gsheet.id"
   if (file.exists(gsheetFile))
      gsheet <- readLines(gsheetFile,warn=FALSE)
   else {
      gsheetFile <- dir(path="./scenarios",pattern="\\.gsheet$"
                       ,full.names=TRUE,recursive=FALSE)
      if (length(gsheetFile)==1)
         gsheet <- jsonlite::fromJSON(gsheetFile)$doc_id
      else
         gsheet <- stop("cannot get scenarios sheet files")
   }
   mdname <- tempfile(fileext=".xlsx") ## tmpdir=file.path(dpath,"scenarios"),
   download.file(paste0("https://docs.google.com/spreadsheets/export?id="
                       ,gsheet,"&format=xlsx"),mdname,mode="wb")
}
scname <- readxl::read_excel(mdname,sheet="Scenarios")
cfname <- readxl::read_excel(mdname,sheet="CF_list")
if (googleDrive)
   file.remove(mdname)
scname <- scname[,c("#","Name")]
cfname <- cfname[,c("CF_code","File_name","CF_name")]
scind <- match(as.integer(gsub("sc(\\d+).*","\\1",spath0)),scname$'#')
#if (length(ind <- grep("^(input|output|pulayer)$",basename(spath0))))
#   spath0 <- spath0[-ind]
sname0 <- unname(sapply(file.path(gpath,spath0),function(x) {
   fname <- file.path(x,"scenario.txt")
   if (file.exists(fname)) readLines(fname) else basename(x)
}))
sname0 <- sapply(strsplit(sample(sname0),split="\\s+--\\s+"),function(x) {
   paste(x,collapse=", ")
})
#spath <- file.path(gpath,spath0)[1] ## not used anywhere
if (F)
   sname0 <- spath0
if (T)
   sname0 <- paste0(scname[["#"]][scind],": ",scname[["Name"]][scind])
sname <- sname0[1]
'shapefile' <- function(fname) {
   if (isZip <- length(grep("\\.zip$",basename(fname)))>0) {
      if (devel)
         print(fname)
      list1 <- unzip(fname)
      ret <- sf::st_read(list1[grep("\\.shp$",list1)],quiet=TRUE)
      file.remove(list1)
   }
   else
      ret <- sf::st_read(fname,quiet=TRUE)
   if (FALSE)
      return(ret)
   sf::st_zm(ret)
}
Fpu <- dir(path=mpath,pattern="^pulayer\\.shp\\.zip$",recursive=TRUE,full.names=TRUE)
pu <- shapefile(Fpu)
'polarmap' <- function(epsg,centered=TRUE,data=NULL) {
   if (is.character(epsg))
      epsg <- as.integer(epsg)
   extent <- 11000000 + 9036842.762 + 667
   origin <- c(-extent, extent)
   maxResolution <- 2*extent/256
   bounds <- list(c(-extent,extent),c(extent,-extent))
   resolutions <- purrr::map_dbl(0:18,function(x) maxResolution/(2^x))
   crsArctic <- leafletCRS(crsClass="L.Proj.CRS",code=paste0("EPSG:",epsg)
                          ,proj4def=sf::st_crs(epsg)$proj4string
                          ,resolutions=resolutions,origin=origin,bounds=bounds)
   str(crsArctic)
   if (is.null(data))
      m <- leaflet(options=leafletOptions(crs=crsArctic,minZoom=3,maxZoom=9))
   else
      m <- leaflet(data,options=leafletOptions(crs=crsArctic,minZoom=3,maxZoom=9))
   if (centered) {
      if (epsg==3575)
         m <- setView(m,-100,80,4) 
      else if (epsg==3576)
         m <- setView(m,-100,82,4)
      else if (epsg==3574)
         m <- setView(m,-40,82,4)
      else if (epsg==3573)
         m <- setView(m,-100,84,4)
      else if (epsg==3572)
         m <- setView(m,-150,86,4)
      else if (epsg==3571)
         m <- setView(m,180,87,4)
      else if (TRUE)
         m <- setView(m,0,90,4)
        # m <- setView(m,12.57,55.687,12) ## Kopenhagen
   }
   m <- addTiles(m,urlTemplate=paste0("http://{s}.tiles.arcticconnect.ca/osm_"
                                     ,epsg,"/{z}/{x}/{y}.png")
                ,attribution="Map: © ArcticConnect. Data: © OpenStreetMap contributors"
                ,options=tileOptions(subdomains="abc"
                                    ,noWrap=TRUE,continuousWorld=FALSE)) 
  # if (!is.null(data))
  #    m <- addPolygons(m,data=data,weight=0.5)
  # m <- addGraticule(m,interval=10)
   m
}
#Fpu <- dir(path=mpath,pattern="^pulayer\\.shp\\.zip$",recursive=TRUE,full.names=TRUE)
#pu <- shapefile(Fpu)
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
                        ,MARLIN="http://sevin-expedition.ru/netcat_files/img/logo-sev.gif"
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
         ,menuItem(text=" ",icon=icon("github"),
                   ##~ href="https://stackoverflow.com/questions/36679944/mapview-for-shiny")
                   href="https://github.com/nplatonov/accenter/")
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
                                       ,list('Manual'=rname1[1],'Preselected'=rname1[-1])
                                       ,selected=ifelse(length(rname1)==1,rname1,sample(rname1,1))
                                      # ,selected=grep("INTERACTIVE",rname1,value=TRUE,ignore.case=TRUE)
                                       ,width="500px"
                                       )
                           ,selectInput("spath","Scenario",sname0
                                       ,selected=sample(sname0,1)
                                      # ,selected=grep("sc08",sname0,value=TRUE)
                                       ,width="500px"
                                       )
                           ,selectInput("epsg","Projection (EPSG code)",epsgList,selected=3575
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
server <- function(input,output,session) {
   if (T & devel)
      session$onSessionEnded(stopApp)
   exchange <- reactiveValues(edits=NULL,puvspr=NULL,spec=NULL#,prob=NULL
                             ,freq=NULL,freq3=NULL,overlay=NULL,res=NULL)
  # scenario <- observeEvent(input$spath,{
   scenario <- observe({
      if (devel)
         .elapsedTime("scenario (observe) -- start")
      ##~ input$epsg
      ##~ input$spath
      ##~ input$rpath
      showModal(modalDialog(title = "Data verification in progress","Please wait"
                           ,size="s",easyClose = TRUE,footer = NULL))
     # .elapsedTime("observe -- start")
      epsg <- as.integer(input$epsg)
      spath <- file.path(gpath,spath0[match(input$spath,sname0)])
      freq <- shapefile(file.path(spath,"freq.shp.zip"))
      freq$ID <- pu$ID
      freq$value <- freq$sum/max(freq$sum) ## subject to be overestimated
     # prob <- freq
     # sf::st_geometry(prob) <- NULL
      Fspec <- dir(path=spath,pattern="spec\\.dat(\\.gz)*$"
                  ,recursive=TRUE,full.names=TRUE)
      opW <- options(encoding="UTF-8")
      spec <- read.csv(Fspec,sep=",") #,encoding="UTF-8") ## encoding="windows-1251")
      options(opW)
      spec$prop <- spec$target/spec$amount
      Fpuvspr <- dir(path=spath,pattern="puvspr\\.dat(\\.gz)*$"
                    ,recursive=TRUE,full.names=TRUE)
      if (!length(Fpuvspr))
         Fpuvspr <- dir(path=mpath,pattern="puvspr\\.dat(\\.gz)*$"
                       ,recursive=TRUE,full.names=TRUE)
      puvspr <- read.csv(Fpuvspr)
     # freq <- aggregate(freq,by=list(freq$sum),mean)["sum"]
      Fagr <- file.path(spath,"freq-aggregate.shp.zip")
      if (file.exists(Fagr)) {
         freq3 <- shapefile(Fagr)
         freq3 <- sf::st_transform(shapefile(Fagr),4326)
      }
      else if (TRUE) {
         if (devel)
            .elapsedTime("aggregate -- start")
         freq3 <- sf::st_transform(
                       aggregate(freq,by=list(freq$sum),mean)["sum"],4326)
         if (devel)
            .elapsedTime("aggregate -- finish")
      }
      else
         freq3 <- sf::st_transform(freq,4326)
      if (input$rpath==editName) {
         if (devel)
            .elapsedTime("   mapedit leaflet -- start")
         m <- polarmap(epsg)
         pal <- colorNumeric(palette="viridis",domain=freq3$sum)
         m <- addPolygons(m,data=freq3["sum"]
                         ,color=~pal(sum)
                         ,weight=0
                        # ,popup=~sum
                         ,label=~as.character(sum)
                         ,stroke=TRUE
                        # ,weight=0.5
                         ,fillOpacity=0.5
                        # ,highlightOptions=highlightOptions(fillOpacity=0)
                         )
         if (devel)
            .elapsedTime("   mapedit leaflet -- finish")
         if (input$rpath==editName) {
            edits <- callModule(editMod,"editor",leafmap=m
                                       ,editor=c("leaflet.extras","leafpm")[2])
            exchange$edits <- edits
         }
      }
     # else
     #    exchange$edits <- NULL
      exchange$puvspr <- puvspr
      exchange$spec <- spec
     # exchange$prob <- prob
      exchange$freq <- freq
      exchange$freq3 <- freq3
      if (devel)
         .elapsedTime("scenario (observe) -- finish")
      removeModal()
   })
  # edits <- callModule(editMod,"editor",m@map)
  # scenario$invalidate()
   data <- reactive({
      if (devel)
         dir.create("./shiny.log",showWarnings=FALSE)
      if (devel)
         .elapsedTime("data (reactive) -- start")
      epsg <- input$epsg
      if (input$rpath==editName) {
         edits <- exchange$edits()
         if (devel)
            .elapsedTime("   data (reactive) -- switch to edit")
         req(edits$finished)
        # s <- subset(edits()$finished,feature_type %in% c("rectangle","polygon","polyline"))
         s <- edits$finished
         req(nrow(s)>0)
      }
      else {
        # exchange$edits <- NULL
        # rname <- rname0[match(input$rpath,gsub("\\.(zip)","",basename(rname0)))]
         rname <- rname0[grep(input$rpath,basename(rname0))]
         s <- shapefile(rname)
         if (FALSE) {
            if (devel)
               .elapsedTime("predefined union -- start")
            print(sf::st_crs(s))
           # print(s)
           # str(s)
            print(nrow(s))
            print(length(s))
            print(length(sf::st_geometry(s)))
            s <- sf::st_union(s)
            print(sf::st_crs(s))
            print(nrow(s))
            print(length(s))
            print(length(sf::st_geometry(s)))
            if (devel)
               .elapsedTime("predefined union -- finish")
         }
        # s <- sf::st_geometry(s)
      }
      exchange$overlay <- s
     # exchange$edits <- NULL
      freq <- exchange$freq
      req(nrow(freq)>0)
      if (devel)
         .elapsedTime("   intersection -- start")
      ##~ showNotification(id="reactive",closeButton=FALSE,duration=120,"Find intersection..."
                      ##~ ,type="warning")
      if (!all(sf::st_is_valid(s))) {
         showNotification(id="valid",closeButton=FALSE,duration=120,"Make geometry valid..."
                         ,type="warning")
         s <- lwgeom::st_make_valid(s)
         removeNotification(id="valid")
      }
      s <- sf::st_transform(sf::st_geometry(s),sf::st_crs(freq))
     # sf::st_agr(s) <- "constant"
      sf::st_agr(freq) <- "constant"
     # res <- sf::st_intersection(freq,s)
      print(c(careful=length(sf::st_geometry(s))))
      careful <- length(sf::st_geometry(s))!=1
      showModal(modalDialog(title = "Find intersection...","Please wait"
                           ,size="s",easyClose = TRUE,footer = NULL))
      file.remove(dir(path="./shiny.log",pattern="^res.+\\.(cpg|dbf|prj|shp|shx)"
                     ,full.names=TRUE))
      if (devel) {
         sf::st_write(freq,"./shiny.log/res-freq.shp",quiet=TRUE)
         sf::st_write(s,"./shiny.log/res-s.shp",quiet=TRUE)
      }
      if (careful)
         res <- sf::st_intersection(freq,s)[c("ID","sum")]
      else {
         ind <- sf::st_disjoint(s,freq,sparse=F)
         res <- freq[which(!ind),]
      }
      removeModal()
      removeNotification(id="reactive")
      if (devel)
         .elapsedTime("   intersection -- finish")
      req(nrow(res)>0)
      if (careful) {
         ind1 <- seq(nrow(res)) #which(sf::st_area(res)>=0)
         ind2 <- match(res[["ID"]][ind1],freq[["ID"]])
         sf::st_geometry(res[ind1,]) <- sf::st_geometry(freq[ind2,])
      }
      xy <- do.call("rbind",unclass(sf::st_centroid(sf::st_geometry(res))))
      ind <- which(!duplicated(xy))
      res <- res[ind,]
      ID <- res[["ID"]]
      if (FALSE) {
         if (devel)
            .elapsedTime("union -- start")
         res <- sf::st_union(res)
         res <- sf::st_sf(res,data=data.frame(FID=0))
         if (devel)
            .elapsedTime("union -- finish")
      }
      if (devel) {
         file.remove(dir(path="./shiny.log",pattern="intersection\\.(cpg|dbf|prj|shp|shx)"
                    ,full.names=TRUE))
         sf::st_write(res,"./shiny.log/intersection.shp",quiet=TRUE)
      }
      puvspr <- exchange$puvspr
      spec <- exchange$spec
     # prob <- exchange$prob
      res1 <- subset(puvspr,pu %in% ID)
      spec2 <- sort(unique(res1$species))
      res3 <- subset(puvspr,species %in% spec$id)
      res3$reached <- freq$value[match(res3$pu,freq$ID)]*res3$amount
      res3a <- res3[res3$pu %in% ID,]
      res4 <- aggregate(res3[,c("amount","reached")],by=list(species=res3$species),sum)
      res4a <- aggregate(res3a[,c("amount","reached")],by=list(species=res3a$species),sum)
      spec$reached <- res4$reached
      ind <- match(res4a$species,res4$species)
      spec$selected <- 0
      spec$selected[ind] <- res4a$reached
      spec$represent <- 0
      spec$represent[ind] <- res4a$amount
      res5 <- with(spec,data.frame(id=id ## 1
                                  ,name=name # "should be specified in English" # runame ## 2
                                  ,represent=represent/amount ## 3
                                  ,target=prop ## 4
                                  ,reached=reached/amount/prop ## 5
                                  ,selected=selected/amount/prop ## 6
                                  ))
     # cat("--------------------------\n")
     # str(res5name)
     # cat("==========================\n")
      res5$name <- cfname$CF_name[match(res5$id,as.integer(cfname$CF_code))]
      res5$prop <- with(res5,selected/reached) ## 7
      if (TRUE)
         res5 <- res5[with(res5,order(prop,represent,decreasing=TRUE)),]
      exchange$res <- res
      if (devel)
         .elapsedTime("data (reactive) -- finish")
      list(res=res,freq=freq,freq3=exchange$freq3,res5=res5)
   })
   output$ui <- renderUI({
      if (T & input$rpath==editName) {
         ret <- editModUI("editor")#,height=height)
      }
      else {
         ret <- leafletOutput("viewerLeaflet")#,height=height)
      }
      ret
   })
   output$viewerLeaflet <- renderLeaflet({
      epsg <- as.integer(input$epsg)
      freq <- data()$freq
      freq3 <- data()$freq3
      predefined <- exchange$overlay
      req(nrow(freq))
     # req(nrow(predefined))
      req(length(sf::st_geometry(predefined)))
      if (devel)
         .elapsedTime("occurrence leaflet -- start")
     # showNotification(closeButton=FALSE,duration=12
     #                 ,"Data processing in progress",type="warning")
      showNotification(id="leaflet",closeButton=FALSE,duration=120,"Render leaflet..."
                      ,type="warning")
     # print("LEAFLET PREDEFINED -- start")
      if (FALSE) {
         predefined <- sf::st_transform(predefined,sf::st_crs(freq))
         predefined$column_for_matching <- seq(nrow(predefined))
         sf::st_agr(predefined) <- "constant"
         predefined2 <- sf::st_crop(sf::st_simplify(predefined,dTolerance=1e-34)
                               ,sf::st_bbox(freq)+500*1e3*c(-1,-1,1,1))
         ind <- match(predefined2$column_for_matching,predefined$column_for_matching)
         predefined2$column_for_matching <- NULL
         sf::st_geometry(predefined2) <- sf::st_geometry(predefined)[ind]
         predefined2 <- sf::st_transform(predefined2,4326)
      }
      else  {
         if (T) {
            if (devel)
               .elapsedTime("   union -- start")
            predefined <- sf::st_union(predefined)
            if (devel)
               .elapsedTime("   union -- finish")
         }
         predefined2 <- sf::st_transform(predefined,4326)
      }
      if (FALSE) {
         freq <- sf::st_transform(freq,4326)
         if (TRUE) ## simplified
            freq2 <- aggregate(freq,by=list(freq$sum),mean)["sum"]
      }
      if (tryToRefuseLeafletRendering) {
        # m <- polarmap(epsg)
         m <- mapview::mapview() %>% leafem::removeMouseCoordinates()
        # m <- mapview() ## return 'm@map'. but after modifications can returnx` 'm'
        # m <- leafem::removeMouseCoordinates(m)
        # m <- removeMouseCoordinates(m)
         if (devel)
            .elapsedTime("occurrence leaflet -- finish")
         removeNotification(id="leaflet")
         return(m)
      }
      m <- polarmap(epsg)
      pal <- colorNumeric(palette="viridis",domain=freq3$sum)
      m <- addPolygons(m,data=freq3["sum"]
                      ,color=~pal(sum)
                      ,weight=0
                     # ,popup=~sum
                      ,label=~as.character(sum)
                      ,stroke=TRUE
                     # ,weight=0.5
                      ,fillOpacity=0.5
                     # ,highlightOptions=highlightOptions(fillOpacity=0)
                      )
      m <- addPolygons(m,data=predefined2,weight=1,fillOpacity=0.3)
      ##~ pal <- colorNumeric(palette="viridis",domain=freq$sum)
     ##~ # m <- leaflet(freq) %>% addTiles()
      ##~ m <- leaflet() %>% addTiles() %>%
           ##~ addPolygons(data=freq2
                      ##~ ,color=~pal(sum)
                      ##~ ,weight=0
                     ##~ # ,popup=~sum
                      ##~ ,label=~as.character(sum)
                      ##~ ,stroke=TRUE
                     ##~ # ,weight=0.5
                      ##~ ,fillOpacity=0.5
                     ##~ # ,highlightOptions=highlightOptions(fillOpacity=0)
                      ##~ ) %>%
           ##~ addPolygons(data=predefined2,weight=1,fillOpacity=0.3)
     # print("LEAFLET PREDEFINED -- finish")
      if (devel)
         .elapsedTime("occurrence leaflet -- finish")
      removeNotification(id="leaflet")
      m
   })
   output$selectLeaflet <- renderLeaflet({
      d <- data()$res
      req(nrow(d)>0)
      showNotification(id="leaflet",closeButton=FALSE,duration=120,"Render leaflet..."
                      ,type="warning")
      on.exit(removeNotification(id="leaflet"))
      ind <- input$tbl_rows_selected
      withOverlap <- !is.null(ind)
      if (devel)
         .elapsedTime("review leaflet -- start")
      if (T) {
         if (!withOverlap) {
            sf::st_agr(d) <- "constant"
            cd <- unclass(sf::st_transform(sf::st_geometry(sf::st_centroid(d)),4326))
            lon0 <- sapply(cd,function(xy) xy[1])
         }
         else {
            puvspr <- exchange$puvspr
            xy <- NULL
            for (i in ind) {
               dt <- data()$res5[i,]
               ind2 <- which(as.integer(puvspr$species) %in% as.integer(dt$id))
               res <- puvspr[ind2,]
               ind3 <- which(pu$ID %in% res$pu)
               xy <- rbind(xy,sf::st_geometry(pu[ind3,]))
            }
            cd <- sf::st_sfc(xy)
            sf::st_crs(cd) <- sf::st_crs(pu)
            cd <- unclass(sf::st_transform(sf::st_geometry(sf::st_centroid(cd)),4326))
            lon0 <- sapply(cd,function(xy) xy[1])
            lon0 <- mean(lon0)
         }
         if ((FALSE)&&(any(lon0)<0)&&(any(lon0)>0)) {
            if (any(abs(lon))>150)
               epsg <- 3571
            else
               epsg <- 3575
         }
         else if (lon0<(-165) || lon0>=(+135))
            epsg <- 3571
         else if (lon0>=(-165) && lon0<(-125))
            epsg <- 3572
         else if (lon0>=(-125) && lon0<(-70))
            epsg <- 3573
         else if (lon0>=(-70) && lon0<(-25))
            epsg <- 3574
         else if (lon0>=(-25) && lon0<(+50))
            epsg <- 3575
         else if (lon0>=(50) && lon0<(+135))
            epsg <- 3576
         else
            stop("which is lon0?:",dQuote(lon0))
         if (T) {
            if (devel)
               .elapsedTime("   union -- start")
            d <- sf::st_union(d)
            if (devel)
               .elapsedTime("   union -- finish")
         }
         d <- sf::st_transform(d,4326)
         m <- polarmap(epsg,centered=FALSE)
         m <- addPolygons(m,data=d,weight=0.5)
      }
      else {
         d <- sf::st_transform(d,4326)
         m <- leaflet() %>% addTiles() %>%
              addPolygons(data=d,weight=0.5,popup=d$sum)
      }
      if (devel)
         .elapsedTime("review leaflet -- finish")
      if (!withOverlap) 
         return(m)
      for (i in ind) {
         dt <- data()$res5[i,]
         ind2 <- which(as.integer(puvspr$species) %in% as.integer(dt$id))
         res <- puvspr[ind2,]
         ind3 <- which(pu$ID %in% res$pu)
         sf::st_geometry(res) <- sf::st_geometry(pu[ind3,])
         pal <- colorNumeric(palette="plasma",domain=res$amount)
         m <- addPolygons(m,data=sf::st_transform(res,4326)
                         ,color=~pal(amount)
                         ,weight=0 # 0.5
                        # ,popup=~sum
                         ,label=~as.character(round(amount,2))
                         ,stroke=TRUE
                        # ,weight=0.5
                         ,fillOpacity=0.5
                        # ,highlightOptions=highlightOptions(fillOpacity=0)
                         )
      }
      m
   })
   output$selectstat <- renderPlot({
      d <- data()
      nrep <- max(d$freq$sum)
      boxplot(
         list(
            all = d$freq$sum/nrep,
            selected = d$res$sum/nrep
         ),
         xlab = "probability of selection"
      )
   })
   output$cells <- renderText({
      paste0("Cells in ",regionName,": ",nrow(pu),". "
            ,"Cells in selection: ",nrow(data()$res),".")
   })
   output$species <- renderText({
      paste0("Conservation features in project: "
            ,length(unique(exchange$puvspr$species)),". "
            ,"Conservation features in scenario: "
            ,nrow(data()$res5),". "
            ,"Conservation features in selection: "
            ,nrow(subset(data()$res5,prop>0)),". ")
   })
   output$tblSimple <- renderDT(
      head(data()$res5)[,-c(2)],options = list(lengthChange = FALSE)
   )
  # proxy <- dataTableProxy('tbl')
   output$tbl <- renderDT({
     # proxy %>% DT::selectRows(NULL)
      dt <- data()$res5
      if (FALSE)
         dt <- subset(dt,prop>0)
      slen <- length(which(dt$prop>0))
      cname <- c('1, id'="CF"
                ,'2, name'="Name"
                ,'3, represent'="Representation of selection"
                ,'4, target'="Target"
                ,'5, reached'=paste("Target achievement for",regionName)
                ,'6, selected'="Target achievement for selection"
                ,'7, prop'="Proportion of target achievement in selection"
                )
      colnames(dt) <- cname
      DT::datatable(dt,rownames=FALSE
                   ,extensions=c("ColReorder","Buttons")[-2]
                   ,options=list(pageLength=20
                                ,lengthMenu=sort(c(10,15,20,25,50,100,200,slen,nrow(dt)))
                                ,autowidth=TRUE
                               # ,lengthChange=FALSE
                                ,stateSave=TRUE
                                ,searchHighlight=TRUE
                                ,scrollX=TRUE
                                ,colReorder=TRUE
                                ,pagingType="first_last_numbers"
                                ,dom='lfrBtip'
                               # ,buttons=c("pdf","print","copy")
                                )
                   ,filter=c("none","bottom","top")[1]
                  # ,selection=c("multiple","single","none")[2]
                   ,selection=list(mode="single",selected=integer(),target="row")
                   ) %>%
         DT::formatStyle(cname[7]
                        ,background=styleColorBar(dt[,cname[7]],'palegreen')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[3]
                        ,background=styleColorBar(dt[,cname[3]],'lavender')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[5]
                        ,background=styleColorBar(dt[,cname[5]],'bisque')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[6]
                        ,background=styleColorBar(dt[,cname[6]],'beige')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[7],target="row"
                        ,color = styleInterval(c(0.00),c("DarkGrey","inherits"))) %>%
         DT::formatRound(cname[c(3,4,7)],4) %>%
         DT::formatPercentage(cname[c(5,6)],2)
   })
   output$question <- renderUI({
     # wd <- setwd("./resources");on.exit(setwd(wd))
      a1 <- tempfile()  ## "res1.html" tempfile()
      local_bib <- gsub("\\\\","/",file.path(tempdir(),"accenter.bib"))
      if (!file.exists(local_bib))
         download.file("https://nplatonov.github.io/platt.bib",local_bib)
     # file.copy(bib,"platt.bib")
      a1 <- rmarkdown::render('resources/question.Rmd'
                       ,output_format=rmarkdown::html_fragment()
                      # ,output_format=rmarkdown::html_vignette(css=NULL)
                       ,output_file=a1,quiet=!TRUE
                       ,params=list(prm=data()$res5
                                   ,kind=1L
                                   ,bib=local_bib
                                   )
                       )
      a2 <- scan(a1,what=character(),encoding="UTF-8",quiet=TRUE)
     # file.remove("platt.bib")
      file.remove(a1)
      HTML(a2)
   })
   
   ##~ output$dt_verbatim_dev <- renderPrint({
      ##~ input$tbl_rows_selected
     ##~ # sample(letters,3)
   ##~ })
   ##~ observeEvent(input$spath, {
      ##~ updateTabItems(session,"tabs","map")
   ##~ })
   ##~ observeEvent(input$rpath, {
      ##~ if (input$rpath==editName)
         ##~ updateTabItems(session,"tabs","map")
      ##~ else
         ##~ updateTabItems(session,"tabs","details")
   ##~ })
   ##~ observeEvent(input$epsg, {
      ##~ updateTabItems(session,"tabs","map")
   ##~ })
   observeEvent(input$tbl_rows_selected, {
      updateTabItems(session,"tabs","review")
      updateTabsetPanel(session,"tabset1","review")
   })
   ##~ observeEvent(input$selectLeaflet_click, {
      ##~ cat("----------------\n")
      ##~ str(input$selectLeaflet_click)
      ##~ cat("================\n")
      ##~ updateTabsetPanel(session,"tabset1","map")
   ##~ })
   observeEvent(input$selectLeaflet_shape_click, {
     # data_of_click$clickedMarker <- input$map_marker_click
     # message("*** LEAFLET CLICK ***")
      updateTabsetPanel(session,"tabset1","details")
   })
   observeEvent(input$viewerLeaflet_shape_click, {
      updateTabsetPanel(session,"tabset1","review")
   })
   observeEvent(input$'editor-map_shape_click', {
     # updateTabsetPanel(session,"tabset1","review") ## false switching during digitizing
   })
}
shinyApp(ui=uiTab,server=server)
