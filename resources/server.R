#' ---
#' spin: false
#' ---
#'
server <- function(input,output,session) {
   if (T & devel)
      session$onSessionEnded(stopApp)
   exchange <- reactiveValues(edits=NULL,puvspr=NULL,spec=NULL#,prob=NULL
                             ,freq=NULL,freq3=NULL,overlay=NULL,res=NULL)
  # scenario <- observeEvent(input$spath,{
   branch <- reactive({
      prm <- parseQueryString(session$clientData$url_search)
      if (devel) {
        # prm <- list(branch="pampan/4.2")
         prm <- list(branch="pampan/4.2.1")
        # prm <- list(branch="pampan/4.2.1",region="60")
      }
      if (devel)
         .elapsedTime("branch (reactive) -- start")
      branch <- prm[['branch']]
      if (F & is.null(branch))
         branch <- "pampan"
      bpath <- dpath
      complete <- dir.exists(file.path(bpath,"results")) & is.null(branch)
      if (!complete) {
         bpath <- file.path(dpath,"branch")
         if (is.character(branch)) {
            bpath1 <- file.path(bpath,branch)
            if (dir.exists(bpath1))
               bpath <- bpath1
            else {
               bpath1 <- file.path(bpath,dirname(branch))
               if (dir.exists(bpath1))
                  bpath <- bpath1
            }
         }
         complete <- dir.exists(file.path(bpath,"results"))
         if (!complete) {
            dname <- file.path(bpath,"default")
            if (file.exists(dname))
               bsub <- readLines(dname)
            else
               bsub <- sample(list.dirs(dirname(dname),recursive=FALSE
                                       ,full.names=FALSE),1)
            bpath <- file.path(bpath,bsub)
            complete <- dir.exists(file.path(bpath,"results"))
            if (!complete) {
               dname <- file.path(bpath,"default")
               if (file.exists(dname))
                  bsub <- readLines(dname)
               else
                  bsub <- sample(list.dirs(dirname(dname),recursive=FALSE
                                          ,full.names=FALSE),1)
               bpath <- file.path(bpath,bsub)
               complete <- dir.exists(file.path(bpath,"results"))
            }
         }
      }
      hpath <- c(".","..","../..","results/sc01")#,reserve,file.path(reserve,".."))
      hpath <- c(file.path(bpath,hpath),dpath)
      hpath2 <- normalizePath(hpath,winslash="/",mustWork=FALSE)
      hpath <- hpath[grep(tail(hpath2,1),hpath2)]
      mpath <- file.path(hpath,"common")
      mpath <- mpath[dir.exists(mpath)] #[1]
      rpath <- file.path(hpath,"predefined")
      rpath <- rpath[dir.exists(rpath)][1]
      rname0 <- dir(path=rpath,pattern="\\.(geojson|sqlite|shp)(\\.(zip))*$",full.names=TRUE)
      rname1 <- c(editName,gsub("\\.shp(\\.zip)*","",basename(rname0)))#[1]
      regname <- prm[['region']]
      if (!is.null(regname)) {
         if (length(rname2 <- grep(regname,basename(rname0),value=TRUE))==1)
            rname1 <- gsub("\\.shp(\\.zip)*","",rname2)
      }
      Fpuvspr <- lapply(mpath,function(mpath2)
                          dir(path=mpath2,pattern="^puvspr\\.dat(\\.(gz|zip))*$"
                          ,recursive=TRUE,full.names=TRUE))
      indM <- which(sapply(Fpuvspr,function(x) length(x)>0))[1]
      rm(Fpuvspr)
      Fpu <- lapply(mpath,function(mpath2)
                          dir(path=mpath2,pattern="^pulayer\\.shp\\.zip$"
                          ,recursive=TRUE,full.names=TRUE))
      Fpu <- Fpu[[which(sapply(Fpu,function(x) length(x)>0))[1]]]
      mpath <- mpath[indM]
      str(data.frame(dpath=dpath,bpath=bpath,mpath=mpath,Fpu=Fpu))
     # print(c(dpath=dpath,bpath=bpath,mpath=mpath),quote=FALSE)
      pu <- shapefile(Fpu) ## Fpu
      gpath <- file.path(bpath,hpath,"results")
      gpath <- gpath[dir.exists(gpath)][1]
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
      mdname <- dir(path=file.path(gpath,"..","scenarios"),pattern="\\.xlsx$",full.names=TRUE)
      if (googleDrive <- length(mdname)!=1) {
         cpath <- file.path(bpath,hpath,"scenarios")
         cpath <- cpath[dir.exists(cpath)][1]
         gsheetFile <- file.path(cpath,"gsheet.id")
         if (file.exists(gsheetFile))
            gsheet <- readLines(gsheetFile,warn=FALSE)
         else {
            gsheetFile <- dir(path=cpath,pattern="\\.gsheet$"
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
      sheets <- readxl::excel_sheets(mdname)
      subsetSheet <- grep("(subset|metadata)",sheets,value=TRUE,ignore.case=TRUE)
      scname <- readxl::read_excel(mdname,sheet="Scenarios")
      cfname <- readxl::read_excel(mdname,sheet="CF_list")
      slname <- readxl::read_excel(mdname,sheet=subsetSheet)
      if (googleDrive)
         file.remove(mdname)
      scname <- scname[,c("#","Name")]
      cfname <- cfname[,c("CF_code","File_name","CF_name")]
      if (!("Distinctive" %in% colnames(slname)))
         slname$Distinctive <- NA
      if (!("Representative" %in% colnames(slname)))
         slname$Representative <- NA
     # if (!("CF_code" %in% colnames(slname)))
      if (!length(grep("cf.*code",colnames(slname),ignore.case=TRUE)))
         slname$CF_code <- as.integer(gsub("^(\\d+)\\D.*","\\1",slname$File_name))
     # slname <- slname[,c("CF_code","Distinctive","Representative")]
      redis <- rep("",nrow(slname))
      redis[!is.na(slname$Distinctive) & is.na(slname$Representative) &
            slname$Distinctive] <- "D"
      redis[is.na(slname$Distinctive) & !is.na(slname$Representative) &
            slname$Representative] <- "R"
      redis[!is.na(slname$Distinctive) & !is.na(slname$Representative) &
            slname$Representative & slname$Distinctive] <- "RD"
      redis <- data.frame(CF_code=as.integer(slname$CF_code),redis=redis)
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
      regionName <- "Study area"
      aoi <- "Arctic"
      epsgList <- paste0("EPSG:",3571:3576)
      epsg <- "EPSG:3575"
      if (length(grep("(fareast|bering)",bpath,ignore.case=TRUE))) {
         regionName <- "Russian Far East area"
         aoi <- "bering"
         epsgList <- "EPSG:3571"
         epsg <- "EPSG:3571"
      }
      projectName <- regionName
      if (length(grep("(pampan|arcnet)",bpath,ignore.case=TRUE)))
         projectName <- "ArcNet"
      cat("---------------\n")
      print(rname1)
      cat("---------------\n")
      updateSelectInput(session,"spath",choices=sname0,selected=sample(sname0,1))
      updateSelectInput(session,"rpath"
                       ,choices=if ((length(rname1)==1)&&(rname1!=editName))
                             rname1
                          else
                             list('Custom AOI'=rname1[1],'Preselected'=rname1[-1])
                       ,selected=ifelse(length(rname1)==1,rname1,sample(rname1,1))
                       )
      updateSelectInput(session,"epsg",choices=epsgList,selected=epsg)
      if (devel)
         .elapsedTime("branch (reactive) -- finish")
      ret <- list(gpath=gpath,sname0=sname0,spath0=spath0,cfname=cfname,pu=pu
                 ,mpath=mpath,rname0=rname0,region=regionName,project=projectName
                 ,aoi=aoi,redis=redis)#,epsg=epsg)
      ret
   })
   scenario_noAssign <- observe({ ## 'scenario' without assignment 
      if (devel)
         .elapsedTime("scenario (observe) -- start")
      ##~ input$freq
      action <- ifelse(input$spath==initName,"Branch initialization"
                                            ,"Data verification")
      showModal(modalDialog(title=paste(action,"in progress")
                           ,"Please wait",size="s",easyClose = TRUE,footer = NULL))
     # .elapsedTime("observe -- start")
      epsg <- if (input$epsg==initName) NA else input$epsg
      gpath <- branch()$gpath
      spath0 <- branch()$spath0
      sname0 <- branch()$sname0
      ind <- match(input$spath,sname0)
      if (is.na(ind)) {
         ind <- sample(seq_along(sname0),1)
      }
      spath <- file.path(gpath,spath0[ind])
      if (FALSE) {
         cat("------------\n")
         print(spath0)
         print(c(gpath=gpath))
         print(c('input$spath'=input$spath))
         print(sname0)
         print(spath)
         cat("------------\n")
      }
      freq <- shapefile(file.path(spath,"freq.shp.zip"))
      pu <- branch()$pu
      freq$ID <- pu$ID
      isFreq <- length(grep("(freq|sum)",input$freq,ignore.case=TRUE))>0
      isThreshold <- (isFreq)&&(length(grep("\\d$",input$freq))>0)
      if (isFreq) {
         if (isThreshold) {
           # th <- gsub(".*(\\d+(\\.\\d+)*)$","\\1",input$freq)
            th <- as.numeric(gsub("^.*\\D(\\d(\\.\\d+$))","\\1",input$freq))
            v <- freq$sum/max(freq$sum)
            v[v<th] <- 0
            v[v>0] <- 1
            cat("-0519-1---------\n")
            print(table(v))
            freq$value <- v
            freq$sum <- v
            cat("-0519-1---------\n")
         }
         else
            freq$value <- freq$sum/max(freq$sum) ## subject to be overestimated
      }
      else {
         freq$value <- freq$best
         freq$sum <- freq$best
      }
      cat("-0519-2---------\n")
      print(summary(freq$value))
      print(summary(freq$sum))
      cat("-0519-2---------\n")
     # prob <- freq
     # sf::st_geometry(prob) <- NULL
      Fspec <- dir(path=spath,pattern="spec\\.dat(\\.gz)*$"
                  ,recursive=TRUE,full.names=TRUE)
      opW <- options(encoding="UTF-8")
      spec <- read.csv(Fspec,sep=",") #,encoding="UTF-8") ## encoding="windows-1251")
      options(opW)
      spec$prop <- spec$target/spec$amount
      redis1 <- branch()$redis
      if (devel) {
         str(redis1)
         str(redis1$redis)
      }
      ind2 <- match(spec$id,redis1$CF_code)
      redis2 <- rep("",nrow(spec))
      redis2[which(!is.na(ind2))] <- redis1$redis[c(na.omit(ind2))]
      spec$redis <- redis2
      Fpuvspr <- dir(path=spath,pattern="puvspr\\.dat(\\.gz)*$"
                    ,recursive=TRUE,full.names=TRUE)
      if (!length(Fpuvspr))
         Fpuvspr <- dir(path=branch()$mpath,pattern="puvspr\\.dat(\\.gz)*$"
                       ,recursive=TRUE,full.names=TRUE)
      puvspr <- read.csv(Fpuvspr)
     # freq <- aggregate(freq,by=list(freq$sum),mean)["sum"]
      Fagr <- file.path(spath,"freq-aggregate.shp.zip")
      if ((isThreshold)&&(file.exists(Fagr))) {
         freq3 <- sf::st_transform(shapefile(Fagr),4326)
         cat("0519-3-----------\n")
         print(th)
         print(summary(freq3$sum))
         v <- freq3$sum/max(freq3$sum)
         v[v<th] <- 0
         v[v>0] <- 1
         freq3$sum <- v
         print(summary(freq3$sum))
         cat("0519-3-----------\n")
      }
      else if ((isFreq)&&(file.exists(Fagr))) {
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
         m <- polarmap(as.integer(gsub("\\D","",epsg)),centered=branch()$aoi)
         pal <- colorNumeric(palette="plasma",domain=freq3$sum)
         m <- addPolygons(m,data=freq3["sum"]
                         ,color=~pal(sum)
                         ,weight=0
                        # ,popup=~sum
                         ,label=~as.character(sum)
                         ,stroke=TRUE
                        # ,weight=0.5
                         ,fillOpacity=0.3
                        # ,highlightOptions=highlightOptions(fillOpacity=0)
                         )
         m <- addLegend(m
                       ,position="topleft"
                       ,pal=pal
                      # ,values=seq(nrow(da[[i]]))
                       ,values=freq3[["sum"]]
                      # ,colors=as.character(attr(da2,"colortable"))
                      # ,labels=as.numeric(names(attr(da2,"colortable")))
                       ,opacity=0.5,title=input$freq
                       ,group="freq"
                       )
         if (devel)
            .elapsedTime("   mapedit leaflet -- finish")
         if (input$rpath==editName) {
            edits <- callModule(editMod,"editor",leafmap=m
                               ,editor=c("leaflet.extras","leafpm")[2]
                               ,editorOptions=list(
                                    toolbarOptions=leafpm::pmToolbarOptions(position="topright"))
                               )
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
         rname0 <- branch()$rname0
        # cat("---------\n")
        # print(gsub("\\.shp(\\.zip)*$","",basename(rname0)))
        # print(input$rpath)
         rname <- rname0[grep(paste0("^",input$rpath,"$")
                             ,gsub("\\.shp(\\.zip)*$","",basename(rname0)))]
        # print(rname)
        # cat("---------\n")
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
      if (!all(sf::st_is_valid(s))) { ## &&("lwgeom" %in% loadedNamespaces())
        showNotification(id="valid",closeButton=FALSE,duration=120,"Make geometry valid..."
                        ,type="warning")
         s <- sf::st_make_valid(s)
         removeNotification(id="valid")
      }
      s <- sf::st_transform(sf::st_geometry(s),sf::st_crs(freq))
     # sf::st_agr(s) <- "constant"
      sf::st_agr(freq) <- "constant"
     # res <- sf::st_intersection(freq,s)
      print(c(spatial_count=length(sf::st_geometry(s))))
      gridded <- length(grep("pampan/4.2.1",branch()$mpath))>0
      careful <- gridded | length(sf::st_geometry(s))!=1
      print(c(careful=careful))
      showModal(modalDialog(title = "Find intersection...","Not long to finish"
                           ,size="s",easyClose = TRUE,footer = NULL))
      if (devel) {
         file.remove(dir(path="./shiny.log",pattern="^res.+\\.(cpg|dbf|prj|shp|shx)"
                        ,full.names=TRUE))
         sf::st_write(freq,"./shiny.log/res-freq.shp",quiet=TRUE)
         sf::st_write(s,"./shiny.log/res-s.shp",quiet=TRUE)
      }
      if (careful) {
         res <- sf::st_intersection(freq,s)[c("ID","sum")]
         if (gridded)
            res <- res[sf::st_area(res)>units::as_units(99,"m^2"),]
      }
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
     # names(redis2) <- spec$id
     # redis <- redis$redis[redis$CF_code %in% spec$id]
     # str(redis)
      ##~ rcol <- rep("black",length(redis))
      ##~ rcol[redis %in% c("Representative")] <- "red"
      ##~ rcol[redis %in% c("Distinctive")] <- "blue"
      ##~ rcol[redis %in% c("Distinctive and Representative"
                        ##~ ,"Representative and Distinctive")] <- "magenta"
     # dt[,2] <- paste0("<span style=",dQuote(paste0("color: ",rcol,";")),">",dt[,2],"</span>")
      ##~ rval <- rep("",length(redis))
      ##~ rval[redis %in% c("Representative")] <- "R"
      ##~ rval[redis %in% c("Distinctive")] <- "D"
      ##~ rval[redis %in% c("Distinctive and Representative"
                       ##~ ,"Representative and Distinctive")] <- "RD"
      res5 <- with(spec,data.frame(id=id ## 1
                                  ,name=name # "should be specified in English" # runame ## 2
                                  ,RD=redis ## 3
                                  ,represent=represent/amount ## 4
                                  ,target=prop ## 5
                                  ,reached=reached/amount/prop ## 6
                                  ,selected=selected/amount/prop ## 7
                                  ))
     # cat("--------------------------\n")
     # str(res5name)
     # cat("==========================\n")
      cfname <- branch()$cfname
      res5$name <- cfname$CF_name[match(res5$id,as.integer(cfname$CF_code))]
      res5$prop <- with(res5,selected/reached) ## 8
      if (TRUE)
         res5 <- res5[with(res5,order(represent,prop,decreasing=TRUE)),]
      if (TRUE)
         res5 <- res5[res5$target>0,]
      exchange$res <- res
      if (devel)
         .elapsedTime("data (reactive) -- finish")
      list(res=res,freq=freq,freq3=exchange$freq3,res5=res5)
   })
   output$uiMap <- renderUI({
      req(input$epsg!=initName)
     # req(!inherits(try(input$rpath),"try-error"))
      print("uiMap -- BEGIN")
      if (editName %in% input$rpath) {
         ret <- editModUI("editor") %>% withSpinner() #,height=height)
      }
      else {
         ret <- leafletOutput("viewerLeaflet") %>% withSpinner() #,height=height)
      }
      print("uiMap -- END")
      ret
   })
   output$viewerLeaflet <- renderLeaflet({
      if (devel)
         .elapsedTime("occurrence leaflet -- init")
      req(input$epsg!=initName)
      epsg <- as.integer(gsub("\\D","",input$epsg))
      freq <- data()$freq
      freq3 <- data()$freq3
      predefined <- exchange$overlay
      req(nrow(freq))
      if (F & !nrow(freq)) {
         m <- polarmap(epsg)
         return(m)
      }
     # req(nrow(predefined))
      req(length(sf::st_geometry(predefined)))
      if (devel)
         .elapsedTime("occurrence leaflet -- start")
     # showNotification(closeButton=FALSE,duration=12
     #                 ,"Data processing in progress",type="warning")
      showNotification(id="leaflet",closeButton=FALSE,duration=120
                      ,"Render leaflet (view)...",type="warning")
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
      grROI <- input$rpath
      m <- polarmap(epsg,centered=branch()$aoi)
      if (input$freq=="Frequency")
        # pal <- colorNumeric(palette="YlOrRd",domain=freq3$sum,reverse=TRUE)
         pal <- colorNumeric(palette="plasma",domain=freq3$sum,reverse=F)
      else
         pal <- colorFactor(palette="plasma",domain=freq3$sum,reverse=F)
      m <- addPolygons(m,data=freq3["sum"]
                      ,color=~pal(sum)
                      ,weight=0
                     # ,popup=~sum
                      ,label=~as.character(sum)
                      ,stroke=TRUE
                     # ,weight=0.5
                      ,fillOpacity=0.3
                     # ,highlightOptions=highlightOptions(fillOpacity=0)
                      ,group=grROI
                      )
      m <- addPolygons(m
                      ,data=predefined2
                      ,color="#070"
                      ,fillColor="#0A0"
                      ,weight=2
                      ,fillOpacity=0.4
                      ,label=if (input$rpath!=editName) input$rpath else NULL
                      ,highlightOptions=highlightOptions(weight=3
                                         ,color="#070"
                                         ,fillColor="#0F0"
                                         ,opacity=1
                                         )
                      )
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
      m <- addLegend(m
                    ,position="topleft"
                    ,pal=pal
                   # ,values=seq(nrow(da[[i]]))
                    ,values=freq3[["sum"]]
                   # ,colors=as.character(attr(da2,"colortable"))
                   # ,labels=as.numeric(names(attr(da2,"colortable")))
                    ,opacity=0.3,title=input$freq
                    ,group=grROI
                    )
      m <- leafem::addHomeButton(m
                                ,ext=matrix(ursa::spatial_bbox(predefined2),ncol=2)
                                ,group=grROI
                               # ,position=pos
                                )
      m <- leafem::addHomeButton(m
                                ,ext=c(-45,50,135,50)
                                ,group=branch()$aoi
                                )
     # print("LEAFLET PREDEFINED -- finish")
      if (devel)
         .elapsedTime("occurrence leaflet -- finish")
      removeNotification(id="leaflet")
      m
   })
   output$selectLeaflet <- renderLeaflet({
      d <- data()$res
      req(nrow(d)>0)
      showNotification(id="leaflet",closeButton=FALSE,duration=120
                      ,"Render leaflet (select)...",type="warning")
      on.exit(removeNotification(id="leaflet"))
      ind <- input$tbl_rows_selected
      withOverlap <- !is.null(ind)
      grROI <- "Selection"
      if (devel)
         .elapsedTime("review leaflet -- start")
      if (T) {
         pu <- branch()$pu
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
         grCol <- substr(as.character(ursa::colorize("A"
                             ,pal.dark=91,pal.light=91,pal.hue=2)$colortable),1,7)
        # pal <- colorFactor(as.character(grCol),domain=factor(1))
         m <- addPolygons(m
                         ,data=d
                         ,weight=0.5
                         ,color=grCol
                         ,opacity=0.9
                         ,label="click for swithing to details"
                         ,highlightOptions=highlightOptions(weight=2,opacity=1)
                         ,group=grROI
                         )
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
      m <- addLegend(m
                    ,position="topleft"
                   # ,pal=pal
                    ,labels=grROI
                    ,colors=grCol
                    ,opacity=0.7
                   # ,title=grROI
                    ,group=grROI
                    )
      m <- leafem::addHomeButton(m
                                ,ext=matrix(ursa::spatial_bbox(d),ncol=2)
                                ,group=grROI
                               # ,position=pos
                                )
      grName <- paste("CF",data()$res5[ind,"id"])
      for (i in ind) {
         j <- match(i,ind)
         dt <- data()$res5[i,]
         ind2 <- which(as.integer(puvspr$species) %in% as.integer(dt$id))
         res <- puvspr[ind2,]
         grCol <- as.character(ursa::colorize(res$amount
                      ,ramp=FALSE,pal.dark=63,pal.light=191)$colortable)
         ind3 <- which(pu$ID %in% res$pu)
         sf::st_geometry(res) <- sf::st_geometry(pu[ind3,])
        # pal <- colorNumeric(palette="plasma",domain=res$amount)
         if (length(unique(res$amount))>7)
            pal <- colorNumeric(as.character(grCol),domain=range(res$amount))
         else
            pal <- colorFactor(as.character(grCol)
                          # ,n=length(unique(res$amount))
                           ,domain=unique(res$amount))
         res <- sf::st_transform(res,4326)
         m <- addPolygons(m,data=res
                         ,color=~pal(amount)
                         ,weight=0 # 0.5
                        # ,popup=~sum
                         ,label=~as.character(round(amount,2))
                         ,stroke=TRUE
                        # ,weight=0.5
                         ,fillOpacity=0.7
                        # ,highlightOptions=highlightOptions(fillOpacity=0)
                         ,group=grName[j]
                         )
         m <- addLegend(m
                       ,position="topleft"
                       ,pal=pal
                      # ,values=seq(nrow(da[[i]]))
                       ,values=res$amount
                      # ,colors=as.character(attr(da2,"colortable"))
                      # ,labels=as.numeric(names(attr(da2,"colortable")))
                       ,opacity=0.7
                       ,title=grName[j]
                       ,group=grName[j]
                       )
         m <- leafem::addHomeButton(m
                                   ,ext=matrix(ursa::spatial_bbox(res),ncol=2)
                                   ,group=grName[j]
                                  # ,position=pos
                                   )
      }
      m <- addLayersControl(m
                           ,overlayGroups=c(grROI,grName)
                           ,options=layersControlOptions(collapsed=FALSE)
                           )  
      
      m
   })
   output$plotlyHist <- renderPlotly({
      roi <- ifelse(grepl("^PAC\\s\\d+$",input$rpath)>0
                   ,input$rpath,"Selection")
      d <- data()
      da <- rbind(data.frame(freq="Study Area",value=d$freq$sum)
                 ,data.frame(freq=roi,value=d$res$sum))
      p <- plot_ly(da,x=~value,type="histogram",histnorm="probability",split=~freq)
     # prm <- plotlyOpt()
     # prm$config[[1]] <- prm$layout[[1]] <- p
     # p <- do.call("layout",prm$layout)
      
      p <- layout(p
                 ,legend=list(x=0.45,y=0.9)
                 ,title=list(text=paste("Comparison of planning units distibution\n"
                                       ,"in Study Area and",roi)
                            ,font=list(size=12))
                 ,xaxis=list(title="Selection Frequency",zeroline=F
                            ,titlefont=list(size=12))
                 ,yaxis=list(title="Occurrence"
                            ,titlefont=list(size=12))
                 )
      p <- config(p
                 ,displaylogo=FALSE
                 ,scrollZoom=TRUE)
     # p <- do.call("config",prm$config)
      p
   })
   output$plotlyBox <- renderPlotly({
      roi <- ifelse(grepl("^PAC\\s\\d+$",input$rpath)>0
                   ,input$rpath,"Selection")
      d <- data()
      da <- rbind(data.frame(freq="Study Area",value=d$freq$sum)
                 ,data.frame(freq=roi,value=d$res$sum))
      p <- plot_ly(da,y=~value,type="box",split=~freq,showlegend=F)
     # prm <- plotlyOpt()
     # prm$config[[1]] <- prm$layout[[1]] <- p
     # p <- do.call("layout",prm$layout)
      p <- layout(p
                 ,legend=list(orientation="v")
                 ,xaxis=list(title="")
                 ,yaxis=list(title="Selection Frequency",zeroline=F)
                 )
      p <- config(p
                 ,displaylogo=FALSE
                 ,scrollZoom=TRUE)
      # p <- do.call("config",prm$config)
      p
   })
   output$plotlyDesc <- renderText({
      roi <- ifelse(grepl("^PAC\\s\\d+$",input$rpath)>0
                   ,input$rpath,"current selection")
      val <- paste("Comparison distribution (by frequency or by choices)"
                  ,"of selected cells in Study Area and",roi)
      val
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
      roi <- ifelse(grepl("^PAC\\s\\d+$",input$rpath)>0
                   ,input$rpath,"current selection")
      paste0("Total planning units in ",branch()$region,": ",nrow(branch()$pu),". "
            ,"Planning units in ",roi,": ",nrow(data()$res),".")
   })
   output$species <- renderText({
      roi <- ifelse(grepl("^PAC\\s\\d+$",input$rpath)>0
                   ,input$rpath,"current selection")
      paste0("Conservation features in ",dQuote(branch()$project)," project: "
           # ,length(unique(exchange$puvspr$species)),". " ## --20210115
            ,length(unique(exchange$spec$id)),". " ## ++20210115
            ,"Conservation features in the ",dQuote(input$spath)," scenario: "
            ,nrow(data()$res5),". "
            ,"Conservation features in ",roi,": "
            ,nrow(subset(data()$res5,prop>0)),". ")
   })
   output$tblSimple <- renderDT({
      head(data()$res5)[,-c(2)]
   },options = list(lengthChange = FALSE))
  # proxy <- dataTableProxy('tbl')
   output$tbl <- renderDT({
     # proxy %>% DT::selectRows(NULL)
      dt <- data()$res5
      if (FALSE)
         dt <- subset(dt,prop>0)
      if (branch()$project=="ArcNet" & grepl("^PAC\\s\\d+$",input$rpath)>0) {
         dt$id <- paste0("<a href='"
                        ,"https://nplatonov.github.io/chicory/" ## subject to replace
                        ,"cf/s"
                        ,sapply(paste0("cf",dt$id),digest::digest,"crc32")
                        ,".html' target=_blank>"
                        ,dt$id,"</a>")
      }
      slen <- length(which(dt$prop>0))
      lmenu <- sort(c(5,7,10,15,20,25,50,100,200,slen,nrow(dt)))
      lmenu <- lmenu[lmenu<=nrow(dt)]
      if (F)
         cname <- c('1, id'="CF"
                   ,'2, name'="Name" ## имячко
                   ,'3, R/D'="Repre\uADsen\uADta\uADtive\u2009/\u2009Dis\uADtinc\uADtive"
                   ,'4, represent'="Repre\uADsen\uADta\uADtion in se\uADlec\uADtion"
                   ,'5, target'="Con\uADser\uADva\uADtion Tar\uADget"
                   ,'6, reached'=paste("Tar\uADget achie\uADve\uADment for","( ***study area ***)")
                   ,'7, selected'="Tar\uADget achie\uADve\uADment for se\uADlec\uADtion"
                   ,'8, prop'="Propor\uADti\uADon of tar\uADget achieve\uADment in se\uADlec\uADtion"
                   )
      else 
         cname <- tabname()$colnames
      if (F)
         cname <- gsub("\uAD","",cname)
      colnames(dt) <- cname
      if (devel) {
         writeLines(rjson::toJSON(dt),"./shiny.log/table.json")
         saveRDS(dt,"./shiny.log/table.rds")
      }
     # rownames(dt) <- dt$CF
     # if (T)
     #    dt$CF <- NULL
      da <- DT::datatable(dt
                   ,rownames=FALSE # is.null(dt$CF)
                   ,escape=FALSE
                   ,extensions=c("Scroller","FixedHeader","Buttons","Responsive")[4]
                   ,options=list(dom='iftlp' ## scroller 'itf', 'Bfrtip', Accenter 'lfrBtip'
                                ,pageLength=7 # nrow(dt)
                                ,lengthMenu=lmenu
                               # ,autowidth=TRUE
                               # ,lengthChange=FALSE
                                ,stateSave=TRUE
                                ,searchHighlight=TRUE
                               # ,pagingType="first_last_numbers"
                               # ,colReorder=T ## comment for use with responsive 
                                ,scrollX=F
                               # ,scrollY="calc(100vh - 250px)"
                               # ,deferRender=F
                               # ,scroller=F ## scroller
                                ,responsive=T
                                ,columnDefs=list(list(responsivePriority=1,targets=0)
                                                ,list(responsivePriority=2,targets=3)
                                                ,list(responsivePriority=3,targets=7)
                                                ,list(responsivePriority=4,targets=6)
                                                ,list(responsivePriority=5,targets=5)
                                                ,list(responsivePriority=6,targets=4)
                                                ,list(responsivePriority=7,targets=2)
                                                )
                               # ,fixedHeader=F
                               # ,buttons=c("pdf","print","copy")
                                )
                  # ,filter=c("none","bottom","top")[1]
                  # ,selection=c("multiple","single","none")[2]
                   ,selection=list(mode="multiple",selected=integer(),target=c("cell","row")[2])
                  # ,caption="Datatable caption is here"
                   ) %>%
        # DT::formatString(cname[2]
        #                ) %>%
         DT::formatStyle(cname[8]
                        ,background=styleColorBar(dt[,cname[8]],'palegreen')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[4]
                        ,background=styleColorBar(dt[,cname[4]],'lavender')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[6]
                        ,background=styleColorBar(dt[,cname[6]],'bisque')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[7]
                        ,background=styleColorBar(dt[,cname[7]],'beige')
                        ,backgroundSize='95% 18pt'
                        ,backgroundRepeat='no-repeat'
                        ,backgroundPosition='center'
                        ) %>%
         DT::formatStyle(cname[8],target="row"
                        ,color = styleInterval(c(0.00),c("DarkGrey","inherits"))) %>%
         DT::formatStyle(0,cursor="pointer") %>%
         DT::formatRound(cname[c(4,5,8)],4) %>%
        # DT::formatPercentage(cname[c(6,7)],2)
         DT::formatPercentage(cname[c(4,5,6,7,8)],1)
      da
   })
   output$about <- renderUI({
      br <- dirname(branch()$gpath)
      infoTab(br)
   })
   output$question <- renderUI({
     # wd <- setwd("./resources");on.exit(setwd(wd))
      a1 <- tempfile() ## "C:/tmp/res1.html" tempfile()
      local_bib <- gsub("\\\\","/",file.path(tempdir(),"accenter.bib"))
     # local_bib <- "C:/tmp/accenter.bib"
      if (!file.exists(local_bib))
         download.file("https://nplatonov.github.io/platt.bib",local_bib)
     # file.copy(bib,"platt.bib")
      tbl <- try(data()$res5)
      if (inherits(tbl,"try-error"))
         tbl <- NULL
      a1 <- rmarkdown::render('resources/question.Rmd'
                       ,output_format=rmarkdown::html_fragment()
                      # ,output_format=rmarkdown::html_vignette(css=NULL)
                       ,output_file=a1,quiet=!TRUE
                       ,params=list(prm=tbl
                                   ,kind=1L
                                   ,bib=local_bib
                                   )
                       )
      a2 <- scan(a1,what=character(),encoding="UTF-8",quiet=TRUE)
     # file.remove("platt.bib")
      file.remove(a1)
      HTML(a2)
   })
   output$selectedScenario <- renderText({
      paste0("<b>Scenario</b><br/>",input$spath)
   })
   output$selectedEPSG <- renderText({
      paste0("<b>Map projection </b><br/>",input$epsg)
   })
   output$geopu <- renderUI({
      leafletOutput("geopuLeaflet") %>% withSpinner()
   })
   output$geopuLeaflet <- renderLeaflet({
      puvspr <- exchange$puvspr
      if (devel) {
         cat("-------------- puvspr\n");str(puvspr)
      }
      freq <- exchange$freq
      freq$rep <- freq$dis <- 0
      if (devel) {
         cat("-------------- freq\n");str(freq)
      }
      spec <- exchange$spec
      if (devel) {
         cat("-------------- spec\n");str(spec)  
      }
      puvsprR <- puvspr[puvspr$species %in% spec[grep("R",spec$redis),"id"],]
      puvsprD <- puvspr[puvspr$species %in% spec[grep("D",spec$redis),"id"],]
      if (devel) {
         cat("-------------- puvsprR\n");str(puvsprR)
      }
      if (nrow(puvsprR)) {
         resR <- aggregate(puvsprR[,c("amount","coverage")]
                          ,by=list(pu=puvsprR$pu),sum)
         if (devel) {
            cat("-------------- resR\n");str(resR)
         }
         freq$rep[match(resR$pu,freq$ID)] <- resR$coverage
      }
      if (nrow(puvsprD)) {
         resD <- aggregate(puvsprD[,c("amount","coverage")]
                          ,by=list(pu=puvsprD$pu),sum)
         freq$dis[match(resD$pu,freq$ID)] <- resD$coverage
      }
      if (devel)
         cat("==============\n")
      epsg <- input$epsg
     # str(input$puprm)
      showNotification(id="leafletpu",closeButton=FALSE,duration=25
                      ,"Wait: render leaflet (params)...",type="warning")
      if (input$puprm=="Data coverage")
         freq$plot <- freq$coverage
      else if (input$puprm=="Layer overlap")
         freq$plot <- freq$layers
      else if (input$puprm=="Representative")
         freq$plot <- freq$rep
      else if (input$puprm=="Distinctive")
         freq$plot <- freq$dis
      else
         freq$plot <- freq$sum
      freq <- freq[!is.na(freq$plot),]
      freq <- sf::st_transform(freq,4326)
      m <- polarmap(as.integer(gsub("\\D","",epsg)),centered=branch()$aoi)
      col <- as.character(ursa::colorize(freq$plot
                          ,pal.dark=63,pal.light=191,pal.hue=2)$colortable)
      pal <- colorNumeric(palette=col,domain=range(freq$plot),reverse=F)
      if (T) {
         .elapsedTime("geopuLeaflet render -- start")
         m <- addPolygons(m,data=freq["plot"]
                         ,color=~pal(plot)
                         ,weight=0
                        # ,popup=~sum
                         ,label=~as.character(plot)
                         ,stroke=TRUE
                        # ,weight=0.5
                         ,fillOpacity=0.7
                        # ,highlightOptions=highlightOptions(fillOpacity=0)
                         ,group="prop"
                         )
         m <- addLegend(m
                       ,position="topleft"
                       ,pal=pal
                      # ,values=seq(nrow(da[[i]]))
                       ,values=freq[["plot"]]
                      # ,colors=as.character(attr(da2,"colortable"))
                      # ,labels=as.numeric(names(attr(da2,"colortable")))
                       ,opacity=0.7
                       ,title=input$puprm
                       ,group="prop"
                       )
         .elapsedTime("geopuLeaflet render -- finish")
      }
      removeNotification(id="leafletpu")
      m
   })
   output$external <- renderMenu({
      if (devel | branch()$project=="ArcNet")
         sidebarMenu(
            menuItem(text="ArcNet"
                     ,href="https://arcticwwf.org/work/ocean/arcnet/"
                     ,icon=icon("globe-americas")
                    # ,badgeColor="blue"
                    # ,badgeName="www"
                     )
         )
   })
   tabname <- reactive({
      isPAC <- branch()$project=="ArcNet" & input$rpath!=editName
      dtname <- c('1, id'="CF ID"# 
                 ,'2, name'="CF Name"
                 ,'3, R/D'="Repre&shy;sen&shy;ta&shy;tive\u2009/\u2009Dis&shy;tinc&shy;tive"
                 ,'4, represent'="Pro&shy;por&shy;tion in the se&shy;lec&shy;tion"
                 ,'5, target'="Con&shy;ser&shy;va&shy;tion Tar&shy;get"
                 ,'6, reached'="Tar&shy;get achie&shy;ve&shy;ment for the study area"
                 ,'7, selected'="Tar&shy;get achie&shy;ve&shy;ment for se&shy;lec&shy;tion"
                 ,'8, prop'="Pro&shy;por&shy;tion of tar&shy;get achie&shy;ve&shy;ment in se&shy;lec&shy;tion"
                 )
      if (isPAC) {
          dtname[4] <- "Pro&shy;por&shy;tion in the PAC"
          dtname[7] <- "Cont&shy;ri&shy;bu&shy;tion to ArcNet Tar&shy;get Achie&shy;ve&shy;ment"
          dtname[8] <- "PAC's Cont&shy;ri&shy;bu&shy;tion to the Achie&shy;ved Tar&shy;get"
      }
      ret <- list(NULL
                # ,Map=paste(ifelse(isPAC,input$rpath,"Select area-of-interest"),"on map")
                 ,Map=ifelse(isPAC,paste(input$rpath,"on the map"),"Select area-of-interest on map")
                 ,Details=paste("Conservation features in"
                               ,ifelse(isPAC,input$rpath,"Selection"))
                 ,Review=paste(ifelse(isPAC,input$rpath,"Selection")
                             # ,"results overview"
                              ,"overview"
                              )
                 ,colnames=unname(dtname)
                 )
      ret
   })
   output$tabReview = renderText(tabname()$Review)
   output$tabDetails = renderText(tabname()$Details)
   output$tabMap = renderText(tabname()$Map)
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
   if (F)
      observeEvent(input$tbl_cell_clicked, {
         info <- input$tbl_cell_clicked ## $col, $row, $value
         cat("_cell_clicked:\n")
         str(info)
         cat("_cells_selected:\n")
         print(input$tbl_cells_selected)
         cat("_tbl_rows_selected:\n")
         print(input$tbl_rows_selected)
         cat("_tbl_row_last_clicked:\n")
         print(input$tbl_row_last_clicked)
         cat(" ---------- selected/clicked end ----------\n")
         return(NULL)
         if (is.null(info$value) || info$col != 1)
            return(NULL)
         updateTabItems(session,"tabs","review")
         updateTabsetPanel(session,"tabset1","review")
      })
   if (F)
      observeEvent(input$tbl_rows_selected, {
         if (length(input$tbl_rows_selected)!=1)
            return(NULL)
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
      updateTabsetPanel(session,"tabset1",c("details","review")[1])
   })
   observeEvent(input$'editor-map_shape_click', {
     # updateTabsetPanel(session,"tabset1","review") ## false switching during digitizing
   })
}
