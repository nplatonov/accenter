seed <- sample(100:999,1) ## 239 823
print(c(seed=seed))
set.seed(seed)
height <- "612px"
editName <- c("Custom AOI","*** INTERACTIVE ***","Preselected (dummy)")[1]
devel <- length(grep("^[A-Z]\\:/platt",Sys.getenv("USER")))>0 &
         dirname(normalizePath(file.path(getwd())))!="C:/tmp"
shortTab <- FALSE
tryToRefuseLeafletRendering <- FALSE && devel
p <- proc.time()
options(ursaTimeStart=p,ursaTimeDelta=p,stringsAsFactors=FALSE)
rm(p)
#css <- character()
#if (file.exists("www/custom.css")) {
#   css <- readLines("www/custom.css")
#   css <- paste(c("<style>",paste("   ",css),"</style>"),collapse="\n")
#}
initName <- "Initializing..."
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
'infoTab_deprecated' <- function() {
   opW <- options(warn=2) ## 2
   if (inherits(ret <- try(includeMarkdown("./resources/info.md")),"try-error"))
      ret <- helpText(paste("Welcome! For detail click to"
                          # ,as.character(icon("question"))
                           ,"'?'"
                           ,"icon on sidebar panel."))
   options(opW)
   ret
}
'infoTab' <- function(br) {
   if (missing(br))
      return("not ready")
  # opW <- options(warn=2) ## 2
   branch <- c("resources",gsub("(/\\.)+$","",br))
   dpath <- strsplit(branch,split="/")
   a <- character()
   for (i in seq_along(dpath)) {
      d <- dpath[[i]]
      for (j in seq_along(d)) {
         fname <- do.call(file.path,as.list(c(d[1:j],"description.md")))
         if (file.exists(fname))
            a <- c(a,readLines(fname,encoding="UTF-8"))
      }
   }
   if (length(a)) {
      ftemp <- tempfile()
      Fout <- file(ftemp,"wt",encoding="UTF-8")
      writeLines(a,ftemp)
      close(Fout)
      ret <- includeMarkdown(ftemp)
      file.remove(ftemp)
   }
   else
      ret <- helpText(paste("Welcome! For detail click to"
                          # ,as.character(icon("question"))
                           ,"'?'"
                           ,"icon on sidebar panel."))
  # options(opW)
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
   require(plotly)
   require(shinycssloaders)
  # require(leafpm)
  # requireNamespace("lwgeom") ## 20200428 deprecated 20200412 Shinyapps.io: Error building lwgeom (0.2-3)
})
if (devel)
   .elapsedTime("package load -- finish")
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
dpath <- "."
options(spinner.color="#ECF0F5") 
'polarmap' <- function(epsg,centered=TRUE,opacity=0.8,data=NULL) {
   isASDI <- TRUE
   if (is.character(centered)) {
      aoi <- centered
      centered <- TRUE
   }
   else
      aoi <- "Arctic"
   if (is.character(epsg))
      epsg <- as.integer(epsg)
   if (isASDI) {
      extent <- 4889334.802955
      scale0 <- 136421171.96428573131561279297
      resolution <- 0.28*1e-3*scale0/(2^seq(0,18))
      crsArctic <- leafletCRS(crsClass="L.Proj.CRS"
                           ,code=paste0("EPSG:",epsg)
                          # ,proj4def=ursa::spatial_crs(epsg)
                           ,proj4def=sf::st_crs(epsg)$proj4string
                           ,resolutions=resolution
                           ,origin=c(-extent,extent)
                           ,bounds=list(c(-extent,extent),c(extent,-extent))
                           )
      urlTemplate <- paste0("https://geoportal.arctic-sdi.org/action?"
                           ,"&action_route=GetLayerTile"
                           ,"&id=1"
                           ,"&layer=arctic_cascading"
                           ,"&style=default"
                           ,"&Service=WMTS"
                           ,"&Request=GetTile"
                           ,"&Version=1.0.0"
                           ,"&Format=image/png"
                           ,"&tilematrixset={tileMatrix}"
                           ,"&TileMatrix={z}"
                           ,"&TileCol={x}"
                           ,"&TileRow={y}"
                           )

      minZoom <- 1
      maxZoom <- 10
      grBasemap <- "Arcric SDI"
      attrBasemap <- "<a href=https://arctic-sdi.org/services/topografic-basemap/>Arctic SDI Topographic Basemap</a>"
   }
   else {
      extent <- 11000000 + 9036842.762 + 667
      origin <- c(-extent, extent)
      maxResolution <- 2*extent/256
      bounds <- list(c(-extent,extent),c(extent,-extent))
      resolutions <- purrr::map_dbl(0:18,function(x) maxResolution/(2^x))
      crsArctic <- leafletCRS(crsClass="L.Proj.CRS",code=paste0("EPSG:",epsg)
                             ,proj4def=sf::st_crs(epsg)$proj4string
                             ,resolutions=resolutions,origin=origin,bounds=bounds)
      minZoom <- 3
      maxZoom <- 9
   }
   if (F & devel)
      str(crsArctic)
   if (is.null(data))
      m <- leaflet(options=leafletOptions(crs=crsArctic,minZoom=minZoom,maxZoom=maxZoom))
   else
      m <- leaflet(data,options=leafletOptions(crs=crsArctic,minZoom=minZoom,maxZoom=maxZoom))
   if (centered) {
      if (epsg==3575)
         m <- setView(m,-100,80,minZoom+1)
      else if (epsg==3576)
         m <- setView(m,-100,82,minZoom+1)
      else if (epsg==3574)
         m <- setView(m,-40,82,minZoom+1)
      else if (epsg==3573)
         m <- setView(m,-100,84,minZoom+1)
      else if (epsg==3572)
         m <- setView(m,-150,86,minZoom+1)
      else if (epsg==3571) {
         if (aoi=="bering")
            m <- setView(m,160,60,minZoom+2)
         else
            m <- setView(m,180,87,minZoom+1)
      }
      else if (TRUE)
         m <- setView(m,0,90,minZoom+1)
        # m <- setView(m,12.57,55.687,12) ## Kopenhagen
   }
   if (isASDI) {
      m <- addTiles(m
                             ,urlTemplate=urlTemplate
                             ,options=tileOptions(tileMatrix=epsg
                                                          ,opacity=0.6
                                                          )
                            # ,opacity=0.3 ## UNABLE
                             ,attribution=attrBasemap
                             ,group=grBasemap
                             )
   }
   else {
      m <- addTiles(m,urlTemplate=paste0("https://{s}.tiles.arcticconnect.ca/osm_"
                                        ,epsg,"/{z}/{x}/{y}.png")
                   ,attribution="Map: \uA9 ArcticConnect. Data: \uA9 OpenStreetMap contributors"
                   ,options=tileOptions(subdomains="abc",opacity=opacity
                                       ,noWrap=TRUE,continuousWorld=FALSE))
   }
   if (FALSE) {
      cat("----------------- POLARMAP --------------------\n")
      str(m)
      cat("----------------- POLARMAP --------------------\n")
   }
  # if (!is.null(data))
  #    m <- addPolygons(m,data=data,weight=0.5)
  # m <- addGraticule(m,interval=10)
   m
}
#Fpu <- dir(path=mpath,pattern="^pulayer\\.shp\\.zip$",recursive=TRUE,full.names=TRUE)
#pu <- shapefile(Fpu)
'plotlyOpt' <- function() {
   fs <- 12
   axis <- list(tickfont=list(size=fs),titlefont=list(size=fs),zeroline=F,showgrid=T)
   legend <- list(font=list(size=round(0.9*fs)))
   title <- list(font=list(size=fs),color='yellow')
   layout <- list(NULL,title=title,legend=legend,xaxis=axis,yaxis=axis)
   config <- list(NULL
                 ,displaylogo=FALSE
                 ,displayModeBar=c("hover","true")[2]
                 ,scrollZoom=TRUE
                 ,modeBarButtonsToRemove=list(NULL
                                             ,"zoom2d","pan2d","toggleSpikelines"
                                             ,"zoomIn2d","zoomOut2d"
                                             ,"autoScale2d","resetScale2d"
                                            # ,"hoverClosestCartesian","hoverCompareCartesian"
                                             ,"select2d","lasso2d"
                                             )
                # ,modeBarButtonsToAdd=list("hoverCompareCartesian")
                 ,setBackground="orange"
                 ,toImageButtonOptions=list(width=760,height=540,scale=1)
                 )
   list(layout=layout,config=config)
}
