plutil::ursula()
invisible({
   scnum <- try(readLines(file.path("../common","scenario.txt")))
   if (inherits(scnum,"try-error"))
      scnum <- "35"
   else
      scnum <- gsub("^(\\d+)\\D.*","\\1",scnum)
   dpath <- paste0("../results/sc",scnum,"-A-spf-blm")
   list1 <- spatial_dir(path=dpath,pattern="freq.shp",recursive=TRUE)
   list2 <- basename(dirname(list1))
   spf <- as.numeric(gsub(".*spf=((_|-)*\\d+(\\.\\d+)*).*","\\1",list2))
   blm <- as.numeric(gsub(".*blm=((_)*\\d+(\\.\\d+)*).*","\\1",list2))
   spfU <- rev(sort(unique(spf)))
   blmU <- sort(unique(blm))
   pu <- ursa_read("../common/pulayer/pulayer.tif")
   if (dev <- F) {
      f <- ursa:::.fasterize(sample(list1,1),by="sum")
      print(f)
      hist(ursa(f,"value"))
      q()
   }
  # print(pu)
  # PU <- spatialize("../marxan/pulayer/pulayer.shp")
  # print(series(spatial_data(PU)))
   leg <- c("right"
           ,lapply(seq_along(spfU),function(i) list(i,"left"))
           ,lapply(seq_along(blmU),function(j) list("bottom",j)))
  # str(leg)
   compose_open(layout=c(length(blmU),length(spfU)),legend=leg,dev=F
               ,fileout=file.path(dpath,"summary","freq-matrix.png"))
   ct <- NULL
   pb <- ursaProgressBar(list1)
   cat("plot ")
   for (y in spfU) {
      for (x in blmU) {
         cat(".")
        # print(c(blm=x,spf=y))
         i <- which(blm==x & spf==y)
         if (!is.numeric(i)) {
            panel_new("white")
            setUrsaProgressBar(pb)
            next
         }
         f <- ursa:::.fasterize(list1[i],by="sum")
         if (F) {
            hist(ursa_value(f))
         }
         else {
            panel_new("white")
            if (is.null(ct))
               ct <- colorize(f)
            panel_raster(f,colortable=ct)
           # f <- spatialize(list1[i])
           # print(series(spatial_data(f)))
           # q()
         }
         setUrsaProgressBar(pb)
      }
   }
   close(pb)
   cat(" done!\n")
  # q()
  # legend_mtext("dummy freq")
   legend_colorbar(ct,units="freq")
   for (i in seq_along(spfU))
      legend_mtext(as.character(spfU[i]))
   for (i in seq_along(blmU))
      legend_mtext(as.character(blmU[i]))
   compose_close(bpp=8)
})
