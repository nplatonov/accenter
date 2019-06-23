plutil::ursula(3)
invisible({
   size <- 90
   mode <- c("best","mean","freq")[3]
   scnum <- try(readLines(file.path("../common","scenario.txt")))
   if (inherits(scnum,"try-error"))
      scnum <- "35"
   else
      scnum <- gsub("^(\\d+)\\D.*","\\1",scnum)
   dpath <- paste0("../results/sc",scnum,"-A-spf-blm")
   print(dpath)
   list1 <- list.dirs(path=dpath)
   list1 <- unique(list1[grep("^sc",basename(list1))])
   ind <- sapply(list1,function(d) {
      length(spatial_dir(path=d,pattern="freq.shp"))==1
   })
   list1 <- list1[ind]
   list2 <- basename(list1)
   indV <- which(!list2 %in% basename(dpath))
   list1 <- list1[indV]
   list2 <- list2[indV]
   dpath <- unique(dirname(list1))
   spf <- as.numeric(gsub(".*spf=((_|-)*\\d+(\\.\\d+)*).*","\\1",list2))
   blm <- as.numeric(gsub(".*blm=((_)*\\d+(\\.\\d+)*).*","\\1",list2))
  # print(format(blm,sci=FALSE),quote=F);q()
   blmL <- round(log(blm)/log(10),2)
   da <- data.frame(x=blmL,y=spf,value=NA)
   if (F) {
      message("                                 <<< subsetting for debug")
     # da <- subset(da,x>)
   }
   str(da)
   xo <- sort(unique(da$x))
   yo <- sort(unique(da$y))
   da$x <- match(da$x,xo)
   da$y <- match(da$y,yo)
   xs <- seq_along(xo)
   ys <- seq_along(yo)
   names(xs) <- as.numeric(format(10^xo,digits=2))
   names(ys) <- yo
   prmList <- c('1'="Score",'2'="Cost",'3'="Planning_Units"
               ,'4'="Connectivity",'skip1'="Connectivity_Total"
               ,'skip2'="Connectivity_In",'skip3'="Connectivity_Edge"
               ,'skip4'="Connectivity_Out",'skip5'="Connectivity_In_Fraction"
               ,'5'="Penalty",'6'="Shortfall",'7'="Missing_Values",'8'="MPM"
               ,'9'="Selection_Frequency")#['7']
   prmList <- prmList[grep("skip",names(prmList),invert=TRUE)]
   prmList <- grep("freq",prmList,value=TRUE,ignore.case=TRUE,invert=!(mode %in% "freq"))
  # if (!FALSE)
  #    prmList <- prmList[c('9')]
   for (j in seq_along(prmList)) {
      prm <- prmList[j]
      message(prm,":")
      message(rep("~",nchar(prm)))
      if (isFreq <- length(grep("Freq",prm))>0) {
         pb <- ursaProgressBar(min=0,max=nrow(da),title="Extract frequency")
         session_grid(NULL)
      }
      for (i in sample(seq_len(nrow(da)))) {
         if (!isFreq) {
            sname <- dir(path=list1[i],pattern="output_sum\\.csv(\\.gz$)$"
                        ,full.names=TRUE)
            if (length(sname)!=1) {
               da[i,"value"] <- NA
            }
            else {
               score <- read.csv(sname)
               if (i==-1)
                  print(score)
               if (mode %in% "best")
                  da[i,"value"] <- score[which.min(score$Score),prm]
               else if (mode %in% "mean")
                  da[i,"value"] <- apply(score[,prm,drop=FALSE],2,mean)
            }
         }
         else {
            if (!FALSE) { ## not optimal due to excess of geometry
              # message(i,": ",list1[i])
               ta <- table(spatialize(spatial_dir(path=list1[i]
                                              ,pattern="freq\\."),engine="sf")$sum)
            }
            else { ## more longtime than spatial reading
               res <- read.csv(dir(path=list1[i],pattern="output_solution.+\\.csv(\\.gz$)$"
                                    ,full.names=TRUE),header=TRUE)
               ta <- table(colSums(res[,-1]))
              # print(ta)
            }
            da[i,"value"] <- sum(c(head(ta,1),tail(ta,1)))/sum(ta)
            setUrsaProgressBar(pb)
         }
      }
      if (F) {
         saveRDS(da,"MPM_mean.rds")
         q()
      }
      if (isFreq)
         close(pb)
      expand <- 0
      surf2 <- akima::interp(da[,"x"],da[,"y"],da[,"value"]
                            ,xo=seq(min(xs)-0.5*expand,max(xs)+0.5*expand,by=1/10)
                            ,yo=seq(min(ys)-0.5*expand,max(ys)+0.5*expand,by=1/10)
                            ,linear=TRUE,extrap=TRUE,duplicate="mean")
      a <- as.ursa(surf2,flip=TRUE)
      g0 <- regrid(expand=1.05)
      attr(g0$seqx,"lab") <- xs
      attr(g0$seqy,"lab") <- ys
      attr(g0$seqx,"units") <- "BLM"
      attr(g0$seqy,"units") <- "SPF"
      session_grid(g0)
      p <- colorize(a,ramp=FALSE,interval=TRUE,stretch="eq",lazy=TRUE)
     # print(ursa(p,"colortable"))
      compose_open(p,legend=list("top","right"),dev=FALSE #,width=550
                  ,fileout=sprintf("%s/cal%02d%s_%s.png"
                                  ,gsub("raw","summary",dpath)
                                  ,as.integer(names(prm)),mode,prm))
      for (i in seq(p)) {
         panel_new()
        # panel_plot(p[i])
         panel_contour(p[i],"line fill label",lwd=0.1,cex=0.7)
        # panel_points(da$x,da$y,pch=15,col="#FFFFFF20",cex=4)
         panel_text(da$x,da$y,labels=format(da$value,scientific=FALSE,digits=3),srt=0)
        # panel_annotation(x=da$x[15],y=da$y[15],label=sprintf("%.1f",da$residual[15]))
         if (length(p)>1)
            panel_annotation(names(p)[i],pos="bottomright")
         panel_decor()#lon=seq(-3,4,by=0.5),lat=seq(0,1,by=0.1),grid.verbose=TRUE)
        # panel_decor(lon=seq(-3,4,by=0.5),lat=seq(0,1,by=0.1),grid.verbose=TRUE)
      }
      compose_legend(list(gsub("_"," ",prm),p))
     # compose_legend(prm,p)
     # compose_legend(prm)
     # compose_legend(p)
      compose_close(open=j==length(prmList),bpp=8)
   }
})
