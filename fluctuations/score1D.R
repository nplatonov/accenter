plutil::ursula()
invisible({
   dpath <- c('1'="."
             ,'2'="../results/sc00-A-spf-blm/raw"
             ,'3'="../results/sc00-B-numitns/raw"
             )[c(1,3)]
   list1 <- list.dirs(path=dpath)
   list1 <- unique(list1[grep("^sc",basename(list1))])
   list2 <- basename(list1)
   indV <- which(!list2 %in% basename(dpath))
   list1 <- list1[indV]
   list2 <- list2[indV]
   dpath <- unique(dirname(list1))
   print(dpath)
   spf <- as.numeric(gsub(".*spf=((-)*\\d+(\\.\\d+)*).*","\\1",list2))
   blm <- as.numeric(gsub(".*blm=((-)*\\d+(\\.\\d+)*).*","\\1",list2))
   numitns <- as.numeric(gsub(".*numitns=((-)*\\d+(\\.\\d+)*).*","\\1",list2))
   blm <- round(log(blm)/log(10))
   da <- data.frame(x=blm,y=spf,z=numitns,best=NA)
   da$value <- vector("list",nrow(da))
   prmList <- c('1'="Score",'2'="Cost",'3'="Planning_Units"
               ,'4'="Connectivity",'skip1'="Connectivity_Total"
               ,'skip2'="Connectivity_In",'skip3'="Connectivity_Edge"
               ,'skip4'="Connectivity_Out",'skip5'="Connectivity_In_Fraction"
               ,'5'="Penalty",'6'="Shortfall",'7'="Missing_Values",'8'="MPM"
               ,'9'="Selection_Frequency")
   prmList <- prmList[grep("skip",names(prmList),invert=TRUE)]
  # if (TRUE)
  #    prmList <- prmList[c('8','9')]
   require(ggplot2)
   isPNG <- TRUE
   for (j in seq_along(prmList)) {
      prm <- prmList[j]
      da2 <- NULL
      if (isFreq <- length(grep("Freq",prm))>0)
         pb <- ursaProgressBar(min=0,max=nrow(da),title="Extract frequency")
      for (i in seq_len(nrow(da))) {
         score <- read.csv(dir(path=list1[i],pattern="output_sum\\.csv(\\.gz$)$"
                              ,full.names=TRUE))
         da[i,"best"] <- which.min(score$Score)
         if (!isFreq) {
            sc <- score[,prm]
         }
         else {
           # res <- spatial_data(spatialize(spatial_dir(path=list1[i])))
            ta <- table(spatial_data(spatialize(spatial_dir(path=list1[i]),engine="sf"))$sum)
            sc <- sum(c(head(ta,1),tail(ta,1)))/sum(ta)
            sc <- rep(sc,nrow(score))
            sc <- jitter(sc,factor=0.01)
            setUrsaProgressBar(pb)
         }
         if (i==1)
            print(sc)
         da[[i,"value"]] <- sc
         best <- rep("other",length(sc))
         ind <- which.min(score$Score)
         best[ind] <- "best"
         da3 <- data.frame(x=da$x[i],y=da$y[i],z=da$z[i],logz=log(da$z[i])/log(2)
                         # ,best=da$best[i]
                          ,best=c(best,"best")
                          ,prm=unname(prm),run=c(seq_along(sc),0)
                          ,value=c(sc,sc[ind]))
         da2 <- rbind(da2,da3)
      }
      if (isFreq)
         close(pb)
      if (TRUE)
         da2 <- subset(da2,run>0)
      fname <- sprintf("%s/viol%02d_%s.png"
                      ,gsub("raw","summary",dpath)
                      ,as.integer(names(prm)),prm)
      if (isPNG)
         png(filename=fname,width=960,height=640,units="px",type=c("cairo","windows")[1])
      p1 <- ggplot(da2,aes(factor(z),value))+ #,colour=best))+
            theme_bw(base_size=22)+
            labs(y="",x="Millions iterations",title=gsub("_"," ",prm))+
            geom_violin(trim=FALSE,colour="wheat3",adjust=1.0,fill="transparent")+
            geom_jitter(size=3,width=0.2,height=0.0,aes(alpha=best))+
            scale_alpha_discrete(range=c(1.0,0.2))+
            guides(alpha=guide_legend(title="Score:"))+
            theme(legend.position="right",legend.box="vertical")+
            NULL
      print(p1)
      if (isPNG)
         dev.off()
   }
   system2("open",fname)
})
