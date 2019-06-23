marxanSoft <- "D:/RAS/2019/PAMPAN/marxan"
marxanData <- "../common"
dstPath <- c(".","../results")[2]
source(file.path(marxanSoft,"marxan.R"))
'main' <- function(spf=0.7,blm=0.1,numitns=8*1e6,numreps=11,target=NA,sc="scXX") {
   dname <- paste0("sc"
                  ,"_spf=",format(spf,scientific=FALSE)
                  ,"_blm=",format(blm,scientific=FALSE)
                  ,"_numitns=",sprintf("%03d",numitns*1e-6)
                  )
   dname <- file.path(dstPath,sc,dname)
   if (dir.exists(dname)) {
      if (runAll <- TRUE)
         return(10L)
      else
         unlink(dname,recursive=TRUE)
   }
   print(dname)
  # return(20L)
   dir.create(dname,recursive=TRUE)
  # list1 <- dir(marxanData,recursive=TRUE) ## -- 20180323
   list1 <- dir(path=marxanData,pattern="\\.dat(\\.gz)*$",recursive=TRUE) ## ++ 20180323
  # print(list1)
   sapply(list1,function(x) {
      if (dirname(x)!=".") {
         d2<- file.path(dname,dirname(x))
         if (!dir.exists(d2))
            dir.create(d2,recursive=TRUE)
      }
     # print(x)
     # print(length(grep("\\.gz$",basename(x))))
      if (length(grep("\\.gz$",basename(x))))
     # print(c(src=file.path(marxanData,x),dst=file.path(dname,x)))
         system2("gzip",c("-f -d -c",file.path(marxanData,x))
                                    ,stdout=file.path(dname,gsub("\\.gz$","",x))
                                    ,stderr=FALSE)
      else
         file.copy(file.path(marxanData,x),file.path(dname,x),copy.date=TRUE)
   })
   wd <- setwd(dname);on.exit(setwd(wd))
   output <- gsub("^sc_","output_",basename(dname),ignore.case=TRUE)
   if (!dir.exists(output))
      dir.create(output)
   fspec <- file.path("input/spec.dat")
   fpuvspr <- file.path("input/puvspr.dat")
   prm <- replace_prm(BLM=blm
                     ,NUMITNS=numitns
                     ,NUMREPS=numreps
                     ,RANDSEED=sample(100:999,1)
                     ,OUTPUTDIR=output)
   print(extract_prm("outputdir"))
   if (!file.exists(fspec))
      stop(paste("File",dQuote(fspec),"is not found"))
   spec <- read_dat(fspec)
   if (F)
      spec$id <- sprintf("%04d",spec$id)
   print(head(spec))
   print(tail(spec))
   if ((is.numeric(spf))&&(length(spf)==1)) {
      print(c(spf=spf))
      spec$spf <- spf
   }
   if ((!is.null(spec$amount))&&(!is.na(target))&&(is.numeric(target))&&
       (length(target)==1)) {
      spec$prop <- target
      spec$target <- spec$amount*target
   }
   write_dat(spec,fspec)
   puvspr <- read_dat(fpuvspr)
   puvspr <- subset(puvspr,species %in% spec$id)
   write_dat(puvspr,fpuvspr)
   ret <- shell(file.path(marxanSoft,"marxan.exe"))
   if (ret)
      plutil::pause()
   system2("Rscript",list(file.path(marxanSoft,"_3a_pack-output.R")))
  # plutil::pause(TRUE)
   system2("Rscript",list(file.path(marxanSoft,"_3j-solutionmatrix.R"),"silent"))
  # plutil::pause(TRUE)
  # system2("Rscript",list(file.path(marxanSoft,"_3c-plot-puvspr.R"),"silent"))
  # plutil::pause(TRUE)
  # system("i_view32 /killmesoftly")
   list1 <- dir(recursive=TRUE,full.names=TRUE)
   ind <- .grep("^(debug|(bound|pu|puvspr)\\.dat|(bound|pulayer)\\.sqlite)"
               ,basename(list1))
   list2 <- list1[ind]
   file.remove(list2)
   list1 <- list1[-ind]
   sapply(list1[which(dirname(list1)!=".")],function(x) file.rename(x,basename(x)))
   list1 <- list.dirs()
   sapply(list1[list1!="."],function(x) unlink(basename(x),force=TRUE,recursive=TRUE))
   list1 <- grep("input.dat",dir(pattern="\\.dat$"),invert=TRUE,value=TRUE)
   sapply(list1,function(fname) system2("gzip",c("-9",fname)))
   0L
}
invisible({
   numreps <- 14
   scnum <- try(readLines(file.path(marxanData,"scenario.txt")))
   if (inherits(scnum,"try-error"))
      scnum <- "00"
   else
      scnum <- gsub("^(\\d+)\\D.*","\\1",scnum)
   if (is_SPF_BLM <- T) {
      spf <- c('1'=0.8,'2'=0.9,'3'=1.0,'4'=1.1,'5'=1.2,'6'=1.3,'7'=1.4,'8'=1.5
              ,'9'=1.6,'10'=1.7,'11'=1.8)[2:10]
     # blm <- c(-3,-2,-1,0,1)
      blm <- c('1'=0.0005,'2'=0.001,'3'=0.003,'4'=0.005,'5'=0.01,'6'=0.02,'7'=0.03
              ,'8'=0.04,'9'=0.06,'10'=0.1,'11'=0.3,'12'=1,'13'=3,'14'=13)[2:10]
      blmFix <- T # length(unique(round(diff(blm))))>1
     # blmFix <- !ursa:::.is.integer(blm)
      numitns <- 16*1e6
      sc <- paste0("sc",scnum,"-A-spf-blm/raw")
   }
   else if (is_NUMITNS <- T) {
      spf <- 1.2
      blm <- ifelse(T,-1.5,log(0.01)/log(10))
      blmFix <- FALSE
      numitns <- rev(as.integer(2^(seq(7)+1)*1e6))#[-c(1,2)]
      sc <- paste0("sc",scnum,"-B-numitns/raw")
   }
   else
      stop("undefined calibration mode")
   spf2 <- head(spf,-1)+diff(spf)/2
   blm2 <- head(blm,-1)+diff(blm)/2
   detail <- c(1,2,3)[1]
   if (detail==3) {
      blm <- sort(c(blm,blm2))
      spf <- sort(c(spf,spf2))
   }
   else if (detail==2) {
      blm <- blm2
      spf <- spf2
   }
   if (!blmFix)
      blm <- round(10^blm,5)
   prmList <- expand.grid(spf=spf,blm=blm,numitns=numitns,numreps=numreps,sc=sc
                         ,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)
   print(series(prmList))#;q()
  # plutil::pause()
   n <- nrow(prmList)
  # pb <- ursaProgressBar(min=0,max=n)
   sapply(sample(seq_len(n)),function(i) {
      do.call("main",prmList[i,])
     # setUrsaProgressBar(pb)
   })
  # close(pb)
   plutil::out_of_service(wait=300)
})
