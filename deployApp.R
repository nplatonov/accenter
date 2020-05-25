## ?rsconnect::deployApp
deploy <- T
opW <- options(warn=10)
account <- c("devel","release","emulate")[2]
arglist <- commandArgs(TRUE)
if (length(arglist)) {
   if (length(ind <- grep("dev",arglist)))
      account <- "devel"
   else if (length(ind <- grep("rel",arglist)))
      account <- "release"
   else if (length(ind <- grep("emu",arglist)))
      account <- "emulate"
   if (length(ind <- grep("^local",arglist)))
      deploy <- FALSE
}
if (deploy) {
   if (length(grep("devel",R.Version()$status,ignore.case=TRUE)))
      stop("R-devel is not supported on shinyapps.io (seems to)")
   require(rsconnect)
   options(rsconnect.dummy=NULL
          ,rsconnect.http=c("rcurl","curl","internal")[2]
          ,rsconnect.check.certificate=FALSE
          ,rsconnect.http.verbose=FALSE
          )
   opShiny <- getOption(switch(account[1],release="rsconnectWWF","rsconnect"))
   if (is.null(opShiny)) {
      message("Expected record of 'rsconnect' option (example):")
      opShiny <- list(name="yourname"
                     ,token=paste(sample(c(0:9,LETTERS[seq(6)]),32,rep=TRUE),collapse="")
                     ,secret=paste(sample(c(0:9,LETTERS,letters,"+"),40,rep=TRUE),collapse="")
                     )
      str(opShiny)
      opShiny <- NULL
      stop("Authentification data are not receieved")
   }
   #str(opShiny)
   res <- try(with(opShiny,setAccountInfo(name=name,token=token,secret=secret)))
   if (inherits(res,"try-error")) {
      message(attr(res,"condition")$message)
      stop()
   }
}
appname <- switch(account[1],release="platini",c("accenter","openday")[1])
appfiles <- c("www","branch","app.R") ## -"predefined"
if (account %in% c("devel"))
   appfiles <- c("common","results","scenarios",appfiles)
if (TRUE) {
   list1 <- dir(path=appfiles,recursive=TRUE,full.names=TRUE)
   list1 <- list1[grep("(^_.+|^output.+|\\.(webp|png|R)$)",basename(list1),invert=TRUE)]
   list1 <- list1[grep("(spf|blm|numitns)",basename(dirname(list1)),invert=TRUE)]
   appfiles <- unique(c(list1,"resources/question.Rmd"
                       ,dir(path="resources",pattern="\\S\\.R$",full.names=TRUE)
                       ,"app.R"))
   if (!(account %in% c("devel")))
      appfiles <- c(appfiles,"resources/info.md")
}
if (!deploy) {
   dpath <- paste0("C:/tmp/",appname)
   if (dir.exists(dpath))
      unlink(dpath,recursive=TRUE,force=TRUE)
   invisible(lapply(sample(appfiles),function(src) {
      if (dir.exists(src)) {
         lapply(dir(path=src,recursive=TRUE,full.names=TRUE), function(f) {
            dst <- file.path(dpath,f)
            if (!dir.exists(dirname(dst)))
               dir.create(dirname(dst),recursive=TRUE)
            file.copy(src,dst,copy.date=TRUE)
         })
      }
      else {
         dst <- file.path(dpath,src)
         if (!dir.exists(dirname(dst)))
            dir.create(dirname(dst),recursive=TRUE)
         file.copy(src,dst,copy.date=TRUE)
      }
   }))
}
if (deploy)
   deployApp(appName=appname,appFiles=appfiles,account=opShiny$name)
options(opW)
warnings()
