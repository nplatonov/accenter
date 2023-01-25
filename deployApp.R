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
   if (length(ind <- grep("^upload",arglist)))
      deploy <- TRUE
}
if (deploy) {
   if (length(grep("devel",R.Version()$status,ignore.case=TRUE)))
      stop("R-devel is not supported on shinyapps.io (seems to)")
   require(rsconnect)
   options(rsconnect.dummy=NULL
         # ,rsconnect.http=c("rcurl","curl","internal")[2]
          ,rsconnect.check.certificate=FALSE
          ,rsconnect.http.verbose=FALSE
          )
   opShiny <- getOption(switch(account[1],release="rsconnectMartin","rsconnect"))
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
appname <- switch(account[1]
                 ,release=c("accenter","platini")[1]
                 ,c("accenter","openday")[1])
extrafiles <- c()
appfiles <- c("www","app.R") ## -"predefined"
extralist <- c("default","description.md")
ind <- grep("default",extralist)
if (account %in% c("devel")) {
   if (includeDefaultBranch <- TRUE) {
      br <- "branch"
      bf <- file.path(br,extralist)
      b <- try(readLines(bf[ind]))
      if (!inherits(b,"try-error")) {
         extrafiles <- c(extrafiles,bf)
         br <- file.path(br,b)
         bf <- file.path(br,extralist)
         b <- try(readLines(bf[ind]))
         if (!inherits(b,"try-error")) {
            extrafiles <- c(extrafiles,bf)
            br <- file.path(br,b)
            bf <- file.path(br,extralist)
            b <- try(readLines(bf[ind]))
            if (!inherits(b,"try-error")) {
            extrafiles <- c(extrafiles,bf)
            br <- file.path(br,b)
            }
         }
      }
      appfiles <- c(br,appfiles)
   }
   if (T & !includeDefaultBranch)
      appfiles <- c("common","results","scenarios",appfiles)
}
if (account %in% c("release"))
   appfiles <- c("branch",appfiles)
if (TRUE) {
   list1 <- dir(path=appfiles,recursive=TRUE,full.names=TRUE)
   list1 <- list1[grep("(^_.+|^output.+|\\.(webp|png|R)$)",basename(list1),invert=TRUE)]
   list1 <- list1[grep("(spf|blm|numitns)",basename(dirname(list1)),invert=TRUE)]
  # print(unique(basename(dirname(list1))))
   list1 <- list1[grep("~$",basename(dirname(list1)),invert=TRUE)]
  # print(unique(basename(dirname(list1))))
   appfiles <- unique(c(list1,"resources/question.Rmd"
                       ,dir(path="resources",pattern="\\S\\.R$",full.names=TRUE)
                       ,"app.R"))
   if (T | !(account %in% c("devel")))
      appfiles <- c(appfiles,"resources/description.md")
}
appfiles <- c(appfiles,extrafiles)
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
