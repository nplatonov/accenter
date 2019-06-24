## ?rsconnect::deployApp
opW <- options(warn=10)
require(rsconnect)
options(rsconnect.http=c("rcurl","curl","internal")[1]
       ,rsconnect.check.certificate=FALSE
       ,rsconnect.http.verbose=FALSE)
opShiny <- getOption("rsconnect")
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
with(opShiny,setAccountInfo(name=name,token=token,secret=secret))
appname <- "accenter"
appfiles <- c("common","predefined","results","scenarios","app.R")
if (TRUE) {
   list1 <- dir(path=appfiles,recursive=TRUE,full.names=TRUE)
   list1 <- list1[grep("(^_.+|^output.+|\\.(webp|png|R)$)",basename(list1),invert=TRUE)]
   list1 <- list1[grep("(spf|blm|numitns)",basename(dirname(list1)),invert=TRUE)]
   appfiles <- unique(c(list1,"app.R"))
}
deployApp(appName=appname,appFiles=appfiles,account=opShiny$name)
options(opW)
warnings()
