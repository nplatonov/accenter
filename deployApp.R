## ?rsconnect::deployApp
opW <- options(warn=10)
require(rsconnect)
options(rsconnect.http=c("rcurl","curl","internal")[1]
       ,rsconnect.check.certificate=FALSE
       ,rsconnect.http.verbose=FALSE)
account <- c("devel","release")[1]
opShiny <- getOption(switch(account[1],devel="rsconnect",release="rsconnectWWF"))
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
appname <- switch(account[1],devel="accenter",release="platini")
appfiles <- c("predefined","www","branch","app.R")
if (account %in% c("devel"))
   appfiles <- c("common","results","scenarios",appfiles)
if (TRUE) {
   list1 <- dir(path=appfiles,recursive=TRUE,full.names=TRUE)
   list1 <- list1[grep("(^_.+|^output.+|\\.(webp|png|R)$)",basename(list1),invert=TRUE)]
   list1 <- list1[grep("(spf|blm|numitns)",basename(dirname(list1)),invert=TRUE)]
   appfiles <- unique(c(list1,"resources/question.Rmd","app.R"))
}
deployApp(appName=appname,appFiles=appfiles,account=opShiny$name)
options(opW)
warnings()
