# Load files to lemmarizer text and stopwords to pre process
#


if(length(grep("lemmaWords", ls())) == 0) {
        print("Reading support files")
        source("./loadSupport.R")
}

if(length(grep("stopWords", ls())) == 0) {
        print("Reading support files")
        source("./loadSupport.R")
}


cleanText <- function(htmlString) {
        ntext <-character()
        ntext <- delete.markup(htmlString, markup.type = "plain")
        ntext <- tolower(ntext)
        ntext <- gsub("<.*?>", "", ntext)
        ntext <- gsub("\n", "", ntext)
        ntext <- gsub("\'", "", ntext)
        ntext <- gsub("\"", "", ntext)
        ntext <- gsub("\"", "", ntext)
        ntext <- gsub("\"", "", ntext)
        ntext <- gsub("\\b\\d+\\b",  "", ntext)
        ntext <- gsub("[[:punct:]]", "", ntext)
        return(ntext)
}

lemmatizer <- function ( termo = termo) {
        ntermo <- character()
        nr     <- grep(termo,lemmaWords)[1]
        ntermo <- lemmaWords[[nr]][1,]
        return(ntermo[[1]])  
}

stopword <- function(termo = termo) {
        nr <- which(termo == stopWords)[1]
        if(is.na(nr))
                return(FALSE)
        return(TRUE)
}
cleanStopword <- function(termos = termos, verbs = TRUE) {
        minSizeTerm <- config$value[which(config$variable == "minSizeTerm")]
        iterm<-1
        nterm<-character()
        for(i in 1:length(termos)[1]) {
                if(nchar(termos[i]) < minSizeTerm) next
                if(verbs) {
                        if(stopword(termos[i])) {
                                nterm[iterm] <- termos[i]
                                iterm=iterm+1
                                }
                        }
                }
        return(nterm)
}

