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
        library(stylo)
        library(XML)
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
        if(is.na(nr)) ntermo <- termo
        return(ntermo[[1]])  
}

stopword <- function(termo = termo) {
        nr <- which(termo == stopWords)[1]
        if(is.na(nr))
                return(FALSE)
        return(TRUE)
}
cleanStopword <- function(termos = termos, verbs = TRUE) {
        minSizeTerm <- as.integer(config$value[which(config$variable == "minSizeTerm")])
        iterm<-1
        nterm<-character()
        auxTermos <- strsplit(termos,split = " ")[[1]]
        for(i in 1:length(auxTermos)) {
                if(nchar(auxTermos[i]) < minSizeTerm) next
                if(verbs) {
                        print(auxTermos[i])
                        if(stopword(auxTermos[i])) {
                                next
                                }
                        }
                nterm[iterm] <- auxTermos[i]
                iterm=iterm+1
                }
        return(nterm)
}

