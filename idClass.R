# Identify class from commom terms into index files
#
source("loadCollection.R")

# Defining variables
#

lTerm <- data.frame()
doc <- loadCollection(collection)
nTerm <- data.frame()
iNTerm<-1

# Load files with factors to determine how many terms frequency from documents
#
for(classe in list.files(myClass,recursive = TRUE)) {
        for(i in 1:length(doc$docName)) {
                myTerm <-  read.table(paste0(myClass,"/",classe), stringsAsFactors=FALSE, 
                                      encoding="UTF-8", sep=";", quote="\"",col.names = c("term","tfidf"), 
                                      blank.lines.skip = TRUE)
        
                lTerm<-head(unique(order(myTerm$tfidf,decreasing = TRUE, 
                                         x = paste(myTerm$term,myTerm$tfidf))),10)
                for(i in 1:length(lTerm)) {
                        nTerm[[iNTerm]] <- cbind(myTerm$term[lTerm[i]],myTerm$tfidf[lTerm[i]])
                        iNTerm=iNTerm+1
                        }
                }
        }

