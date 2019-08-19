collection = "./collection"; tmpDir = "temp"; idxDir = "index"; cleanFiles = TRUE
lfiles<-"d1.txt"

# make sure about this file

source("./preProcess.R")

iFile <- function(collection = "", tmpDir = "temp", idxDir = "index", cleanFiles = TRUE
                  lemmaWords = FALSE, stopingWords = FALSE) {
        if(cleanFiles) {
                if(dir.exists(tmpDir)) {
                        unlink(tmpDir,recursive = TRUE, force = TRUE)
                        }
                if(dir.exists(idxDir)) {
                        unlink(idxDir,recursive = TRUE, force = TRUE)
                        }
        }
        if(collection == "") {
                return("Sixtax ERROR: must be indicated the directory to read")
        }
                
        if(!dir.exists(tmpDir)) {
                dir.create(tmpDir)
        }
        if(!dir.exists(idxDir)) {
                dir.create(idxDir)
        }
        collectionClean<-paste0(collection,"Clean") # to write a clean file
        if(!dir.exists(collectionClean)) {
                dir.create(collectionClean)
        }
        # Creating Dataset model with data from files
        myDataset<-list()
        indexFiles<-list()
        i<-1
        iTermos<-1
        termos <- character()
        
        # Read collection and create full term vector
        for(lfiles in list.files(collection)) {
                texto<-paste0(read.delim2(paste0(collection,"/",lfiles), 
                                  stringsAsFactors = FALSE))
                texto <- cleanText(texto)
                # Clean new vector Terms from unecessary words (stopwords)
                #if(lemmaWords)
                #        texto <- lapply(strsplit(texto, split = " "),lemmatizer)
                #if(stopingWords)
                #        texto <-cleanStopword(texto, FALSE)
                write.table(x = as.data.frame(strsplit(texto," "),col.names = "term"),
                            file = paste0(collectionClean,"/",lfiles), quote = FALSE, 
                            row.names = FALSE, sep = ";",col.names = "terms")
                termos <- paste(termos,texto)
                }
        # Unique vector terms created to support math
        termos <- as.list(strsplit(termos, " "))
        termos <- as.array(sort(unique(termos[[1]])))
        fCollection <- list()
        i<-1
        # Load a list with all clean files
        for(lfiles in list.files(collectionClean)) {
                fCollection[[i]] <-read.table(paste0(collectionClean,"/",lfiles),strip.white = TRUE, 
                                  stringsAsFactors = FALSE, header = TRUE)
                i=i+1
                }
        for(lfiles in list.files(collectionClean)) {
                for(i in 1:length(termos)[1]) {
                        
                        }
                }
        print("File index created!")
}
