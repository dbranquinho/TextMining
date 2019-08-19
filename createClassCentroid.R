# Plot Class from tree PSA
#
# variable for test into function
# classe <- "at2"
# myClass <- "./class"
# lfile <- "004062006at2.txt.idx"
# kfold <- 1


createClassCentroid <- function(kfold = 0.7) {
        source("loadCollection.R")
        if(!dir.exists("./statistic"))
                dir.create("./statistic")
        if(!dir.exists("./files2test"))
                dir.create("./files2test")
        niBkp <- read.csv("index/ni.idx",stringsAsFactors = FALSE, header = FALSE,
                          col.names = c("term","tf"),sep = ";", encoding = "UTF-8")
        for(classe in list.files(myClass)) {
                pb <- winProgressBar(title=sprintf("Centroid  creation process to %s",classe), 
                                     label="Initiating ...", min=0, max=100, initial=0)
                count = 0
                ni<-niBkp
                ni$mean <- 0
                total = 0
                outFiles <- character()
                countFold <- as.integer(length(list.files(paste0(myClass,"/",classe)))*kfold)
                countFoldF <- as.integer(length(list.files(paste0(myClass,"/",classe))))
                for(lfile in list.files(paste0(myClass,"/",classe))) {
                        if(!file.exists(paste0("./index/",lfile,".idx")))
                           next
                        doc <- read.csv(paste0("./index/",lfile,".idx"),
                                        stringsAsFactors = FALSE, 
                                        header = FALSE, col.names = c("term","tfidf"),
                                        sep = ";", encoding = "UTF-8")
                        total <- total + 1
                        info <- sprintf("%2.1f%% %d/%d %s", round(((total*100)/countFoldF),digits = 1),
                                        total,countFoldF,classe)
                        setWinProgressBar(pb, ((total*100)/countFoldF), label=info)
                        count = count + 1
                        if(count > countFold) {
                                outFiles <-rbind(outFiles,as.character(lfile))
                                next
                                }
                        for(i in 1:length(doc$term)) {
                                if (length(which(doc$term[i] == ni$term)) == 0) next 
                                val1 <- doc$tfidf[i]
                                val2 <- ni$mean[which(doc$term[i] == ni$term)]
                                ni$mean[which(doc$term[i] == ni$term)] <- 
                                        val2+(1/total)*(val1-val2)
                        }
                        }
                ni$i <- 1:length(ni$term)
                write.csv(ni,paste0("./statistic/",classe,".trn"))
                write.csv(outFiles,paste0("./files2test/",classe,".tst"))
                close(pb)
        }
}