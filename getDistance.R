# Bringing up the distance from two space vectors
#

getDistance <- function(f1 = f1, f2 = f2, mode = "e", centroid = FALSE) {
        source("loadConfig.R")
        file1 <- data.frame()
        file2 <- data.frame()
        file1 <- read.table(f1, stringsAsFactors=FALSE, encoding="UTF-8", sep=";", 
                            quote="\"",col.names = c("term","tfidf"), blank.lines.skip = TRUE)
        if(centroid) {
                ni   <- read.csv(paste0(statistic,"/",f2))
                file2 <- data.frame(ni$term,ni$mean)
                colnames(file2)<-c("term","mean")
                file2 <- subset(file2, mean > 0)
                }
        if(!centroid) {
                file2 <- read.table(f2, stringsAsFactors=FALSE, encoding="UTF-8", sep=";", 
                                    quote="\"",col.names = c("term","tfidf"), blank.lines.skip = TRUE)
                }
        soma<-0L
        aux<-c(file1$term,file2$term)
        termo<-sort(unique(aux))
        tam<-length(termo)
        if(mode=="e") {
                for(nr in 1:tam) {
                        val1<-which(termo[nr] == file1)   
                        if(!length(val1)) 
                                val1<-0
                        else
                                val1<-file1$tfidf[which(termo[nr] == file1)]
                        val2<-which(termo[nr] == file2)
                        if(!length(val2)) 
                                val2<-0
                        else
                                val2<-file2$tfidf[which(termo[nr] == file2)]
                        soma<-soma + (val1 - val2)^2
                }
                return(sqrt(soma))
        }
        if(mode=="c") {
                sumab<-0
                suma<-0
                sumb<-0
                for(nr in 1:tam) {
                        a<-which(termo[nr] == file1)   
                        if(!length(a)) 
                                a<-0
                        else
                                a<-file1$tfidf[which(termo[nr] == file1)]
                        b<-which(termo[nr] == file2)
                        if(!length(b)) 
                                b<-0
                        else
                                b<-file2$tfidf[which(termo[nr] == file2)]
                        sumab=sumab+(a*b)
                        suma=suma+a^2
                        sumb=sumb+b^2
                }
                return((sumab)/(sqrt(suma)*sqrt(sumb)))
        }
        
}