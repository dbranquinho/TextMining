# Create or update centroid from a Class 
#


iClassFile <- function(className = className, filename = filename, 
                       action = "insert", cc = FALSE) {
        
        if(!dir.exists("class")) {
                dir.create("class")
        }
        className<-paste0("./class","/",className)
        if(cc == TRUE) {
                if(!dir.exists(className)) {
                        dir.create(className)
                }
        }
        if(!dir.exists(className)) {
                return(paste(className,"not found"))
        }        
        print(className)
        if(!file.exists(paste0(className,"/centroid.ctr"))) {
                file.copy(filename,paste0(className,"/centroid.ctr"))        
        }
        total<-length(dir(className,include.dirs = FALSE))
        file1 <- read.table(filename, stringsAsFactors=FALSE, encoding="UTF-8", sep=";", 
                            quote="\"",col.names = c("term","tfidf"), blank.lines.skip = TRUE)
        fcentroid <- read.table(paste0(className,"/centroid.ctr"), stringsAsFactors=FALSE, 
                                encoding="UTF-8", sep=";", quote="\"",col.names = c("term","tfidf"), blank.lines.skip = TRUE)
        aux<-c(file1$term,fcentroid$term)
        termo<-sort(unique(aux))
        tam<-length(termo)
        new_centroid<-cbind(termo,0L)
        colnames(new_centroid)<-c("termo","tf_idf")
        for(nr in 1:tam) {
                val1<-file1$tfidf[which(termo[nr] == file1)]
                if(!length(val1)) val1<-0
                val2<-fcentroid$tfidf[which(termo[nr] == fcentroid)]
                if(!length(val2)) val2<-0
                media<-val2+(1/total)*(val1-val2)
                new_centroid[nr,2]<-media
        }
        write.table(new_centroid,paste0(className,"/centroid.ctr"),row.names = FALSE, 
                    sep = ";", quote = FALSE, col.names = FALSE, fileEncoding = "utf8")
        file.copy(filename,className,overwrite = TRUE)
}
