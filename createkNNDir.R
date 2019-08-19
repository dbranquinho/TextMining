# Create Knn Directory with all distances the files to all other files into collection
###

source('LoadConfig.R')
source('loadCollection.R')
source('getDistance.R')

if(!dir.exists(kNNDir)) {
        dir.create(kNNDir)
}
fileKnn<-data.frame(cbind("",0),row.names = NULL,stringsAsFactors = FALSE)
names(fileKnn)<-c("docName","Distance")
for(i in 1:dim(doc)[1]) {
        nameDoc<-doc$docName[i]
        if(!file.exists(paste0(kNNDir,"/",nameDoc))) {
                file.create(paste0(kNNDir,"/",nameDoc))
        }
        iKnn<-1
        for(j in 1:dim(doc)[1]) {
                nameDoc2<-doc$docName[j]
                if(nameDoc!=nameDoc2) {
                        nameDoc2<-doc$docName[j]
                        eDistance<-getDistance(paste0(collection,"/",nameDoc),
                                               paste0(collection,"/",nameDoc2))
                        fileKnn$docName[iKnn]<-nameDoc2
                        fileKnn$Distance[iKnn]<-eDistance
                        iKnn<-1
                        write.table(fileKnn,paste0(kNNDir,"/",nameDoc),row.names = FALSE, append = TRUE,
                                    sep = ";", quote = FALSE, col.names = FALSE, fileEncoding = "utf8")
                        
                }
        }
}


