# Create class from atractors 
###

source('centroid.R')
source('config.R')

files<-list.files(collection, all.files = FALSE)
atractors<-read.csv("config/atractors.dat",header = FALSE,sep = ";",stringsAsFactors = FALSE, col.names = c("file","class"))
tam<-length(atractors[[1]])

for(i in 1:tam) {
        iClassFile(atractors$class[i],paste0(collection,"/",atractors$file[i]), cc = TRUE)
}