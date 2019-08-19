# to create execution in parallel

for(lfile in list.files("./index/",full.names = TRUE)) {
        nfile<-unlist(strsplit(lfile,"t"))
        nfile<-paste0(nfile[[1]][1],"idx")
        file.rename(lfile,nfile)
}