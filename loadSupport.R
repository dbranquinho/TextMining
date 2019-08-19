source("loadConfig.R")

tlemmaWords <- list()
tlemmaWords <- read.table("support/lematizer.txt",strip.white = TRUE, sep = ";", quote = "",
                        stringsAsFactors = FALSE, header = FALSE, encoding = "UTF-8")
stopWords <- data.frame()
stopWords <- read.table("support/stopwords.txt", stringsAsFactors = FALSE, 
                        header = FALSE, quote = "", encoding = "UTF-8")

lemmaWords <- list()

for(i in 1:dim(tlemmaWords)[1]) {
        lista<-strsplit(tlemmaWords$V2[i],split = ",")   
        lemmaWords[[i]] <- rbind(tlemmaWords$V1[i],lista)
}
remove(tlemmaWords,lista,i)

