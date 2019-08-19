# Plot Class from tree PSA
#

source("loadCollection.R")
collection<-"./class/at2"
doc<-list()
i=1
for(lfiles in list.files(collection,full.names = TRUE)) {
        doc[[i]] <- read.csv(lfiles, header = FALSE, stringsAsFactors = FALSE, 
                             sep = ";", encoding = "utf-8")
        i = i +1
}

head(unique(sort(doc[[1]]$V2,decreasing = TRUE)),length(doc)*.1)
plot(1:221, doc[[1]]$V2, type = "p")
lines(lm(1:221 ~ doc[[1]]$V2))
