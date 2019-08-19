# Load collection from directory collection
#
# for a tst
c <- 0

myClass <- "class"
classe <- "at2"
fileLoaded <- ""
lfile <- "01012004at2.txt"


source('loadConfig.R')
myCollection <- data.frame(stringsAsFactors = FALSE,
                           Class <- "",
                           File <- "",
                           Text <-"")

filewrite <- data.frame(myCollection)
fileLoaded <- read.table("dataset/files.idx",sep = ";",nrows = 5,stringsAsFactors = FALSE,
                       encoding = "UTF-8")
filewrite <-  cbind(class = "", fileLoaded )
colnames(filewrite) <- c("Class","File","Text")

for(linha in fileLoaded) {
        c <- 0
        linhadt <- as.data.frame(linha,stringsAsFactors = FALSE)
        Classe <- substr(linha$File,start = nchar(linha$File) -6 ,
                                  stop = nchar(fileLoaded$File)-3) 
        File <- linha$File
        Text <- linha$Text
        filewrite [c,] <- rbind(filewrite,c(classe, File,Text))
        if(c > 3)
                break
        
}
write.csv(x = filewrite,file = "dataset/aTribuna.csv")

