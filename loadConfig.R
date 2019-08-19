# Load configuration file and create all variables into de system
#

config<-read.csv("config/config.dat",header = TRUE,sep = ";",stringsAsFactors = FALSE)
collection<-config$value[[which(config$var=="collection")]]
myClass<-config$value[[which(config$var=="myClass")]]
kNNDir<-config$value[[which(config$var=="kNNDir")]]
minSizeTerm<-as.integer(config$value[[which(config$var=="minSizeTerm")]])
statistic<-config$value[[which(config$var=="statistic")]]
index<-config$value[[which(config$var=="index")]]
