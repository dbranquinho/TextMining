t1<-c(1:10)
t2<-c(rnorm(t1,5,4))
plot(t1, t2, col = "blue", type = "l")
par(new=T)
t3<-c(3:8)
t4<-c(rnorm(t3,5,4))
library(manipulate)
manipulate(plot(t3, t4, col = "red", type = "l"), x = slider(1,t4))

# for term not found in ni file
ni <- rbind2(ni,c(0,doc$term[i],0,doc$tfidf[i],0,doc$tfidf[i]))
ni <- ni[order(ni$mean),]
ni$i <- 1:length(ni$term)
ind <- which(ni$term == doc$term[i])

data(mtcars)
manipulate(plot(cars, xlim=c(0,x.max)), x.max=slider(15,25))

classea<-c("alt.atheism","comp.graphics","comp.os.ms-windows.misc","comp.sys.ibm.pc.hardware",
"comp.sys.mac.hardware","comp.windows.x","misc.forsale","rec.autos","rec.motorcycles",
"rec.sport.baseball")

classeb<-c("rec.sport.hockey","sci.crypt", "sci.electronics","sci.med",
        "sci.space", "soc.religion.christian", "talk.politics.guns",
        "talk.politics.mideast", "talk.politics.misc", "talk.religion.misc")

i<-600
maxFiles<-700

lapply(c(classea,i,maxFiles),iCLassFileAll)

lapply(c(classeb,i,maxFiles),iCLassFileAll)

iCLassFileAll("alt.atheism",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("comp.graphics",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("comp.os.ms-windows.misc",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("comp.sys.ibm.pc.hardware",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("comp.sys.mac.hardware",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("comp.windows.x",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("misc.forsale",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("rec.autos",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("rec.motorcycles",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("rec.sport.baseball",clean = TRUE,iniFile = 100, maxFiles = 100)

iCLassFileAll("rec.sport.hockey",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("sci.crypt",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("sci.electronics",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("sci.med",clean = TRUE,maxFiles = 100)
iCLassFileAll("sci.space",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("soc.religion.christian",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("talk.politics.guns",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("talk.politics.mideast",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("talk.politics.misc",clean = TRUE,iniFile = 100, maxFiles = 100)
iCLassFileAll("talk.religion.misc",clean = TRUE,iniFile = 100, maxFiles = 100)

