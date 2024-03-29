# Plot Class from Centroids in files2test directory
#
# lfile <- "at2.trn"
# if(length(compare) != 0)
#        par(mfrow=c(1,2))


plotClassAll <- function(compare = NULL, corteG = 0) {
        source("loadConfig.R")
        par(mfrow=c(5,5))
        par(mar = c(3.5,0.2,0.2,0.2))
        classes <- list.files((myClass))
        myClasses <- as.character(lapply(paste0(classes),FUN = paste0))
        return(lapply(myClasses,wplot = TRUE,compare = compare, multiPlot = TRUE, 
                      corte = corteG, interact = FALSE, FUN = plotClass))
}
        
plotClass <- function(lfile = lfile, compare = NULL, all = FALSE, 
                      wplot = TRUE, corte = 0, typePlot = "p", 
                      multiPlot = FALSE, interact = FALSE) {
        if(corte<0)
                return("ERROR - corte must be greather than zero")
        source("loadConfig.R")
        if(!multiPlot)
                par(mfrow=c(1,1))
        nrFiles <- 1
        colorPlot <- 1
        corM <- 0
        if(!file.exists(paste0(statistic,"/",lfile,".trn"))) {
                return(sprintf("ERRO - %s not found",paste0(statistic,"/",lfile,".trn")))
                }
        ni <- read.csv(paste0(statistic,"/",lfile,".trn"))
        if(wplot) {
                ni <- read.csv(paste0(statistic,"/",lfile,".trn"))
                ni <- subset(ni, mean > max(ni$mean) * corte)
                ni$tfidf <- 0
                ni <- ni[order(ni$mean,decreasing = FALSE),]
                ni$i <- 1:length(ni$term)
                if(length(compare)[1] == 0) {
                        par(new = "F")
                        plot(ni$i, ni$mean, col = "blue", xlim = c(0,max(ni$i)), 
                             ylim = c(0,max(ni$mean)), type = typePlot, main = lfile,
                             xlab = "Terms", ylab = "TF-IDF")
                        abline(glm(ni$i ~ ni$mean), col = "blue")
                        corM <- cor(ni$mean,ni$i)
                        return(corM)
                }
                if(length(compare)[1] > 0) {
                        soma <- 0
                        if(file.exists(paste0(compare))) {
                                doc  <- read.csv(paste0(compare),stringsAsFactors = FALSE, 
                                                 header = FALSE,
                                                 col.names = c("term","tfidf"),
                                                 sep = ";", encoding = "UTF-8")
                                doc <- subset(doc, tfidf > max(doc$tfidf) * corte)
                                for(i  in 1:length(doc$term)[1]) {
                                        ind <- which(ni$term == doc$term[i])
                                        if(length(ni$term[ind])[1] > 0)
                                                ni$tfidf[ind] <- doc$tfidf[i]
                                        if(length(ni$term[ind])[1] == 0) {
                                                next
                                        }
                                        #soma <- soma +
                                        #        ((doc$tfidf[i] - ni$mean[ind]) * ni$i[ind])
                                }
                                #corM <- soma
                                corM <- cor(ni$mean,ni$tfidf)
                                ni <- subset(ni, tfidf > 0)
                                ni$i <- 1:length(ni$term)
                                plot(ni$i, ni$mean, col = "blue", 
                                     type = typePlot, main = lfile,
                                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                                     xlab = paste("correlation: ",corM), ylab = "TF-IDF/Mean")
                                abline(glm(ni$mean ~ ni$i), col = "blue")
                                par(new = "T")
                                plot(ni$i, ni$tfidf, col = "red", 
                                     pch = 16,
                                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                                     xlab = paste("correlation: ",corM), ylab = "TF-IDF/Mean")
                                abline(glm(ni$tfidf ~ ni$i), col = "red")
                                return(corM)
                                }
                        }
        }
        return(c("ERRO",length(compare)[1]))
        }