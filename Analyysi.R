#Analyysi.R
#2017-05-18 Jakke Mäkelä
#Analysoi kuntien Vihreiden tulokset v 2017 vaaleissa

library(dplyr)

#http://tulospalvelu.vaalit.fi/KV-2017/fi/ladattavat_tiedostot.html

ReadPiiriData <- function(PiiriNum){
  fulldata <- read.csv("kv-2017_teat_maa.csv",header=FALSE,sep=";")
  piiridata <- fulldata[fulldata[,2]==PiiriNum,]  
  info <- piiridata[,c(3,8,12,15,18,19,33,34,35,38,39)]
  colnames(info) <- c("kuntanro","puoluetunn","puoluelyh","ehdokasnro",
                      "etunimi","sukunimi","N.ennakko","N.vaalipva","N.yht",
                      "osuus","valintatieto")
  write.csv(info,file=paste("Piiri", PiiriNum,".csv",sep=""))
}

info <- read.csv("Piiri3.csv")

numsum <- function(x){sum(as.numeric(x))}
agg <- aggregate(info,by=list(kunta.agg=info$kuntanro,puoluetunn.agg=info$puoluetunn),FUN=numsum)

kuntasum <- aggregate(info$N.yht,by=list(kunta.sum=info$kuntanro),FUN=numsum)

kuntadata <-read.csv("Kuntaluettelo.csv")

agg$N.kunta <- NA
agg$kuntanimi <- NA
for (i in 1:nrow(agg)){
  agg$N.kunta[i] <- kuntasum$x[kuntasum$kunta.sum==agg$kunta.agg[i]]
  agg$kuntanimi[i] <- as.character(kuntadata$KUNTANIMIFI[kuntadata$KUNTANRO==agg$kunta.agg[i]]) 
}

agg$aan.rel <- agg$N.yht/agg$N.kunta

agg <- as.data.frame(agg)

aggfinal <- select(agg,kunta.agg,kuntanimi,puoluetunn.agg,N.yht,N.kunta,aan.rel)

#Vihreiden koodi on 13
vihr <- aggfinal[aggfinal$puoluetunn.agg==13,]
write.csv(vihr,"VSVihr.csv")

