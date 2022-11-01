## Hello World ###
rm(list=ls()) #clean local environment
require(readxl)
dat <- read_xlsx("C:/Users/timbr/Paleobiology/Biofacies/Project_Buttenheim.xlsx", sheet = 1)  # import dataset
require(BiodiversityR)
require(divDyn)
#View(dat) 

# get ecological trait data
library(chronosphere)
data <- fetch("pbdb", ver="20210530")
require(dplyr)
#Foram <- dplyr::filter(data, data$phylum == "Foraminifera")
#View(Foram)
#list(unique(data$phylum))



glist <- unique(dat$Genus)

glist <- glist[c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,23)] #exclude shell fragments
glist 
unique(dat$Species)

### Faunal list
class(dat)
count(dplyr::filter(dat, dat$Genus == glist[11] ))
unique(dat$Genus)
glist
?pie
x11()
pie(c(    
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[1]))), 
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[2]))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[3] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[4] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[5] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[6] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[7] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[8] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[9] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[10] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[11] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[12] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[13] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[14] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[15] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[16] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[17] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[18] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[19] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[20] ))),
  as.numeric(count(dplyr::filter(dat, dat$Genus == glist[21] )))
)
  ,labels = c(glist[1],"","",glist[4],"",glist[6],glist[7],glist[8],"","",glist[11], "","",glist[14],"","",glist[17],"",glist[19],glist[20],"Foraminifera (P)"),
radius = 0.9,
cex = 0.8, main = "Taxonomic composition (genus)", cex.main = 0.9)

Act <- rbind(ind, sub)
?pie
as.numeric(count(Act))


### Trophic nucleus
pie(c(    
as.numeric(count(og)), as.numeric(count(ko)), as.numeric(count(fora)), as.numeric(count(Act)),as.numeric(count(dun)), as.numeric(count(lbf))  
)
,labels = c("Ogmoconcha", "Procerithium", "Foraminifera (P)", "Actaeonina", "Eucyclus", "LBF"),
radius = 0.9, main = "Taxonomic composition of trophic nucleus (genus)",
cex = 0.8, cex.main = 0.9)

glist #1 4 6 7 8 11 14 17 19 20


### rankabundance

rdat <- read_xlsx("C:/Users/timbr/Paleobiology/Biofacies/Project_Buttenheim.xlsx", sheet = 2)  # import dataset
View(rdat)

rdat <- rdat[order(-rdat$relAbundance),]
View(rdat)

plot(rdat$relAbundance[1:10], type = "o")

   # of family

flist <- unique(dat$Family)
flist
rfdat <- as.data.frame(cbind(flist[c(1,2,3,4,5,6,8,9,10)],c(
  count(dat, dat$Family == flist[1])[2,2,1],
count(dat, dat$Family == flist[2])[2,2,1],
count(dat, dat$Family == flist[3])[2,2,1],
count(dat, dat$Family == flist[4])[2,2,1],
count(dat, dat$Family == flist[5])[2,2,1],
count(dat, dat$Family == flist[6])[2,2,1],
count(dat, dat$Family == flist[8])[2,2,1],
count(dat, dat$Family == flist[9])[2,2,1],
count(dat, dat$Family == flist[10])[2,2,1]
  ), c(  count(dat, dat$Family == flist[1])[2,2,1]/sum1,
         count(dat, dat$Family == flist[2])[2,2,1]/sum1,
         count(dat, dat$Family == flist[3])[2,2,1]/sum1,
         count(dat, dat$Family == flist[4])[2,2,1]/sum1,
         count(dat, dat$Family == flist[5])[2,2,1]/sum1,
         count(dat, dat$Family == flist[6])[2,2,1]/sum1,
         count(dat, dat$Family == flist[8])[2,2,1]/sum1,
         count(dat, dat$Family == flist[9])[2,2,1]/sum1,
         count(dat, dat$Family == flist[10])[2,2,1]/sum1
  )
))
colnames(rfdat) <- c("Taxon", "Count", "per")

 sum1 <- sum(count(dat, dat$Family == flist[1])[2,2,1],
  count(dat, dat$Family == flist[2])[2,2,1],
  count(dat, dat$Family == flist[3])[2,2,1],
  count(dat, dat$Family == flist[4])[2,2,1],
  count(dat, dat$Family == flist[5])[2,2,1],
  count(dat, dat$Family == flist[6])[2,2,1],
  count(dat, dat$Family == flist[8])[2,2,1],
  count(dat, dat$Family == flist[9])[2,2,1],
  count(dat, dat$Family == flist[10])[2,2,1])

rfdat <- rfdat[order(rfdat$per),]
plot(rfdat$per, type = "o",xaxt= "n", xlab = "Faunal member",main = "Faunal rank abundance (family)", cex = 1.5,col = "red", xlim = c(9,1), lwd = 2, ylab = "Proportion of abundance [%]")
axis(1,at=9:1,labels = c("Gastropoda","Ostracoda", "Foraminifera", "Ammonoidea", "Bivalvia","Bryozoa","Scaphopoda","Brachiopoda","Belemnoidea"))

unique(rfdat$Taxon)

View(rfdat)
#library(vegan)
#data("dune.env")
#data(dune)
#View(temp)

# of trophic nucleus 
og <- dplyr::filter(dat, dat$Genus == "Ogmoconcha")
ko <-  dplyr::filter(dat, dat$Species == "kochi")
sub <-dplyr::filter(dat, dat$Species == "submoorei")
dun <- dplyr::filter(dat, dat$Species == "dunkeri")
ind <- dplyr::filter(dat, dat$Species == "indet", dat$Genus == "Actaeonina")
fora <- dplyr::filter(dat, dat$Family == "Foraminifera", dat$Genus == "planktonic")
lbf <- dplyr::filter(dat, dat$Family == "Foraminifera", dat$Genus == "LBF")

tronuc <- rbind.data.frame(og, ko, sub, dun, ind, fora, lbf)
trlist<-unique(tronuc$Family)

trdat <- as.data.frame(cbind(c(trlist[c(1,2)], "Foraminifera (P)", "LBF"),c(
  count(tronuc, tronuc$Family == trlist[1])[2,2,1],
  count(tronuc, tronuc$Family == trlist[2])[2,2,1],
  44, 12
), c(  count(tronuc, tronuc$Family == trlist[1])[2,2,1]/sum1,
       count(tronuc, tronuc$Family == trlist[2])[2,2,1]/sum1,
       44/sum1, 12/sum1
)
))
colnames(trdat) <- c("Taxon", "Count", "per")
trdat <- trdat[order(trdat$per),]
x11()
plot(trdat$per, type = "o", xlim =c(4,1),xaxt="n", ylim = c(0,0.4),main="Rank abundance of trophic nucleus",  xlab = "Trophic nucleus member", cex = 1.5,col = "red", lwd = 2, ylab = "Proportion of abundance [%]")
axis(1,at=4:1,labels = c("Gastropoda","Ostracoda", "Foraminifera (P)", "LBF"))

?plot
count(tronuc, tronuc$Family == "Ostracod")[2,2,1]
trlist
### taphonomy#

frac <- count(dplyr::filter(dat, dat$fractured == "x" ))[,,1]
abr <- count(dplyr::filter(dat, dat$abraded == "x" ))[,,1]
squa <- count(dplyr::filter(dat, dat$squashed == "x" ))[,,1]

Flawless <- count(dplyr::filter(dat, dat$good == "x"))[,,1]

preservation <- rbind(Flawless, frac, abr, squa)

barplot(as.matrix(preservation), space = 0.3, ylab = "Count", main ="Taphonomy" , cex.main = 1.2,beside = T,col = c("green","yellow","orange","red"), names.arg  = c("Well preserved", "Fractured", "Abraded", "Squashed"))
legend(0.5, 125, "159", bty = "n")
legend(1.85, 100, "133", bty = "n")
legend(3.15, 60, "42", bty = "n")
legend(4.45, 51, "35", bty = "n")



?barplot

#### benthos
dplyr::filter(dat, dat$ModeOfLife == "infaunal")

inf <-dplyr::filter(dat, dat$ModeOfLife == "infaunal")
epif <- dplyr::filter(dat, dat$ModeOfLife == "epifaunal")
sinf <- dplyr::filter(dat, dat$ModeOfLife == "semi-infaunal")

unique(inf$Genus)
unique(epif$Genus)
unique(sinf$Genus)
View(inf)


### Traits

unique(dat$ModeOfLife)
nek <- dplyr::filter(dat, dat$ModeOfLife == "nektonic")
pla <- dplyr::filter(dat, dat$ModeOfLife == "planktonic")


cep <-  as.numeric(count(epif))
cin <- as.numeric(count(inf))
csin <- as.numeric(count(sinf))
cne <- as.numeric(count(nek))
cpl <- as.numeric(count(pla))

mol <- rbind(cin,csin,cep,cne,cpl)

barplot(as.matrix(mol), beside = T, space =0.3, col = c("black","brown","orange","blue","green"), names.arg = c("Infaunal","Semi-Infaunal","Epifaunal","Nektonic","Planktonic"), main="Mode of life", cex.main=1.2)
legend(0.5, 100, "74", bty = "n")
legend(1.8, 25, "1", bty = "n")
legend(3.1, 120, "187", bty = "n")
legend(4.4, 30, "4", bty = "n")
legend(5.7, 65, "44", bty = "n")

?legend
?barplot
unique(dat$Motility)
mob <- dplyr::filter(dat, dat$Motility == "mobile")
sta <- dplyr::filter(dat, dat$Motility == "stationary")
bys <- dplyr::filter(dat, dat$Motility == "byssate")
cha <- dplyr::filter(dat, dat$Motility == "mobile, attached")

cmob <- as.numeric(count(mob))
csa <- as.numeric(count(sta))
cby<- as.numeric(count(bys))
cch <- as.numeric(count(cha))

mot <- rbind(cmob,csa,cby,cch)

barplot(as.matrix(mot), beside = T, space=0.3, col = c("red","blue","orange","purple"), names.arg = c("Mobile","Stationary","Byssate","Mobile, attached"), main="Motility", cex.main=1.2)
legend(0.5, 175, "263", bty = "n")
legend(1.85, 100, "62", bty = "n")
legend(3.15, 25, "1", bty = "n")
legend(4.45, 35, "4", bty = "n")


unique(dat$Feeding)

det <- dplyr::filter(dat, dat$Feeding == "detritus")
car <- rbind(dplyr::filter(dat, dat$Feeding == "carnivore"), dplyr::filter(dat, dat$Feeding == "mixotroph"))
sus <- dplyr::filter(dat, dat$Feeding == "suspension")
gra <- dplyr::filter(dat, dat$Feeding == "grazer")
fil <- dplyr::filter(dat, dat$Feeding == "filter")
dep <- dplyr::filter(dat, dat$Feeding == "deposit")
omn <- dplyr::filter(dat, dat$Feeding == "omnivore")

cde <- as.numeric(count(det))
cca <- as.numeric(count(car))
csu <- as.numeric(count(sus))
cgr <- as.numeric(count(gra))
cfi <- as.numeric(count(fil))
cdep <- as.numeric(count(dep))
com <- as.numeric(count(omn))

csf <- csu+cfi

diet <- rbind(cde, cca, csf, cgr, cdep, com)

barplot(as.matrix(diet), beside = T, space = 0.3,col = c("Brown","red","orange","green","purple", "navy"),names.arg = c("Detritus feeder","Micro-carnivore","Suspension feeder","Grazer", "Deposit feeder", "Omnivore"),main="Feeding mechanism", cex.main = 1.2)
legend(0.4, 90, "123", bty = "n")
legend(1.75, 35, "13", bty = "n")
legend(3.0, 35, "13", bty = "n")
legend(4.35, 50, "27", bty = "n")
legend(5.65, 15, "1", bty = "n")
legend(6.9, 70, "44", bty = "n")

##
unique(dat$Family)
as.numeric(count(dplyr::filter(dat, dat$Family == "Ostracod")))

Artic <- dplyr::filter(dat, dat$Family == "Ostracod", dat$Preservation == "Articulate")
      
## Guilds 
unique(dat$ModeOfLife) #4
unique(dat$Feeding) #7
unique(dat$Motility) #4

ecm <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="carnivore",dat$Motility=="mobile")
edm <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="detritus",dat$Motility=="mobile")
edgm <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="detritus/grazer",dat$Motility=="mobile")
egm <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="grazer",dat$Motility=="mobile")
ess <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="suspension",dat$Motility=="stationary")
esb <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="filter",dat$Motility=="byssate")
esma <- dplyr::filter(dat, dat$ModeOfLife=="epifaunal", dat$Feeding=="suspension",dat$Motility=="mobile, attached")
idm <- dplyr::filter(dat, dat$ModeOfLife=="infaunal", dat$Feeding=="detritus",dat$Motility=="mobile")
ism <- dplyr::filter(dat, dat$ModeOfLife=="infaunal", dat$Feeding=="suspension",dat$Motility=="mobile")
sidm <- dplyr::filter(dat, dat$ModeOfLife=="semi-infaunal", dat$Feeding=="deposit",dat$Motility=="mobile")
ncm <- dplyr::filter(dat, dat$ModeOfLife=="nektonic", dat$Feeding=="carnivore",dat$Motility=="mobile")
pos <- dplyr::filter(dat, dat$ModeOfLife=="planktonic", dat$Feeding=="omnivore",dat$Motility=="stationary")


guilds <- rbind(as.numeric(count(idm)),
                as.numeric(count(ism)),
                as.numeric(count(edgm)),
  as.numeric(count(ecm)),
                as.numeric(count(edm)),
                as.numeric(count(egm)),
                as.numeric(count(ess)),
                as.numeric(count(esb)),
                as.numeric(count(esma)),
                as.numeric(count(sidm)),
                as.numeric(count(ncm)),
                as.numeric(count(pos))
                )
barplot(as.matrix(guilds), horiz = T,space= 0.2,xlim=c(0,85), las = 1,names.arg=c("72", "2","81", "7","51","27","6","1","4","1","6","44") ,beside=T,col=c("brown", "red","salmon","orange", "yellow", "olivedrab", "green","turquoise","blue","navy","violet", "purple"), main="Guilds by abundance", legend.text=c( "infaunal, detritus, mobile",
                                                                                                                    "infaunal, suspension, mobile",
                                                                                                                    "epifaunal, detritus/grazer, mobile", "epifaunal, carnivore, mobile",
                                                                                                   "epifaunal, detritus, mobile",
                                                                                                   "epifaunal, grazer, mobile",
                                                                                                   "epifaunal, suspension, stationary",
                                                                                                   "epifaunal, suspension, byssate",
                                                                                                   "epifaunal, suspension, mobile/attached",
                                                                                                   "semi-infaunal, deposit, mobile",
                                                                                                   "nektonic, carnivore, mobile",
                                                                                                   "planktonic, omnivore, stationary"))
