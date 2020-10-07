#libraries
library(adegenet)
library(ape)
#load your aligment
dna<-read.dna("COI_lineamiento_dataset.fas",format="fasta")
gen<-DNAbin2genind(dna)
#load the coordinates and population assignation
coords<-read.csv("COI_coords.csv",head=T)
#Add a little noise to the points
coords$x<-jitter(coords$x,factor=1,amount=0.1)
coords$y<-jitter(coords$y,factor=1,amount=0.1)
#ADD the coordinates and pop assigment to the genind object
gen$other$xy<-coords[,2:3]
gen$pop<-as.factor(coords[,4])
#choose the background graph
gab<-chooseCN(gen$other$xy,ask=F,type=1)
#Calculate the distances
D<-dist(gen$tab)
#Do all the magic, be careful with the threshold, ITS=5, COI=
mon1 <- monmonier(gen$other$xy,D,gab)
###iF THE THRESHOLD IS NOT SO EVIDENT, DO A PCA
pco1 <- dudi.pco(D,scannf=FALSE,nf=1)
D <- dist(pco1$li)
#Re-run the monmonier
mon1 <- monmonier(gen$other$xy,D,gab)
plot(mon1,add.arrows=FALSE,bwd=10,col="black")
points(gen$other$xy, cex=2, pch=20,col=fac2col(pop(gen), col.pal=spectral))
legend("topright",leg=c("Pop A", "Pop B"),pch=c(20),col=spectral(2), pt.cex=2)
