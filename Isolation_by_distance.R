library(geosphere)
library(pegas)
library(MASS)
library(SpatialTools)
library(ape)
library(pegas)
library(poppr)

fst<-read.table('COI_fst.phy',head=F)
nombres<-fst[,1]
fst<-fst[,-1]
colnames(fst)<-nombres
rownames(fst)<-nombres

##TRansform zeros
for (i in 1:ncol(fst)){
	x=which(fst[,i] < 0)
	fst[x,i]<-0
}

#Transform fst to 1/1-Fst

Tfst=NULL
for (i in 1:ncol(fst)){
	fst[,i]=fst[,i]/(1-fst[,i])
}
Tfst=fst

#Transform Inf values to zero
for (i in 1:ncol(Tfst)){
	x=which(Tfst[,i] == Inf)
	Tfst[x,i]<-0
}


#Geography
coordinates<-read.csv('coords_COI.csv',sep=",",head=F)
nombres<-coordinates[,1]
coordinates<-coordinates[,-1]
dis_geo<-distm(as.matrix(coordinates),fun=distGeo)
Dgeo<-dist1(as.matrix(coordinates))

km=NULL
count=1
for(i in dis_geo){
	x<-i/1000
	km[count]<-x
	count=count+1
}

#transformacion distancias
Tgeo=NULL
count=1
for(i in km){
	x=i+1
	y<-log(x)
	Tgeo[count]<-y
	count=count+1
}

#Regression

Tfst<-as.matrix(Tfst)
dim(Tgeo)<-c(ncol(Tfst),ncol(Tfst))
pdf("COI_IBD.pdf",family = "Helvetica")
plot(Tgeo,Tfst,pch=16,xlab="Geographic Distance(ln)",ylab="Fst/1-Fst",main="IBD")
linea<-lm(c(Tfst)~c(Tgeo))
summary(linea)
abline(linea,col='red')
dev.off()
cor.test(Tgeo,Tfst,method="pearson")
distfst<-as.dist(Tfst)
distgeo<-as.dist(Tgeo)
ibd <- mantel.randtest(distgeo,distfst,nrepet=999)
