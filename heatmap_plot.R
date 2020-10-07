library("ggplot2")
library("reshape")

# Convert the distance matrix to data.frame
dist_matrix<-read.table("dst.phy")
nombres<-dist_matrix[,1]
dist_matrix<-dist_matrix[,-1]
colnames(dist_matrix)<-nombres
rownames(dist_matrix)<-nombres
#orden<-c("Santa_aria","Puert_opez","SanMartin","Guaviare","Caqueta","Mitu","Carmen","Rovira","Panam_City","Chiriqui","Punta_enas","Apartado")
orden<-c("Santa_aria","Puert_opez","SanMartin","Guaviare","Caqueta","Carmen","Rovira","Nilo","Panam_City","Chiriqui","Punta_enas","Apartado")
dist_matrix<-dist_matrix[orden,orden]
#dist_matrix[upper.tri(dist_matrix)]<-NA
pwFST_dist <- reshape2::melt(as.matrix(dist_matrix), na.rm=T,id.vars=nombres)
head(pwFST_dist)

# Set up heatmap plot
pdf("dst.pdf")
ggplot(pwFST_dist, aes(Var1, Var2)) + geom_tile(aes(fill=value)) + scale_fill_gradient(name = "Da",low = "#FFFFFF",high = "#012345")+theme_bw()
dev.off()

for (i in 1:ncol(dist_matrix)){
  x=which(dist_matrix[,i] < 0)
  dist_matrix[x,i]<-0
}
