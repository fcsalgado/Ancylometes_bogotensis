install.packages('ecospat')
library(ecospat)
cis <- read.csv('cis.csv')
trans <- read.csv('trans.csv')
##	Niche	quantification	and	comparaison
###	PCA-ENVIRONMENT
# the	pca	is	calibrated	on	all	the	sites	of	the	study	area
pca.env<-dudi.pca(rbind(cis,trans)[,3:9],scannf=FALSE,nf=2)
# Plot	Variables	Contribution	with	ecospat.plot.contrib()
ecospat.plot.contrib(contrib=pca.env$co, eigen=pca.env$eig)
#	predict	the	scores	on	the	axes
scores.globclim<-pca.env$li
scores.sp.nat<-suprow(pca.env,cis[which(cis[,10]==1),3:9])$li
scores.sp.inv<-suprow(pca.env,trans[which(trans[,10]==1),3:9])$li
scores.clim.nat<-suprow(pca.env,cis[,3:9])$li
scores.clim.inv<-suprow(pca.env,trans[,3:9])$li
#	Calculate	the	Occurrence	Densities	Grid	with	ecospat.grid.clim.dyn()
#	For	a	species	in	CIS
grid.clim.nat<-ecospat.grid.clim.dyn(glob=scores.globclim,
                                     glob1=scores.clim.nat, sp=scores.sp.nat, R=100, th.sp=0)
#For a species in Trans
grid.clim.inv<-ecospat.grid.clim.dyn(glob=scores.globclim,
                                     glob1=scores.clim.inv, sp=scores.sp.inv, R=100, th.sp=0) 
#	Calculate	Niche	Overlap	
ecospat.niche.overlap (grid.clim.nat, grid.clim.inv, cor=TRUE)
#Perform	the	Niche	Equivalency	Test	
eq.test<-ecospat.niche.equivalency.test(grid.clim.nat,
                                        grid.clim.inv, rep=1000, alternative = "greater", ncores = 1)
#	Niche	Similarity	Test
sim.test<-ecospat.niche.similarity.test(grid.clim.inv,
                                        grid.clim.nat, rep=1000, alternative = "lower", rand.type = 2,
                                        ncores = 1) 
#plot equivalency test
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
#plot similarity test
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
#Visualizing	niche	categories,	niche	dynamics	and	climate	analogy	between	ranges	with
ecospat.plot.niche.dyn(grid.clim.nat, grid.clim.inv, quant=0.1,
                       interest=2, title= "Niche Overlap", name.axis1="PC1",
                       name.axis2="PC2")
