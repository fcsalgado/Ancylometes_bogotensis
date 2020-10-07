#Running phrapl without an group

# load the libraries
library(ape)
library(phrapl)
#Read your trees
trees<-read.tree('trees_out.tre')
#Remove node label
for(i in 1:2){
	trees[[i]]$node.label<-NULL
}
#load your assignation file
assignFile<-read.csv("categorias.txt",header=TRUE,stringsAsFactors=FALSE)
#modificar nombres tips
for(s in 1:2){
lista=NULL
count=0
for(i in trees[[s]]$tip.label){
	z<-match(i,assignFile$indiv)
	count=count+1
	lista[count]<-z
	}
trees[[s]]$tip.label<-lista
}

nom=NULL
count=0
for (i in 1:37){
	w=i
	count=count+1
	nom[count]=w
}
assignFile$indiv<-nom



##tips sample, number of subsampling, this for tree space
popAssignments<-list(c(4,4))#this 3 tips from 3 populations
#subsampling trees, this is necessary for huge data
assignmentsGlobal<-assignFile
observedTrees<-trees
popAssignments<-list(c(4,4))
subsamplesPerGene<-100
outgroup=FALSE
outgroupPrune=FALSE
#apply the function
observedTrees<-PrepSubsampling(assignmentsGlobal=assignmentsGlobal,observedTrees=observedTrees,
popAssignments=popAssignments,subsamplesPerGene=subsamplesPerGene,outgroup=outgroup,
outgroupPrune=outgroupPrune)
#if the trees are not rooted use the next commands
library(phangorn)
observedTreesMidpoint<-lapply(observedTrees[[1]],midpoint)
class(observedTreesMidpoint)<-"multiPhylo"
observedTreesMidpoint<-list(observedTreesMidpoint)

#tip thing
subsampleWeights.df<-GetPermutationWeightsAcrossSubsamples(popAssignments=popAssignments,observedTrees=observedTreesMidpoint)
#Models
load("MigrationArray_2Pop_4K.rda")
#save the input
migrationArrayMap<-GenerateMigrationArrayMap(migrationArray)

save(list=c("observedTrees","subsampleWeights.df","migrationArrayMap"),file=paste(getwd(),"phraplInput.rda",sep=""))
###Add a new model
collapseList=list(c(1,1))
n0multiplierMap=list(c(1,1))
growthList=list(c(0,0))
migrationList<-list(t(array(c(NA,1,2,NA),dim=c(2,2))))

migrationIndividual<-GenerateMigrationIndividualsOneAtATime(collapseList=collapseList,
n0multiplierList=n0multiplierMap,growthList=growthList,migrationList=migrationList)

migrationArray[[6]]<-migrationIndividual

#numero de arboles
nTrees<-100000

#busqueda

modelRange<-c(1:6)

result<-GridSearch(modelRange=modelRange,
	migrationArray=migrationArray,
	#inputTrees=inputTrees,
	#migrationArrayMap=migrationArrayMap,           # do not required in the new version CHECK
	popAssignments=popAssignments,
	nTrees=nTrees,
	#	msPath="/Users/ariadnamoralesgarcia/msdir/ms",
	#	comparePath="/Library/Frameworks/R.framework/Versions/3.1/Resources/library/phrapl/extdata/comparecladespipe.pl",
	observedTree=observedTreesMidpoint,
	subsampleWeights.df=subsampleWeights.df,
	print.ms.string=TRUE,
	print.results=TRUE,
	debug=TRUE,return.all=TRUE,
	collapseStarts=c(0.30,0.58,1.40,2.54,4.1),
	migrationStarts=c(0.10,0.22,0.46,1.00,2.15),
	subsamplesPerGene=100,
	totalPopVector=totalPopVector,
	popScaling=c(0.25, 0.25, 1, 1, 1),
	print.matches=TRUE)

#Print summary results to end of Rout file
print(result[[1]])

#Make dedicated grid list
gridList<-result[[1]]


#Save the workspace from first grid analysis and create output folders
system(paste("mkdir ", getwd(),  "/results", sep=""))
save(list=ls(), file="phraplrun_final.rda")
system(paste("mkdir ", getwd(),  "/results/RoutFiles", sep=""))
system(paste("mv ", getwd(), "/phraplrun_without_outgroup.Rout ", getwd(), "/results/RoutFiles/phraplrun_without_outgroup.Rout", sep=""))
#system(paste("rm ", getwd(), "/scripts/phraplrun_without_outgroup.R.out", sep=""))
system(paste("mkdir ", getwd(),  "/results/rdaFiles", sep=""))

save(result,file="/results/rdaFiles/phrapl_1_6.rda")


########################
### 3. Post-process  ###
########################

## If different models (from the same migrationArray file) were run separatly, the output can be concatenated.
totalData<-ConcatenateResults(rdaFilesPath=paste(getwd(),"/results/rdaFiles",sep=""),rdaFiles=NULL,addAICweights=TRUE,addTime.elapsed=FALSE)
modelAverages<-CalculateModelAverages(totalData, parmStartCol=9)

## Save output to a txt file
write.table(totalData, file=paste(getwd(),"/results/totalData.txt",sep=""), sep="\t", row.names=FALSE, quote=FALSE)
