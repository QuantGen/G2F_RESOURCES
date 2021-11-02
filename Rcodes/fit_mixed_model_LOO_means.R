
rm(list=ls())
#setwd("~/Dropbox/projects/G2F_RESOURCES")
setwd("/mnt/research/quantgen/projects/lopezcru/G2F_RESOURCES")

# Load functions
library(BGLR)

args=(commandArgs(TRUE))
if(length(args)==0){
   stop('No args provided')
}else{
   for(i in 1:length(args)){
       eval(parse(text=args[[i]]))
    }
}
# job <- 5
foldsCol <- c(YL="year_loc", YEAR="year")[2]
models <- c('1'="YEAR+LOC+YL", '2a'="W1", '2b'="W2", '3a'="LOC+W1", '3b'="LOC+W2")

JOBS <- data.frame(model=names(models),comp=models)
model <- JOBS[job,"model"]
comp <- JOBS[job,"comp"]

dataW <- read.csv("Data/OutputFiles/clean_EC_by_stage.csv", check.names=F)
pheno <- read.csv("Data/OutputFiles/clean_pheno_yearloc_means.csv")

pheno <- pheno[match(dataW$year_loc,pheno$year_loc),]
if(sum(pheno$year_loc != dataW$year_loc) >0) stop("Matching error")

y <- as.vector((pheno$yield))

# Match EC
W <- dataW[,5:ncol(dataW)]
W <- scale(W)

pheno$year <- factor(as.character(pheno$year))
pheno$location <- factor(as.character(pheno$location))
pheno$year_loc <- factor(as.character(pheno$year_loc))

Z.YEAR=scale(as.matrix(model.matrix(~ year-1,data=pheno)),center=TRUE,scale=FALSE)
Z.LOC=scale(as.matrix(model.matrix(~ location-1,data=pheno)),center=TRUE,scale=FALSE)
Z.YL=scale(as.matrix(model.matrix(~ year_loc-1,data=pheno)),center=TRUE,scale=FALSE)

nIter <- 20000
burnIn <- 4000

ETA <- list(YEAR=list(X=Z.YEAR,model='FIXED',saveEffects=TRUE),
           LOC=list(X=Z.LOC,model='BRR',saveEffects=TRUE),
           YL=list(X=Z.YL,model='BRR',saveEffects=TRUE),
           W1=list(X=W,model='BRR',saveEffects=TRUE),
           W2=list(X=W,model='BayesC',saveEffects=TRUE)
           )

outfolder <- paste0("Output/run_LOO_",names(foldsCol),"_mean")
if(!file.exists(outfolder)) dir.create(outfolder,recursive=TRUE)

folds <- levels(pheno[,foldsCol])

compNames <- unlist(strsplit(as.vector(comp),"\\+"))
ETA <- ETA[compNames]
cat("Model:",model,". Components:",paste(names(ETA),collapse=" + "),"\n")

for(ff in 1:length(folds))
{
  yNA <- y
  indexNA <- which(as.character(pheno[,foldsCol]) == folds[ff])
  yNA[indexNA] <- NA

  fm <- BGLR(y=yNA, ETA=ETA, nIter=nIter, burnIn=burnIn)
  yHat <- fm$yHat

  outfile <- paste0(outfolder,"/fm_",names(foldsCol),"_",folds[ff],"_M",model,".RData")
  save(yHat,indexNA,comp,file=outfile)
}
