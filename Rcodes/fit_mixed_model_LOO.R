
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
# job <- 100

models <- c('1'="YEAR+LOC+YL+G",'2a'="W1+G",'2b'="W2+G")
#load("missing_JOBS.RData")
JOBS <- expand.grid(yl=1:85, comp=models)
comp <- as.vector(JOBS[job,"comp"])
yl <- JOBS[job,"yl"]
model <- names(models[models==comp])

compNames <- unlist(strsplit(comp,"\\+"))

dataW <- read.csv("Data/OutputFiles/clean_EC_by_stage.csv", check.names=F)
pheno <- read.csv("Data/OutputFiles/clean_pheno_raw.csv")

pheno <- pheno[pheno$year_loc %in% dataW$year_loc,]
y <- as.vector(pheno$yield)

# Match EC
dataW <- dataW[match(pheno$year_loc,dataW$year_loc),]
if(any(pheno$year_loc != dataW$year_loc)) stop("Matching error")
W <- dataW[,5:ncol(dataW)]
W <- scale(W)

pheno$year <- factor(as.character(pheno$year))
pheno$location <- factor(as.character(pheno$location))
pheno$year_loc <- factor(as.character(pheno$year_loc))
pheno$pedigree <- factor(as.character(pheno$pedigree))

Z.YEAR=scale(as.matrix(model.matrix(~ year-1,data=pheno)),center=TRUE,scale=FALSE)
Z.LOC=scale(as.matrix(model.matrix(~ location-1,data=pheno)),center=TRUE,scale=FALSE)
Z.G=scale(as.matrix(model.matrix(~ pedigree-1,data=pheno)),center=TRUE,scale=FALSE)
Z.YL=scale(as.matrix(model.matrix(~ year_loc-1,data=pheno)),center=TRUE,scale=FALSE)

nIter <- 12000
burnIn <- 4000

ETA <- list(YEAR=list(X=Z.YEAR,model='FIXED',saveEffects=TRUE),
           LOC=list(X=Z.LOC,model='BRR',saveEffects=TRUE),
           YL=list(X=Z.YL,model='BRR',saveEffects=TRUE),
           W1=list(X=W,model='BRR',saveEffects=TRUE),
           W2=list(X=W,model='BayesC',saveEffects=TRUE),
           G=list(X=Z.G,model='BRR',saveEffects=TRUE)
           )

ETA <- ETA[compNames]

cat("Model:",model,". Components:",paste(names(ETA),collapse=" + "),"\n")

outfolder <- "Output/run_LOO_YL"
if(!file.exists(outfolder)) dir.create(outfolder,recursive=TRUE)

YL_levels <- levels(pheno$year_loc)

yNA <- y
indexNA <- which(as.character(pheno$year_loc) == YL_levels[yl])
yNA[indexNA] <- NA

fm <- BGLR(y=yNA, ETA=ETA, nIter=nIter, burnIn=burnIn)
yHat <- fm$yHat

save(yHat,indexNA,file=paste0(outfolder,"/fm_YL_",YL_levels[yl],"_M",model,".RData"))
