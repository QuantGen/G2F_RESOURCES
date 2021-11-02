
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
# job <- 1

comps <- c("YEAR+LOC+YL+G","W1+G","W2+G","LOC+W1+G","LOC+W2+G")

JOBS <- data.frame(model=c(1,"2a","2b","3a","3b"), comp=comps)[,]
model <- JOBS[job,"model"]
comp <- JOBS[job,"comp"]

compNames <- unlist(strsplit(comp,"\\+"))

dataW <- read.csv("Data/OutputFiles/clean_EC_by_stage.csv", check.names=F)
pheno <- read.csv("Data/OutputFiles/clean_pheno_raw.csv")

pheno <- pheno[pheno$year_loc %in% dataW$year_loc,]
y <- as.vector(scale(pheno$yield))

# Match EC
dataW <- dataW[match(pheno$year_loc,dataW$year_loc),]
if(any(pheno$year_loc != dataW$year_loc)) stop("Matching error")
W <- dataW[,5:ncol(dataW)]
W <- scale(W)

phase <- unlist(lapply(strsplit(colnames(W),"_"),function(x)x[1]))

pheno$year <- factor(as.character(pheno$year))
pheno$location <- factor(as.character(pheno$location))
pheno$year_loc <- factor(as.character(pheno$year_loc))
pheno$pedigree <- factor(as.character(pheno$pedigree))
pheno$gen_loc <- factor(paste(pheno$pedigree,pheno$location))

Z.YEAR=scale(as.matrix(model.matrix(~ year-1,data=pheno)),center=TRUE,scale=FALSE)
Z.LOC=scale(as.matrix(model.matrix(~ location-1,data=pheno)),center=TRUE,scale=FALSE)
Z.G=scale(as.matrix(model.matrix(~ pedigree-1,data=pheno)),center=TRUE,scale=FALSE)
Z.YL=scale(as.matrix(model.matrix(~ year_loc-1,data=pheno)),center=TRUE,scale=FALSE)

dim(Z.YEAR)
dim(Z.LOC)
dim(Z.G)
dim(Z.YL)

nIter <- 30000
burnIn <- 5000

ETA <- list(YEAR=list(X=Z.YEAR,model='FIXED',saveEffects=TRUE),
           LOC=list(X=Z.LOC,model='BRR',saveEffects=TRUE),
           YL=list(X=Z.YL,model='BRR',saveEffects=TRUE),
           W1=list(X=W,model='BRR',saveEffects=TRUE),
           W2=list(X=W,model='BayesC',saveEffects=TRUE),
           G=list(X=Z.G,model='BRR',saveEffects=TRUE)
           )


ETA <- ETA[compNames]

cat("Model:",model,". Components:",paste(names(ETA),collapse=" + "),"\n")

outfolder <- "Output/run_VC_full_model"
if(!file.exists(outfolder)) dir.create(outfolder,recursive=TRUE)

fm <- BGLR(y=y, ETA=ETA, nIter=nIter, burnIn=burnIn, saveAt=paste0(outfolder,"/M",model,"_"))

save(fm,file=paste0(outfolder,"/fm_M",model,"_",comp,".RData"))
