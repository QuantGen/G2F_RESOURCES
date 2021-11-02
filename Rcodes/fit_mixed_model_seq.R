
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
# job <- 14

models <- c('1'="YEAR+LOC+YL+G",'2a'="W1+G",'2b'="W2+G",'3a'="LOC+W1+G",'3b'="LOC+W2+G")[2:3]
nP <- 6
Ph <- do.call(rbind,sapply(1:nP,function(x)cbind(Pi=x,Pj=x:nP)))
index <- rep(1:length(models),each=nrow(Ph))
JOBS <- data.frame(model=names(models)[index],comp=models[index],
                   Ph[rep(1:nrow(Ph),length(models)),])
model <- JOBS[job,"model"]
comp <- JOBS[job,"comp"]
Pi <- JOBS[job,"Pi"]
Pj <- JOBS[job,"Pj"]

compNames <- unlist(strsplit(comp,"\\+"))

dataW <- read.csv("Data/OutputFiles/clean_EC_by_stage.csv", check.names=F)
pheno <- read.csv("Data/OutputFiles/clean_pheno_raw.csv")

pheno <- pheno[pheno$year_loc %in% dataW$year_loc,]
y <- as.vector(scale(pheno$yield))

# Match EC
dataW <- dataW[match(pheno$year_loc,dataW$year_loc),]
W <- dataW[,5:ncol(dataW)]
W <- scale(W)

phase <- unlist(lapply(strsplit(colnames(W),"_"),function(x)x[1]))
Phases <- unique(phase)

pheno$year <- factor(as.character(pheno$year))
pheno$location <- factor(as.character(pheno$location))
pheno$year_loc <- factor(as.character(pheno$year_loc))
pheno$pedigree <- factor(as.character(pheno$pedigree))
pheno$gen_loc <- factor(paste(pheno$pedigree,pheno$location))

Z.YEAR=scale(as.matrix(model.matrix(~ year-1,data=pheno)),center=TRUE,scale=FALSE)
Z.LOC=scale(as.matrix(model.matrix(~ location-1,data=pheno)),center=TRUE,scale=FALSE)
Z.G=scale(as.matrix(model.matrix(~ pedigree-1,data=pheno)),center=TRUE,scale=FALSE)
Z.YL=scale(as.matrix(model.matrix(~ year_loc-1,data=pheno)),center=TRUE,scale=FALSE)

nIter <- 30000
burnIn <- 5000

index <- which(phase %in% Phases[Pi:Pj])
W <- W[,index]
cat("Size W:",nrow(W),"x",ncol(W),". Selected cols:",min(index),"-",max(index),"\n")

ETA <- list(YEAR=list(X=Z.YEAR,model='FIXED',saveEffects=TRUE),
           LOC=list(X=Z.LOC,model='BRR',saveEffects=TRUE),
           YL=list(X=Z.YL,model='BRR',saveEffects=TRUE),
           W1=list(X=W,model='BRR',saveEffects=TRUE),
           W2=list(X=W,model='BayesC',saveEffects=TRUE),
           G=list(X=Z.G,model='BRR',saveEffects=TRUE)
           )

ETA <- ETA[compNames]

cat("Model:",model,". Components:",paste(names(ETA),collapse=" + "),"\n")

outfolder <- "Output/run_VC_full_model_seq"
if(!file.exists(outfolder)) dir.create(outfolder,recursive=TRUE)

fm <- BGLR(y=y, ETA=ETA, nIter=nIter, burnIn=burnIn,
      saveAt=paste0(outfolder,"/M",model,"_P",Pi,"_P",Pj,"_"))

save(fm,file=paste0(outfolder,"/fm_M",model,"_",comp,"_P",Pi,"_P",Pj,".RData"))
