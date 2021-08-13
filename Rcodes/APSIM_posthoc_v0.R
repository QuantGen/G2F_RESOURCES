# Load functions
library(ggplot2)
source('Tools/Functions.R')

SimVersion <- "v0"

# Load data
INFO <- read.csv('Data/OutputFiles/info_loc.csv')
Y <- read.csv('Data/OutputFiles/phenotypes.csv')
load(paste0('Data/APSIM_sim/',SimVersion,'Sim.rdata'))

# Ignore local checks
drop <- which(Y$local_check=="Yes")
if(length(drop)>0) Y <- Y[-drop,]

# Average yield (across hybrids) mean within year-location
dat <- do.call(rbind,lapply(split(Y,paste(Y$location,Y$year)),function(x){
  tt <- x[1,c("year","location")]
  index <- INFO$year==tt$year & as.character(INFO$Location)==tt$location
  data.frame(tt,INFO[index,c("lat","lon","harvesting")],yield=mean(x$yield,na.rm=TRUE))
}))

# Keep some columns and change names
sim <- do.call(rbind,sim)
sim <- sim[,c("year","location","Date","SoilWater.Eo",grep("Maize",colnames(sim),value=T))]
colnames(sim)[grep("Stage",colnames(sim))] <- "stage"
colnames(sim)[grep("AboveGround",colnames(sim))] <- "biomass"
colnames(sim)[grep("Grain",colnames(sim))] <- "yield"

# Select
sim <- sim[sim$stage=="HarvestRipe",]

# paste simulated data
index <- match(paste(sim$year,sim$location),paste(dat$year,dat$location))
dat$simulated_harvesting <- dat$simulated_yield <- NA
dat$simulated_harvesting[index] <- as.character(sim[,"Date"])
dat$simulated_yield[index] <- 10*sim[,"yield"]
dat$year <- as.factor(dat$year)
rownames(dat) <- NULL

get_plot <- function(x){
  fm <- lm(simulated_yield~yield,data=x)
  rg <- range(x[,grep("yield",colnames(x))],na.rm=TRUE)
  rgY <- range(x[,"simulated_yield"],na.rm=TRUE)
  tmp <- c(summary(fm)[["r.squared"]],anova(fm)[["Pr(>F)"]][1])
  textreg <- paste0("sim_yield = ",round(coef(fm)[1],3)," + ",round(coef(fm)[2],3),"*yield")
  text2 <- bquote(R^2 == .(round(tmp[1],4)) ~ ". P-value =" ~ .(round(tmp[2],6)))

  ggplot(x,aes(yield,simulated_yield,label=location,fill=year)) +
   lims(x=rg,y=rg) + theme_bw() +
   geom_abline(slope=coef(fm)[2],intercept=coef(fm)[1],linetype="dotted",color="blue") +
   geom_label(size=2.5) + #,color="blue3") +
   geom_text(x=rg[1],y=rg[2],label=textreg,hjust=0,color="green4") +
   geom_text(x=rg[1],y=rg[2]-0.06*diff(rg),label=deparse(text2),parse=TRUE,
            hjust=0,color="green4") +
   theme(strip.text.x = element_text(size=11, margin=margin(t=2,b=2)),
        legend.box.background = element_rect(colour = "gray32",size=0.8),
        legend.key.height=unit(0.85,"line"), legend.justification=c(1,0))
}

# Within-year plots
pp <- lapply(split(dat,dat$year),function(x){
  p0 <- get_plot(x)
  p0 <- p0 + facet_wrap(~year) + theme(legend.position="none")
})

for(i in 1:length(pp)){
  png(paste0("Data/Images/observed_VS_simulated_yield_",names(pp)[i],".png"),
      width=4.5,height=4.3,res=200,units="in")
  print(pp[[i]])
  dev.off()
}

# Across-years plot
png(paste0("Data/Images/observed_VS_simulated_yield.png"),
    width=4.5,height=4.3,res=200,units="in")
get_plot(dat) + theme(legend.position=c(0.99,0.01))
dev.off()

# Save simulated data
write.table(dat,file=paste0("Data/OutputFiles/simulated_data_",SimVersion,".csv"),sep=",",row.names=FALSE)
