# #Author: Armin Elsler and Tom Stubbs
# #Please cite the associated publication when using this script or parts of it

#1. Preparation####
rm(list=ls())

# #Start logfile
# sink(file="mylogfile.txt")
# #Check that working directory is properly set
wd <- getwd() 
wd 

# #Start the clock!:
start_all <- Sys.time()
start_all

# #Timescaling method
timescaling_method <- switch(3, "Hedman", "Equal", "Cal3")
timescaling_method

# #Additional cal3 options
if(timescaling_method == "Cal3"){
  # #If we use cal3, do we want to estimate sampling, extinction, and origination rate directly from our data (1) or 
  # #do we rely on values reported in the literature (2)
  cal3_rates_approach <- switch(2, "direct", "literature")
  # #Should we use the bin_cal3TimePaleoPhy (1) or the cal3TimePaleoPhy (2) approach?
  cal3_bin <- switch(1, "YES", "NO")
  if(cal3_bin == "YES"){
    # #Use "randObs" (1) or "firstLast" (2) setting?
    cal3_dateTreatment <- switch(1, "randObs", "firstLast")
    if(cal3_dateTreatment == "firstLast"){
      # #Observation times of tips at the start of taxon ranges (1) (= FADs) or at the end (2) (= LADs)
      cal3_FADonly <- switch(1, TRUE, FALSE)
    }
  }
  # #Should we add 0.0001 Myr (= 100 yr) to zero-length branches (1) or not (2)?
  # #It seems we have to - BayesTraits can handle zero-length branches, but the postprocessing tool cannot (will not
  # #recognize clades that we selected)
  add_cal3_time <- switch(1, "YES", "NO")
}

cal3_rates_approach
cal3_bin
cal3_dateTreatment
add_cal3_time

# #Should we exclude trees that have not converged?
exclude_trees <- switch(2, "YES", "NO") # #Exclude (1) or keep (2) trees (and data) for BayesTraits analyses, which have not converged

#Which clade should we focus on
myclade = switch(7, "Tetrapoda", 
                 "Parareptilia", 
                 "Archosauromorpha", 
                 "Synapsida", 
                 "Lepidosauria",
                 "Crocodylomorpha",
                 "Sauropterygia")

myclade
#Switch between different clades

iterations = 120 #several analyses have to be done more than once,
#as they are stochastically pulled

#2. Install and load packages####

# install.packages("ape")        #Install packages if necessary
# install.packages("nlme")
# install.packages("caper")
# install.packages("geiger")
# install.packages("phangorn")
# install.packages("adephylo") #for distRoot() (to find patristic distance of tips)
# install.packages("strap")
# install.packages("coda")      #Needed to analyse MCMC analyses
# install.packages("paleotree" , dependencies=TRUE)
# install.packages("devtools")
# #library(devtools)              #necessary for github package install
# #install_github("dwbapst/paleotree") #github version of paleotree installed which fixes small issue with
# #bin_timePaleoPhy and node.mins
# install.packages("rio") #To import xlsx and export it as csv (has some additional dependencies on Ubuntu Linux: sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev)
# install_formats() # #Installs additional packages which might be required by "rior" package
# install.packages("phylobase") #needed for getNode (easily get node ID on phyl. trees)
# install.packages("TreeSim")  #probably not needed (just trying)
# install.packages("TreePar")  #probably not needed (just trying to possibly use these packages for origination and extinction rate estimates)
# install.packages("ggrepel")
# install.packages("htmltab") #needed to import the postprocessed html result files of the BayesTraits runs (has some additional dependencies on Ubuntu Linux)
# install.packages("paleobioDB") #needed for palaeobioDB to work
# install.packages("BAMMtools")
# install.packages("varhandle") #useful package when tidying up your data - can check, whether character/factor vector can be safely converted to numeric (uses regex)
# install.packages("ks")
# install.packages("contrast")
# install.packages("sensiPhy") #For sensitivity tests using comparative methods (e.g., for PGLS)
# install.packages("AICcmodavg") #provides various AIC(c) measures
# install.packages("rcompanion") #provides nagelkerke() function (to obtain pseudo-R-squared for gls models; see Benson et al. 2017 (2018)
# install.packages("rr2") # #alternative R squared approach: see Ives (2018)
# install.packages("gam") #General Additive Model (but already contained in library("mgcv"))
# install.packages(c("dismo", #Packages for ecological niche modelling
#                     "biomod2",
#                     "ecospat",
#                     "rJava",
#                     "raster",
#                     "maptools",
#                     "rgdal"))
# install.packages("psych") ##neded for harmonic.mean function
# install.packages("lawstat") # #needed for brunner.munzel.test
# install.packages("nparcomp") # #needed for npar.t.test, which is anoter implementation of the Brunner-Munzel test
# install.packages("gridExtra") #might be necessary to create geological timescale for ggtree (but maybe not ^^)
# install.packages("ggtree") ##for tutorials see: http://bioconductor.org/packages/release/bioc/html/ggtree.html
# #It might necessary to install ggtree from source:
# # try http:// if https:// URLs are not supported
# source("https://bioconductor.org/biocLite.R")
# #biocLite("BiocUpgrade") ## you may need this
# biocLite("ggtree")
# biocLite("Biostrings")
# biocLite("EBImage") #(has some additional dependencies on Ubuntu Linux: sudo apt-get install libfftw3-dev libtiff5-dev)
# #Some packages for parallel computing (might be needed to run
# #the code on multiple nodes of a cluster):
# install.packages("Rmpi") #additional Ubuntu dependencies: sudo apt-get install libopenmpi-dev
# install.packages("doMPI")
# install.packages("snowfall")
# install.packages("doSNOW")
# install.packages("future")
# install.packages("doFuture")
# install.packages("future.batchtools")
# install.packages("broom") #replaces the fortify() function (but might not be needed actually)
# install.packages("emojifont") #if you need emojis ;)
# install.packages("ReporteRs") #if you want to export your graphs to Powerpoint or Word (you can generate your complete presentation in R)
# install.packages("devEMF") #if you want to export EMF files (Windows vector graphics format)
# #Packages needed for tomwenseleers/export to run:
# install.packages("rJava")
# install.packages("ReporteRs")
# install.packages("ReporteRsjars")
# install.packages("ggplot2")
# install.packages("rtable") # #NOTE: This package is no longer available for R 3.6.0
# install.packages("xtable")
# install.packages("taRifx")
# install.packages("stargazer")
# install.packages("tikzDevice")
# #library(devtools)
# devtools::install_github("tomwenseleers/export") #neat little package that allows to quickly export graphs to Powerpoint (requires an installation of Java 32bit as well)




# library(psych) #may cause problems with geiger?
library(ggplot2)
library(ape)
library(geiger)
library(strap)
library(paleotree)
library(phytools)
library(nlme)
library(caper)
library(coda)
library(rio)
library(phangorn) # #Needed for function Children()
#library(varhandle) # #Needed for function check.numeric() - varhandle is currently not installed on all clusters - therefore we
# #avoid using check.numeric() for now (but just for now!)
#library(AICcmodavg) # #required for the AICc() function and aictab - actually we don't need this library for now (as I have my own aicc functions)
#library(rcompanion) # #required for the nagelkerke() function (alternatively I could just implement it myself - the formula is given in Nagelkerke (1991))
# #For the necessary null model see https://github.com/cran/rcompanion/blob/master/R/nagelkerke.r : use update() function to fit a model with the
# #dependant variable ~ 1 (i. e. without a slope)
library(rr2) # #Alternative R squared approach (masks binaryPGLMM from ape)
library(lattice) #necessary for some of the convergence plots
library(foreach) #load foreach library (needed to parallelize some of our tasks)
library(parallel) #load parallel library (needed to parallelize some of our tasks)
library(doParallel) #load doParallel library (needed to parallelize some of our tasks)
#library(Rmpi) #needed if you want to run the code on multiple nodes
#library(phylobase) #Not needed for now. There are other ways of solving the problem.
#It would be needed to get node ID. This masks "edges" from ape - if this causes any issues,
#we have load the library use the functions and then detach it with the following expression:
#detach("package:phylobase", unload=TRUE)
# library(TreeSim)  #probably not neeeded (see above)
# library(TreePar)  #probably not needed (see above)
# library(ggrepel) #maybe not needed (is for graphs and positioning labels)
# library(ggtree) #only load ggtree when you need it (as it masks some of the functions of other tools)
# library(BAMMtools)
source("myfunctions.r") # #Import my functions

# #Add ICS2017 to geoscale
# #You only need to install the patched version once - afterwards, library(geoscale) will always load the
# #patched version (unless you reinstall geoscale)
# require(geoscale)
# timescales$ICS2017 <- read.csv("./Replace_taxa_ages/Stratigraphic_scale/timescales_ICS2017.csv", sep = ";")
# #oldTimescale <- load("./Replace_taxa_ages/Stratigraphic_scale/geoscale_ORIGINAL/data/timescales.rda", verbose=TRUE)
# setwd("./Replace_taxa_ages/Stratigraphic_scale/geoscale")
# save(timescales, file = "./data/timescales.rda")
# library(devtools)
# build()
# install()
# library(geoscale) # #This is the patched version of the geoscale package (including ICS2017)

#Some useful information regarding the current session (you can check for packages, that
#are based on )
sessionInfo()
packageVersion("paleotree") #based on github version


# ####Parallel tasks####
# #Set useful settings for parallel tasks
# #If the script is run on the cluster, set the number of cores
# #to run equal to the number of cores assigned.
# #If the script is NOT run on the cluster, then set the number
# #of cores to use equal to the number of cores on the system.
# #This is necessary, as detectCores() will only detect the cores,
# #that are available on one (!) node, i. e. it won't detect cores
# #on multiple nodes
if (Sys.getenv("PBS_NODEFILE") != ""){
  # #The following line of code works for BlueCrystal 3 but not for BluePebble
  #if(as.numeric(Sys.getenv("PBS_NUM_NODES")) > 1){ # #Use this when you run your code on the cluster with multiple nodes!
  # #The following line of code works for both BC 3 and BluePebble
  if(length(system("cat $PBS_NODEFILE | uniq", intern=TRUE)) > 1){ # #Use this when you run your code on the cluster with multiple nodes!
    library(doSNOW)
    if (Sys.getenv("PBS_NUM_PPN") != ""){
      nCoresPerNode <- as.numeric(Sys.getenv("PBS_NUM_PPN")) # #For BC 3
    } else{
      nCoresPerNode <- as.numeric(Sys.getenv("NCPUS")) # #For BluePebble
    }
    nCoresPerNode
    nodeNames <- system("cat $PBS_NODEFILE | uniq", intern=TRUE)
    nodeNames
    machines <- rep(nodeNames, each = nCoresPerNode)
    cl <- makeCluster(machines, type = "SOCK")
    registerDoSNOW(cl)
    
    # #How many workers are we using?
    getDoParWorkers()
  } else{ # #Use this when you run your code on the cluster with just 1 (!) node!
    if (Sys.getenv("PBS_NUM_NODES") != ""){
      no_cores <- as.numeric(system("cat $PBS_NODEFILE|wc -l",intern=TRUE)) #grab number of cores (BC 3)
      # #system only provides us with a string, therefore we need as.numeric
    } else{
      no_cores <- as.numeric(Sys.getenv("NCPUS")) #grab number of cores (BluePebble)
      # #system only provides us with a string, therefore we need as.numeric
    }
    print(no_cores) # #Check number of cores
    class(no_cores) # #Check class of no_cores
    
    # #Register parallel back end (must have this many cores available):
    registerDoParallel(cores = no_cores)
  }
} else{ # #Use this when you don't run your code on the cluster with multiple nodes!
  no_cores <- detectCores()# - 2 # Calculate the number of cores
  print(no_cores) # #Check number of cores
  class(no_cores) # #Check class of no_cores
  
  # #Register parallel back end (must have this many cores available):
  registerDoParallel(cores = no_cores)
}


#2. Import data####
Supertree.tree <- read.tree("Sauropterygia.tre")
Supertree.tree
plot.phylo(Supertree.tree, cex=0.3, show.node.label = T)
write.tree(Supertree.tree, file="Supertree.tre") #Converts tree to Newick format

#Import the .txt file used to time-scale the phylogeny
Supertree.ages <- read.table("Sauropterygians_ages.txt", header=T, row.names = 1)
Supertree.ages

Supertree.tree <- read.tree("Supertree.tre") #Read previously created Newick tree
#This is needed, as NEXUS files sometimes cause problems in R

####3. Time-scale the phylogeny####
####Preparations####
name.check(Supertree.tree,Supertree.ages) #Check if all species names are present in both
#the file used to time-scale the tree and the tree itself

#Check if all species names are present in both the file used to time-scale the tree and the tree itself
#Delete taxa from Supertree.ages dataset that are not in tree (i.e., all those taxa that are in the $data_not_tree component of the name.check output)
#The if...else statement is necessary, as name.check does not create an "$data_not_tree" object, if
#all taxa are present in both Supertree.tree and Supertree.ages (causes the error "$ operator is invalid for atomic vectors")
if(name.check(Supertree.tree, Supertree.ages)=="OK"){
  Supertree.ages <- Supertree.ages
} else{
 Supertree.ages <- Supertree.ages[! rownames(Supertree.ages) %in% name.check(Supertree.tree, Supertree.ages)$data_not_tree,]
}
name.check(Supertree.tree, Supertree.ages) #Confirm that tree and data are now in agreement

#####Set time constraints for clade we are interested in#####
dateConstraint<- if(myclade=="Sauropterygia"){
  dateConstraint<-270
} 

#Import/Extract myclade tree
myclade.tree <- Supertree.tree
myclade.ages <- Supertree.ages
####Preparation for time-scaling the tree#####
name.check(myclade.tree,Supertree.ages) #Check if all species names are present in both
#the file used to time-scale the tree and the tree itself

mytimescale <- read.csv("My_timescale.csv", sep=";",row.names=1)
mytimescale

bins <- as.vector(mytimescale[,3])
names(bins) <- row.names(mytimescale)
bins

# assign taxa to list of time bins
taxon.bins <- list()
for (i in 1:length(rownames(mytimescale))) {taxon.bins[[i]] <- rownames(myclade.ages)[which(myclade.ages$FAD > mytimescale[i,"End"] & myclade.ages$LAD < mytimescale[i,"Start"])]}
names(taxon.bins) <- rownames(mytimescale)
taxon.bins

PA_TIME_FILE <- matrix(NA, ncol=length(taxon.bins), nrow=length(myclade.ages[,1]))
rownames(PA_TIME_FILE) <- rownames(myclade.ages)
colnames(PA_TIME_FILE) <- names(taxon.bins)
for(i in 1:length(taxon.bins)) {
  PA_TIME_FILE[,i][taxon.bins[[i]]] <- "1"
}
PA_TIME_FILE
PA_TIME_FILE
write.table(PA_TIME_FILE,"PA_TIME_FILE.txt", sep = "\t")

first_last <- matrix(NA, ncol=2, nrow=length(myclade.ages[,1]))
for(i in 1:length(taxon.bins)) {
  PA_TIME_FILE[,i][taxon.bins[[i]]] <- "1"
}

first <- which(PA_TIME_FILE==1, arr.ind=TRUE)
first <- first[,2]

first_last <- matrix(NA, ncol=2, nrow=length(myclade.ages[,1]))
rownames(first_last) <- rownames(myclade.ages)
first_last

for(i in 1:length(myclade.ages[,2])) {
NonNAindex <- which(!is.na(PA_TIME_FILE[i,]))
firstNonNA <- min(NonNAindex)
first_last[,1][i] <- firstNonNA 
lastNonNA  <- max(NonNAindex)
first_last[,2][i] <- lastNonNA
}
first_last

write.csv(first_last, "time.list.R.csv")

cal3timedata <- read.csv("time.list.R.csv", header = TRUE, row.names = 1)
cal3timedata

mytimescale <- read.csv("My_timescale.csv", sep = ";")
mytimescale

timeList <- list(mytimescale[,2:3],cal3timedata)

mytimeList.myclade <- timeList
mytimeList.myclade

#####timeList#####
#Create a timeList, which is necessary for several functions in paleotree (e.g. to plot raw taxic richness over time)
#it contains our timescale (substages and assigned absolute ages) and the range data for
#tetrapod species, i.e. the species name and the first and last interval it was found in -
#note that intervals are given as numbers (i.e. 1=GIV(l), 2=GIV(u), ...)
#Plot raw taxa richness (number of species per time)
taxicDivDisc(mytimeList.myclade, timelims=c((max(myclade.ages)),(min(myclade.ages))))
#the time scale is set to the "max. age +10 Myr" and "min. age -10 Myr" of the clade
myclade.rawdiversity<-taxicDivDisc(mytimeList.myclade, timelims=c((max(myclade.ages)+10),(min(myclade.ages)-10))) #Plot raw taxa richness (number of species per time)
####Quick and dirty plot with geoscale####
#Use midpoint age
raw.mid_age <- (myclade.rawdiversity[, "int.start"]+myclade.rawdiversity[, "int.end"])/2
#Use FAD instead (midpoint age is better for line plot)
raw.FAD <- (myclade.rawdiversity[, "int.start"])
#Use median diversity (phylogenetic diversity estimate)
raw.diversity <- myclade.rawdiversity[, "int.div"]
#Plot your data
geoscalePlot(raw.FAD,raw.diversity,type="s",
             units=c("Period","Age"),age.lim = c((max(myclade.ages)+1),(min(myclade.ages)-1)),
             data.lim=c(0,55),cex.age=1.0, cex.ts=0.9, label="Raw taxic richness")


# #Generate a set of trees, which is then to be randomly resolved and timescaled
tmp_Hedman_trees <- rep.multiPhylo(myclade.tree, iterations)
tmp_Hedman_trees


# #Cal3 timescaling
# #Get rates necessary for cal3 timescaling

# #Using seqTimeList (as in Lloyd et al., 2016) is not necessary, as intervals do not overlap

# #First, we can try the freqRat function (Foote and Raup, 1996), which is just a simple
# #ratio of the frequencies of taxa found in just one, two and three intervals.
# #Let's apply that to "mytimeList.myclade" and tell it to plot the histogram of taxon durations.
freqRat(mytimeList.myclade, plot=TRUE) # #Note, that the frequency distribution of input range data appears to violate
# #model assumptions, producing an impossible freqRat greater than 1 (freqRat: 1.19987)
# #This might explain some of the issues we have in estimating rates

# #Find mean substage length
intLength <- -apply(mytimeList.myclade[[1]],1,diff)
intLength
hist(intLength) # #Note the uneven substage length
meanInt <- mean(intLength)
meanInt # #Our mean substage length

# #Construct likelihood models of the observed frequency of taxon durations
# #These model are necessary to find the instantaneous per-capita extinction 'q' and the
# #per-interval taxonomic sampling probability 'R'
likFun.myclade <- make_durationFreqDisc(mytimeList.myclade, groups = NULL)

if(cal3_rates_approach == "direct"){
  # #Find instantaneous per-capita extinction 'q' and per-interval taxonomic sampling probability 'R'
  likFun_optim.myclade <- optimPaleo(likFun.myclade)
  likFun_optim.myclade # #Optimizer seems to have converged ($convergence = 0), but note that the taxonomic sampling
  # #probability R.1 appears to to be very high (0.8005801)! For smaller clades, e.g. Parareptilia, it can be even
  # #higher, up to R.1 = 1!
  # #We replicate the result for each tree (this is not per se necessary, but by doing this we do not have to rewrite
  # #if statements to check whether we use this approach or the alternative one)
  likFun_optim.myclade <- rep(list(likFun_optim.myclade), length(tmp_Hedman_trees))
  
  # #We could attempt to change the optimization process
  # #We modify our optimization and set maxit higher
  # likFun_optim.myclade <- optim(parInit(likFun.myclade), likFun.myclade,
  #                               lower = parLower(likFun.myclade), upper = parUpper(likFun.myclade),
  #                               method = "L-BFGS-B", control = list(maxit=1000000000))
  # likFun_optim.myclade # #But we get nearly the same result for R.1 (0.8005809)
  
  
  
  # #We can transform sampling probability to sampling rate using the function sProb2sRate
  # #and the mean interval length:
  # #We replicate the result again for each tree (this is not per se necessary, but by doing this we do not have
  # #to rewrite if statements to check whether we use this approach or the alternative one)
  sRate.myclade <- vector()
  for (i in 1:length(tmp_Hedman_trees)){
    sRate.myclade[i] <- sProb2sRate(likFun_optim.myclade[[i]]$par[2], int.length = meanInt)
  }
  sRate.myclade # #Instantaneous (per-capita) rate of sampling = 0.4378095. This value still appears to be very
  # #high --> indeed, for smaller clades (e.g., Parareptilia) I even got an instantaneous (per-capita) sampling 
  # #rate of Inf (when R.1 = 1) --> see discussions with Dave Bapst in regards to the frequency distribution of taxon
  # #durations (see also Maxwell et al., 2018; Bapst & Hopkins, 2017, p. 53; Sould & Friedman, 2017, p. 172)
  # #Is there a way to solve this conundrum? I could use 0.01 for the per-capita sampling rate,
  # #as mentioned by Soul & Friedman (2017), but there has to be a better way (?). Am I missing something?
  # #In Lloyd et al. (2016) values, which are Inf, are dropped as erroneous, but I don't have a stochastic sequenced
  # #time-list to allow for multiple iterations!
} else if(cal3_rates_approach == "literature"){
  # #Alternative: constrain the sampling rate based on values derived from the literature
  # #Soul and Friedman (2017, p. 181) suggest a per-capita sampling rate on the order of 0.01 Lmy
  # #for Paleozoic and Mesozoic terrestrial tetrapods (see also Friedman & Brazeau, 2011; Bapst & Hopkins, 2017, Table 2)
  # #According to Bapst & Hopkins (2017, Table 2) the per-capita sampling rates range from
  # #0.042 to 0.18 for Devonian tetrapods at genus level (see also Friedman & Brazeau, 2011). As many tetrapods are represented
  # #by monospecific genera, these values should (probably?) also apply to the species level. For Dinosauria the sampling rate
  # #is 0.018 (D. Bapst in Benson et al., 2018, p. 16; see Lloyd et al., 2016)
  # #We generate a uniform distribution with min = 0.018 and max = 0.18 from which we sample our 
  # #per-capita sampling rates
  sRate.myclade <- runif(n = length(tmp_Hedman_trees), min = 0.018, max = 0.18)
  
  # #What is the per-interval taxonomic sampling probability R for a given r.1?
  interval.sampling <- sRate2sProb(sRate.myclade, int.length = meanInt) # #per-interval taxonomic sampling probability R
  likFun_constraint <- list()
  for (i in 1:length(tmp_Hedman_trees)){
    tmp_interval.sampling <- interval.sampling[i] # #We have to use a tmp variable, since I cannot specify 
    # #interval.sampling[i] directly in constrainParPaleo() (will throw an error - have to figure out, why that is the case)
    likFun_constraint[[i]] <- constrainParPaleo(likFun.myclade, R.1 ~ tmp_interval.sampling)
  }
  
  # #Find instantaneous per-capita extinction 'q'
  likFun_optim.myclade <- lapply(likFun_constraint, optimPaleo)
  
  # #We could again attempt to change the optimization process
  # #We modify our optimization and set maxit higher
  # likFun_constraint_optim <- list()
  # for (i in 1:length(tmp_Hedman_trees)){
  #   likFun_constraint_optim[[i]] <- optim(parInit(likFun_constraint[[i]]), likFun_constraint[[i]],
  #                                         lower = parLower(likFun_constraint[[i]]), upper = parUpper(likFun_constraint[[i]]),
  #                                         method = "L-BFGS-B", control = list(maxit=1000000))
  #   }
  
  # #Alternatively we could also just fix our sampling rate to 0.01 Lmy (see Sould and Friedman, 2017, p. 181)
  # #Constraining sampling rate to 0.01 Lmy
  # #r.1=0.01
  # #R?
  # interval.sampling <- sRate2sProb(0.01, int.length = meanInt)
  # likFun_constraint<-constrainParPaleo(likFun.myclade,R.1~interval.sampling)
  # likFun_constraint_optim <- optim(parInit(likFun_constraint),likFun_constraint,
  #                                  lower=parLower(likFun_constraint),upper=parUpper(likFun_constraint),
  #                                  method="L-BFGS-B",control=list(maxit=1000000))
  # likFun_constraint_optim # #Hmm, q.1 is now suddenly 0.001 - but this is equal to the lower constraint: (parLower(likFun_constraint))
  # #What's going on here?
  # #Could this be due to the uneven stage length? See help file for durationFreq:
  # #"It is unclear how to treat uneven intervals and I urge workers to consider multiple strategies."
  # #Therefore we avoid doing this for now (at least when it comes to a sampling rate of 0.01 Lmy)
}


# #To get the extinction rate (= origination rate; at least, that's what we assume)
# #we simply divide the calculated extinction rate by the interval length.
divRate.myclade <- list()
for(i in 1:length(tmp_Hedman_trees)){
  divRate.myclade[[i]] <- likFun_optim.myclade[[i]]$par[1]/meanInt
}
divRate.myclade # # Extinction rate is now per Lmy

# #We could also exclude all infinite values see also Lloyd et al. (2016)
# #But there should not be any (since the sampling rates are fixed between 0.018 and 0.18)
# #or alternatively the estimated one, which is also clearly not Inf
# #So there is currently no need for this
# #Otherwise we would have to exclude those rates - in that case we would have to produce more 
# #rate estimates and then sample out of them to match the number of trees (otherwise there would
# #not be enough rate estimates for the cal3 scaling as implemented below)

if(cal3_bin == "NO"){
  # #Cal3 timescaling: cal3TimePaleoPhy using minMax and the root.age of nodeDates.resolved
  # #Note, that the default cal3 approach adds the terminal ranges, unless you specify FAD.only = TRUE, which is not
  # #possible, however, when dateTreatment is set to "minMax" or "randObs"
  hedman_trees <- foreach(i = 1:length(tmp_Hedman_trees), .packages = "paleotree", .errorhandling = "remove") %dopar% {
    out <- cal3TimePaleoPhy(tree = tmp_Hedman_trees[[i]], timeData = myclade.ages, 
                            brRate = divRate.myclade[[i]], extRate = divRate.myclade[[i]], sampRate = sRate.myclade[i],
                            #node.mins = nodeDates.resolved, # #Cal3 does not require to specify a min. root age
                            #randres=TRUE, #Randres could be used if we were to resolve polytomies within cal3 and if we did not
                            # #want to account for direct ancestors (sparse record of tetrapods is unlikely to contain those) # #We use randres, as it is unlikely that the sparse record of tetrapods contains ancestors + it retains our node labels
                            dateTreatment = "minMax",
                            step.size = 0.001, # #Reducing the step size of increments used in zipper algorithm to assign node ages
                            # #leads to fewer branches with zero-length
                            #add.term = FALSE, # #add.term does not work with cal3 method
                            anc.wt = 0, # #It's very unlikely that ancestors have been sampled in the fossil record of early tetrapods!
                            # #Therefore no taxon is allowed to be an ancestor.
                            ntrees = 1, plot = FALSE)
  }
} else if(cal3_bin == "YES"){
  if(cal3_dateTreatment == "randObs"){
    # #Using bin_cal3TimePaleoPhy (with randObs) instead of cal3TimePaleoPhy (with minMax)
    # #Some of the trees used by Cantalapiedra et al. (2017), have been timescaled using this approach
    # #Note, however, that this approach assumes that the FAD is treated as a fixed/certain number (only
    # #the LAD is treated as being uncertain)
    # #Unlike the other cal3 methods, however, the terminal range does not appear to be added, allowing the LAD
    # #(or rather the 'time of observation') to vary
    hedman_trees <- foreach(i = 1:length(tmp_Hedman_trees), .packages = "paleotree", .errorhandling = "remove") %dopar% {
      out <- bin_cal3TimePaleoPhy(tree = tmp_Hedman_trees[[i]], timeList = mytimeList.myclade,
                                  brRate = divRate.myclade[[i]], extRate = divRate.myclade[[i]], sampRate = sRate.myclade[i],
                                  #node.mins = nodeDates.resolved, # #Cal3 does not require to specify a min. root age
                                  #randres=TRUE, #Randres could be used if we were to resolve polytomies within cal3 and if we did not
                                  # #want to account for direct ancestors (sparse record of tetrapods is unlikely to contain those) # #We use randres, as it is unlikely that the sparse record of tetrapods contains ancestors + it retains our node labels
                                  dateTreatment = "randObs",
                                  step.size = 0.001, # #Reducing the step size of increments used in zipper algorithm to assign node ages
                                  # #leads to fewer branches with zero-length
                                  #add.term = FALSE, # #add.term does not work with cal3 method
                                  anc.wt = 0, # #It's very unlikely that ancestors have been sampled in the fossil record of early tetrapods!
                                  # #Therefore no taxon is allowed to be an ancestor.
                                  ntrees = 1, plot = FALSE) #Not using randres (for now) + minMax (Lloyd et al., 2016) also did not do that: Why? Ask him!
    }
    # #An alternative approach would be to use '4 dates' for FAD and LAD, i. e. the lower and upper bound of the FAD
    # #and the lower and upper bound of the LAD - since we don't necessarily have the data for this in the ETD, we could
    # #just use our current lower and upper bounds of the strat. range of a taxon to provide uniform distributions for
    # #the FAD and LAD of our taxa, i. e. FAD: uniform distribution bound by oldest and youngest occurrence of taxon
    # #and LAD: uniform distribution bound by oldest and youngest occurrence of taxon --> we sample from both uniform distributions
    # #and use the sampled oldest date as FAD and the sampled youngest date as LAD - this way the FAD would no longer be fixed
    # #Note, that we could sample by ourself from the uniform distributions or let bin_cal3TimePaleoPhy() do the job - I think the latter
    # #does it already to a certain extent using "Start" and "End" dates of our different intervals
  } else if(cal3_dateTreatment == "firstLast"){
    # #Using bin_cal3TimePaleoPhy (with firstLast) instead of cal3TimePaleoPhy (with minMax)
    # #For speciation dynamics analyses
    hedman_trees <- foreach(i = 1:length(tmp_Hedman_trees), .packages = "paleotree", .errorhandling = "remove") %dopar% {
      out <- bin_cal3TimePaleoPhy(tree = tmp_Hedman_trees[[i]], timeList = mytimeList.myclade,
                                  brRate = divRate.myclade[[i]], extRate = divRate.myclade[[i]], sampRate = sRate.myclade[i],
                                  #node.mins = nodeDates.resolved, # #Cal3 does not require to specify a min. root age
                                  #randres=TRUE, #Randres could be used if we were to resolve polytomies within cal3 and if we did not
                                  # #want to account for direct ancestors (sparse record of tetrapods is unlikely to contain those) # #We use randres, as it is unlikely that the sparse record of tetrapods contains ancestors + it retains our node labels
                                  dateTreatment = "firstLast",
                                  step.size = 0.001, # #Reducing the step size of increments used in zipper algorithm to assign node ages
                                  # #leads to fewer branches with zero-length
                                  FAD.only = cal3_FADonly,
                                  #add.term = FALSE, # #add.term does not work with cal3 method
                                  anc.wt = 0, # #It's very unlikely that ancestors have been sampled in the fossil record of early tetrapods!
                                  # #Therefore no taxon is allowed to be an ancestor.
                                  ntrees = 1, plot = FALSE) #Not using randres (for now) + minMax (Lloyd et al., 2016) also did not do that: Why? Ask him!
    }
  }
}

# #Note that we could also timescale every randomly resolved tree several times (like Lloyd et al., 2016), but this seems unnecessary
# #when we just start with one tree, that is randomly resolved (= all random resolutions should be equally represented)
# #It is different, however, if we were to use MPTs or Bayesian posteriors, as the trees there can differ quite much (therefore
# #we can't just timescale one of them a single time) - this might also apply to trees with many polytomies
# #For now we will stick with the easier (and faster) way of timescaling a randomly resolved tree just once (Halliday & Goswami (2016) did
# #that as well)! Note, also that timescaling every randomly resolved tree several times is computationally currently
# #not feasible (at least for the downstream analyses), unless we randomly sample another 100 trees from this set of
# #timescaled trees. But then again, why would you even bother with timescaling the 100 trees multiple times, if you
# #end up with 100 trees anyway - let's just keep it simple for now (consider also unexplored side effects, of random 
# #sampling, e.g., would the initial 100 different topologies still all be represented in your new tree sample?)


if(add_cal3_time == "YES"){
  # #Unfortunately, the cal3 algorithm tends to produce many branches with zero-lengths
  # #BayesTraits should be able to deal with this (since it can deal with polytomies)
  # #Other methods, however, cannot deal with zero-length branches
  # #A simple workaround would be to add a very small amount of time to the zero-length branches
  # #If the added time is small enough, it should not lead to significant change of the tree structure
  # #and it avoids the problems associated with zero-length branches
  # #We add 0.0001 Myr (= 100 yr) to branches with a zero-length branch
  # #If we were to add only 10 years and were to adjust the step size accordingly to 0.0001 instead of 0.001,
  # #we would run into RAM memory issues (at least on my local machine and probably on BC3 - BC4 might be able to
  # #cope with it)
  for(i in 1:length(hedman_trees)){
    hedman_trees[[i]]$edge.length[hedman_trees[[i]]$edge.length == 0] <- 0.0001
  }
}
# #If we use the cal3 trees, we have to delete the "sampledLogLike" column from our trees object - otherwise
# #a single tree will use up more than 600 Mb (!) when using a step size of 0.0001, which is way to large and
# #causes issues when trying to save our workspace
for(i in 1:length(hedman_trees)){
  hedman_trees[[i]]$sampledLogLike <- NULL
}

hedman_trees

# #Set class of hedman_trees to "multiphylo"
class(hedman_trees) <- "multiphylo"

# Stop the clock!:
end <- Sys.time()

# How long did it take?:
end - start_all

roots <- list()
for(i in 1:length(hedman_trees)){
  roots[i]<- print(hedman_trees[[i]]$root.time)
}

sample_these_trees <- which(roots<dateConstraint, arr.ind=TRUE)
length(sample_these_trees)
sample_these_trees <- sample(sample_these_trees,100, replace = FALSE)

myclade.ts.trees <- hedman_trees[sample_these_trees]

for(i in 1:length(myclade.ts.trees)){
  print(myclade.ts.trees[[i]]$root.time)
}

check.branches <- data.frame(matrix(ncol = length(myclade.ts.trees), nrow = length(myclade.ts.trees[[1]]$edge.length)))
for(i in 1:length(myclade.ts.trees)){
  check.branches[,i] <- myclade.ts.trees[[i]]$edge.length
}
check.branches

unlist(check.branches)
min(unlist(check.branches))
max(unlist(check.branches))
length(which(unlist(check.branches)==0.0001))/length(unlist(check.branches))


pick <- sample(1:100, 1)
pick
pick2 <- sample(1:100, 1)
pick2
pick3 <- sample(1:100, 1)
pick3
pick4 <- sample(1:100, 1)
pick4
geoscalePhylo(ladderize(myclade.ts.trees[[pick]]), cex.tip = 0.3)
dev.copy(pdf,'sample_dated_supertree_1.pdf', width = 15, height = 15)
dev.off()
geoscalePhylo(ladderize(myclade.ts.trees[[pick2]]), cex.tip = 0.3)
dev.copy(pdf,'sample_dated_supertree_2.pdf', width = 15, height = 15)
dev.off()
geoscalePhylo(ladderize(myclade.ts.trees[[pick3]]), cex.tip = 0.3)
dev.copy(pdf,'sample_dated_supertree_3.pdf', width = 15, height = 15)
dev.off()
geoscalePhylo(ladderize(myclade.ts.trees[[pick4]]), cex.tip = 0.3)
dev.copy(pdf,'sample_dated_supertree_4.pdf', width = 15, height = 15)
dev.off()

save(myclade.ts.trees, file = "cal3trees.RData")
saveRDS(myclade.ts.trees, "cal3trees.rds")
write.nexus(myclade.ts.trees, file = "cal3trees.nex", translate = TRUE)


