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
timescaling_method <- "Hedman"
timescaling_method

#Hedman outgroup mode
Hedman_out_mode <- switch(3, "LAD", "MID", "FAD", "random") #Use LADs (1) or draw dates from a uniform distribution (2) for outgroups
Hedman_out_mode

hedman_reso = 10000 # 10000 # #Set resolution for Hedman method (min. is 1000, but I recommend 10,000 for larger analyses)
hedman_reso

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

iterations = 50 #several analyses have to be done more than once,
iterations #as they are stochastically pulled

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
#dateConstraint<- if(myclade=="#"){
#  dateConstraint<-251.902
#} 

#Import/Extract myclade tree
myclade.tree <- Supertree.tree
myclade.ages <- Supertree.ages
####Preparation for time-scaling the tree#####
name.check(myclade.tree,Supertree.ages) #Check if all species names are present in both
#the file used to time-scale the tree and the tree itself

# #### Run the Hedman timescaling####
# Load functions and libraries:
#source("http://www.graemetlloyd.com/pubdata/functions_7.r")
# #Hedman functions were downloaded from "http://www.graemetlloyd.com/pubdata/functions_7.r"
source("Hedman_functions.r")

# #Generate a set of trees, which is then to be randomly resolved and timescaled
tmp_Hedman_trees <- rep.multiPhylo(myclade.tree, iterations)
tmp_Hedman_trees

# #Find max. and min. ages of our taxa (= FAD and LAD)
max.ages <- myclade.ages$FAD
min.ages <- myclade.ages$LAD

# #Update names:
names(max.ages) <- names(min.ages) <- rownames(myclade.ages)

# #Outgroup ages:
outgroup.ages <- read.table("OUTGROUPS.txt", header=T, row.names=1)
#outgroup.ages <- outgroup.ages[-c(#), ] 
outgroup.ages

# #Convert trees back to multiphylo object:
class(tmp_Hedman_trees) <- "multiPhylo"

# #Randomly resolve the trees
tmp_Hedman_trees <- multi2di.multiPhylo(tmp_Hedman_trees)

# #Create node.mins vector with a length that corresponds to the number of nodes of
# #the tree - we cannot use the "nodeDates" object, since we want to use a fully resolved tree
# #which obviously has more internal nodes (these objects are not needed for the Hedman method,
# #but for the other timescaling methods)
#nodeDates.resolved <- rep(NA, Nnode(tmp_Hedman_trees[[1]]))
#nodeDates.resolved[1] <- dateConstraint # #Assign dateConstraint to root (= 1st node = 1st entry in nodeDates)

# #Randomly pull time of observation from uniform distribution defined by FAD and LAD
# #First create a list of myclade.ages for all trees
timeData <- list()
timeData_outgroup <- list()
for (i in 1:length(tmp_Hedman_trees)){
  timeData[[i]] <- myclade.ages
  timeData_outgroup[[i]] <- outgroup.ages
}

# #Now sample from uniform distribution
saveTD <- timeData
saveTD_outgroup <- timeData_outgroup
for (i in 1:length(tmp_Hedman_trees)){
  timeData[[i]][, 1:2] <- apply(saveTD[[i]], 1, function(x) runif(1, min = x[2], max = x[1]))
  timeData_outgroup[[i]][, 1:2] <- apply(saveTD_outgroup[[i]], 1, function(x) runif(1, min = x[2], max = x[1]))
}
timeData
timeData_outgroup

# #Bring the timedata into the format used by Hedman.tree.dates
time_uniform <- list()
time_uniform_outgroup <- list()
for (i in 1:length(tmp_Hedman_trees)){
  time_uniform[[i]] <- timeData[[i]]$FAD
  names(time_uniform[[i]]) <- rownames(timeData[[i]])
  time_uniform_outgroup[[i]] <- timeData_outgroup[[i]]$FAD
  names(time_uniform_outgroup[[i]]) <- rownames(timeData_outgroup[[i]])
}


# #Start the clock!:
start <- Sys.time()

if(timescaling_method == "Hedman"){
  # Main parallelised loop:
  hedman_output <- list()
  # #If the resolution is too low (below 1000), the Hedman method as implemented here won't work
  # #see Lloyd et al. (2016, Supplementary Information) and the description of Hedman.tree.dates()
  # #for more information
  # #(If increasing the resolution does not help, we might have to skip some trees: you can use ".errorhandling = 'remove'"
  # #or ".errorhandling = 'pass'" in the foreach loop for that!)
  # #We use time_uniform instead of min.ages, as we draw our time of observation from the uniform distribution between FAD and LAD
  # #FAD and LAD have no further meaning in the time_uniform object, as we assign the same age that was sampled from the uniform distribution
  # #to both FAD and LAD (that's why we can drop one of the two columns - we could keep both, if we wanted the
  # #possibility to perform more sophisticated timescaling - see examples of timePaleoPhy)
  # #The default is to use only the LADs for the outgroups (see Lloyd et al., 2016)
  # #We could also draw them from a uniform distribution, but this might have unexplored side-effects, as some of the outgroups
  # #could then suddenly appear in a stratigraphic order that is not congruent sensu Hedman (2010)
  # #Therefore we prefer the more conservative LAD choice!
  if(Hedman_out_mode == "FAD"){# #Use FAD or LADs for outgroups
    hedman_output <- foreach(i = 1:length(tmp_Hedman_trees), .errorhandling = "remove") %dopar% {
      out <- Hedman.tree.dates(tmp_Hedman_trees[[i]], time_uniform[[i]], outgroup.ages$MID, t0 = max(outgroup.ages$FAD)+1, resolution = hedman_reso, conservative = TRUE)
    }
  } else{# #Draw outgroup ages from a uniform distribution
    hedman_output <- foreach(i = 1:length(tmp_Hedman_trees), .errorhandling = "remove") %dopar% {
      out <- Hedman.tree.dates(tmp_Hedman_trees[[i]], time_uniform[[i]], time_uniform_outgroup[[i]], t0 = max(outgroup.ages$FAD)+1, resolution = hedman_reso, conservative = TRUE)
    }
  }
  # #Note that increasing the resolution for the Hedman method has a massive effect on the performance of the method
  # #Timescaling the complete early tetrapod tree with a resolution of 10,000 takes about 25 hours, while doing the same
  # #with a resolution of 20,000 takes about 4.5 days (assuming one core per tree) - and still in both cases the resolution
  # #was not high enough and the timescaling stopped with an error (all trees were dropped!)!
  
  # #How many trees were discarded during the Hedman timescaling?
  hedman_removed <- length(tmp_Hedman_trees) - length(hedman_output)
  hedman_removed
  hedman_removed_pct <- 100*hedman_removed/length(tmp_Hedman_trees) # #Percentage of original trees removed
  hedman_removed_pct
  
  rm(tmp_Hedman_trees) #Remove temporary Hedman trees (which are not needed anymore)
  
  
  # #Extract trees from the Hedman output
  hedman_trees <- list()
  for(i in 1:length(hedman_output)){
    hedman_trees[[i]] <- hedman_output[[i]]$tree
  }
  
}

hedman_removed
hedman_removed_pct

# #Set class of hedman_trees to "multiphylo"
class(hedman_trees) <- "multiphylo"

# Stop the clock!:
end <- Sys.time()

# How long did it take?:
end - start

myclade.ts.trees <- hedman_trees

# #Save my workspace after the Hedman timescalung
save.image(file = paste(myclade,".Hedmanonly.Workspace.RData", sep=""))
#load(file = paste(myclade,".Hedmanonly.Workspace.RData", sep="")) #If you want to save time and only load the calculated results

if(timescaling_method == "Hedman"){
  rm(hedman_output) #Remove the Hedman output, which has a massive memory footprint (we only need the trees, nothing else)
}

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


pick <- sample(1:50, 1)
pick
pick2 <- sample(1:50, 1)
pick2
pick3 <- sample(1:50, 1)
pick3
pick4 <- sample(1:50, 1)
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

save(myclade.ts.trees, file = "Hedmantrees.RData")
saveRDS(myclade.ts.trees, "Hedmantrees.rds")
write.nexus(myclade.ts.trees, file = "Hedmantrees.nex", translate = TRUE)
