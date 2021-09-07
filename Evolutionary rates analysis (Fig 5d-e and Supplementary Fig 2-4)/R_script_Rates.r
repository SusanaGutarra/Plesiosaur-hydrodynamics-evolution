# #Author: Armin Elsler
# #Please cite the associated publication when using this script or parts of it, and the following thesis
# 1. https://research-information.bris.ac.uk/en/studentTheses/macroevolution-of-early-tetrapods 

#1. Preparation####
rm(list=ls())

# #Start logfile
# sink(file="mylogfile.txt")
# #Check that working directory is properly set
wd <- getwd() 
wd 

# #Start the clock!:
start_all <- Sys.time()

# ####Initial settings####
# #BayesTraits version to use
BT_version <- switch(1, "BayesTraitsV2.0.2", "BayesTraitsV3") #BayesTraitsV2.0.2 (1) OR BayesTraitsV3.0.1 (2)
# #Should we run local transformations (kappa, lambda, delta) as well (only possible with >= BayesTraits V3)
if(BT_version == "BayesTraitsV3"){
  BT_localTrans <- switch(2, "YES", "NO")# #Run also local transformations (1) or not (2)
} else{
  BT_localTrans <- "NO" # #If BT_version is not V3, BT_localTrans is set to "NO" (necessary for some conditionals we have
  # #in the code)
}
# #Should we run complete tree transformations (kappa, lambda, delta) as well (only possible with >= BayesTraits V3)
# #We will only run whole tree transformations if no local transformations are used (local transformations should also recover
# #whole tree transformations, if they fit the data + we want to avoid overparameterizing our model)
if(BT_localTrans == "NO"){
  BT_globalTrans <- switch(2, "YES", "NO")# #Run also whole tree transformations (1) or not (2)
} else{
  BT_globalTrans <- "NO" # #If we use local transformations we will not use whole tree transformations
}

# #BayesTraits postprocessing done locally or remote
PP_mode <- switch(1, "local", "remote") #local (1) OR remote (2)

# #Should we exclude trees that have not converged?
exclude_trees <- switch(2, "YES", "NO") # #Exclude (1) or keep (2) trees (and data) for BayesTraits analyses, which have not converged

# #Note, that the current implementation of exclude_trees DOES NOT ACCOUNT FOR local or whole tree transformations (kappa, lambda, delta)
# #and it currently does not obtain logBFs properly when using exclude_trees! Usage of exclude_trees should therefore only be for 
# #testing purposes (which it has been anyways)!!!
# #Which measurements are we using? Body size related ones or PCA scores?
mymeasure <- switch(1, "BodySize", "PCA_scores")

# #What type of PCA data are we dealing with? Is it shape data or functional measurements?
PCA_type <- switch(2, "shape", "functional")

# #How many PCA scores (or other measurements) do we want to incorporate in our analysis?
PCA_No <- 1 # #Usually, we want 1 (also for univariate analyses), 5, 10, or 22

if(mymeasure == "BodySize"){
  PCA_No <- 1 # #If we are dealing with body size measurements, we want to run univariate analyses
}
# #Should we account for within species variation of body size (only possible with >= BayesTraits V3)
if(mymeasure == "BodySize"){
  if(BT_version == "BayesTraitsV3"){
    measurement.error <- switch(1, "YES", "NO")# #Account for within species variation of body size (1) or not (2)
    CV <- 5 # #CV...coefficient of variation in % (see Yablokov, 1974, p. 72-75; Hunt & Carrano, 2010, p. 256; Benson et al., 2018, p. 17)
  }
}
# #Skip or run some developmental code (mainly used to try certain things)
skip_code <- switch(1, "YES", "NO") # #Skip (1) or run (2) the code

#Which clade should we focus on
myclade = switch(7, "Tetrapoda", 
                 "Parareptilia", 
                 "Archosauromorpha", 
                 "Synapsida", 
                 "Lepidosauria",
                 "Crocodylomorpha",
                 "Sauropterygia")

#Switch between different clades

iterations = 20 #several analyses have to be done more than once,
#as they are stochastically pulled

# create a progress bar
# we would like to have a progress bar for all the repeated analyses which use "iterations"
pb <- txtProgressBar(min = 0, max = iterations, style = 3)

# #Set some BayesTraits settings
bayes_iterations <- 2000000000 # 
bayes_sample <- 80000 # 
bayes_burnin <- 400000000 # 

bayes_burnin/bayes_iterations
bayes_iterations-bayes_burnin
(bayes_iterations-bayes_burnin)/bayes_sample

bayes_iterations
bayes_burnin

bayes_stones <- 1000 #
bayes_stones_iterations <- 100000 # 
bayes_scaletrees <- 0.0001

# #Set some BayesTraits postprocessing settings
NoCats <- 1000 # #Set no. of time slices to be used for phylogenetically corrected evol. rates
# #Note, that the number of time slices can influence kernel density plots, which are based on
# #phyl. cor. evol. rates: if the number of slices is (too) small, longer branches could be "over-
# #represented" - a single slice should at max. be as long as the shortest branch (preferentially it
# #should be even shorter)
# #If you use the slices just to plot evol. rates through time, the number of slices should be less of
# #a concern

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
plot.phylo(Supertree.tree, show.node.label = T, cex=0.4)
write.tree(Supertree.tree, file="Supertree.tre") #Converts tree to Newick format

#Import the .txt file used to time-scale the phylogeny
Supertree.ages <- read.table("Sauropterygians_ages.txt", header=T, row.names = 1)
Supertree.ages

Supertree.tree <- read.tree("Supertree.tre") #Read previously created Newick tree
#This is needed, as NEXUS files sometimes cause problems in R
Supertree.tree

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

#Import/Extract myclade tree
myclade.tree <- Supertree.tree
myclade.ages <- Supertree.ages

####Preparation for time-scaling the tree#####
name.check(myclade.tree,Supertree.ages) #Check if all species names are present in both
#the file used to time-scale the tree and the tree itself

## load FBD trees
load(file="cal3trees.RData")
myclade.ts.trees

for(i in 1:length(myclade.ts.trees)){
  print(myclade.ts.trees[[i]]$root.time)
}

# #Save my workspace after the Hedman timescalung
save.image(file = paste(myclade,".Hedmanonly.Workspace.RData", sep=""))
#load(file = paste(myclade,".Hedmanonly.Workspace.RData", sep="")) #If you want to save time and only load the calculated results

###########################################################
######BayesTraits analysis##########

tmp_pca_scores <- read.table("Sauropterygians_Neck.txt", row.names = 1)

#######Use all our timescaled trees##################
droptips_trees <- name.check(myclade.ts.trees[[1]],tmp_pca_scores) #Find taxa to prune
#from our trees, for which we have PCA data. We just check tree 1 of our tree
#sample because all trees which have been timescaled and randomly resolved should have
#the same taxa (they just differ in their topology)
droptips_trees
#Now drop the tips from ALL our timescaled and randomly resolved trees
#myclade.ts.trees.pruned <- lapply(myclade.ts.trees, dropPaleoTip, droptips_trees$tree_not_data)

myclade.ts.trees.pruned <- myclade.ts.trees

#plot(ladderize(myclade.ts.trees.pruned[[1]]), cex=0.3) #Plot one of the pruned trees
#name.check(myclade.ts.trees.pruned[[1]], tmp_pca_scores) #Confirm that tree and data are now in agreement

#myclade.ts.trees.pruned[[1]]

pca_scores <- tmp_pca_scores
rm(tmp_pca_scores)

myclade.ts.trees[[1]]$root.time
myclade.ts.trees.pruned[[1]]$root.time

plot(ladderize(myclade.ts.trees.pruned[[1]]), cex=0.4)

#Save my workspace before the BayesTraits preparation
save.image(file = paste(myclade,".beforeBayesPrep.Workspace.RData", sep=""))
#load(file = paste(myclade,".beforeBayesPrep.Workspace.RData", sep="")) #If you want to save time and only load the calculated results

#####BayesTraits#####
#BayesTraits does NOT allow node labels on trees
#Therefore we have to delete them, before importing the trees into BayesTraits
BayesTraits.trees <- myclade.ts.trees.pruned[1:20]

for(i in 1:length(BayesTraits.trees)) {
      BayesTraits.trees[[i]]$edge.length[BayesTraits.trees[[i]]$edge.length == 0] <- 0.001
}

for(i in 1:length(BayesTraits.trees)){
  BayesTraits.trees[[i]]$node.label=NULL #Delete all the node labels
}

BayesTraits.trees

pick <- sample(1:20, 1)
pick
pick2 <- sample(1:20, 1)
pick2

geoscalePhylo(ladderize(BayesTraits.trees[[pick]]), cex.tip = 0.3)
geoscalePhylo(ladderize(BayesTraits.trees[[pick2]]), cex.tip = 0.3)

#Write tree(s)
for(i in 1:length(BayesTraits.trees)){
  write.nexus(BayesTraits.trees[[i]], file=paste("./BayesTraits/",myclade,".pca.scores.pruned.singletree.",i,".nex", sep=""), translate = TRUE) #Writes all trees as single trees
}
# #Write trees as an R object, keeping all R attributes (BIG thx to Mark!)
# dput(BayesTraits.trees[[1]], file = "phylomorpho_Tree")
# phylomorpho_Tree <- dget("phylomorpho_Tree")
# #We can also include the node labels
# dput(myclade.ts.trees.pruned[[1]], file = "phylomorpho_withlabel_Tree")
# phylomorpho_withlabel_Tree <- dget("phylomorpho_withlabel_Tree")

#Write data
#BayesTraits V2 names the rescaled trees according to the name of the input data file
#The SaveTrees command does not work, therefore it is not possible to modify the name
#of the saved trees from within BayesTraits
#A workaround is to produce a separate data file for every analysis - the different data files
#will contain the same data and be identical except for their name
#This workaround will no longer be necessary for BayesTraits V3, as the names of the rescaled trees
#are based on the specified name of the log files
#For BayesTraits V3 a single data file will be fine
#Single data file
#write.table(bodySize.pruned.Stabletraits, file=paste("./BayesTraits/",myclade_PalAss,".",length,".pruned.txt", sep=""), col.names = FALSE, quote=FALSE) #Write data to bodysize.pruned file
#Multiple data files (necessary for BayesTraits V2)
for(i in 1:length(BayesTraits.trees)){
  write.table(pca_scores, file=paste("./BayesTraits/",myclade,".pca.scores.pruned.",i,".txt", sep=""), col.names = FALSE, quote=FALSE) #Write data to PCA scores file
}

if(exists("measurement.error") == "TRUE"){ # #We have to check whether the object "measurement.error" exists
  if(measurement.error == "YES"){
    # #Store pca_scores values in dataframe
    SampleData <- as.data.frame(pca_scores) # #Vectors/matrices in R can only store one type of data 
    # #(only strings or only numeric). Therefore we cannot just use SampleData <- cbind("Unlinked", pca_scores)
    # #since the numerical values will be treated as characters/factors
    colnames(SampleData)[colnames(SampleData) == "V1"] <- "Value" # #Replace column name "V1" with "Value"
    SampleData$Species <- rownames(SampleData) # #Generate a species column
    SampleData$DataSource <- "Unlinked" # #Add column with "Unlinked" parameter, indicating that recorded
    # #traits have not been collected from the same specimen (see BayesTraits manual). Since we use SampleData
    # #only for the univariate approach of body size data, it should actually not matter, whether we use "Linked"
    # #or "Unlinked" since there is only a single trait
    SampleData <- SampleData[,c("Species", "DataSource","Value")] # #Reorder columns
    
    # #Calculate data including within species variation/measurement error (see Silvestro et al., 2015; Cooper
    # #et al., 2016; Benson et al., 2018)
    # #The coefficient of variation for linear measurements in mammals (vertebrates according to Benson et al., 2018) 
    # #is about 5% (see Yablokov, 1974, p. 72-75; Hunt & Carrano, 2010, p. 256; Benson et al., 2018, p. 17)
    # #Benson et al. (2018) applied this value also to dinosaurs, but note, that this can - at best - only be
    # #considered a rough approximation
    # #A coefficient of variation 5% translates into a standard deviation of 0.0217 on the log10 scale (Benson et
    # #al., 2018) - see below, how we get the standard deviation from the coeffficient of variation (see; Canchola
    # #et al., 2017, p. 2); see also Limpert et al. (2001, p. 345); Julious & Debarnot (2000, p. 69))
    # #CV = 100*sqrt(10^(ln(10)*sigma.SampleData^2)-1) # #CV...coefficient of variation in %
    # #Calculate sigma from CV:
    # #CV <- 5 # #CV...coefficient of variation in %
    sigma.SampleData <- sqrt(log10((CV/100)^2+1)/log(10)) # #Standard deviation based on CV = 5 and log10 scale data
    var.SampleData <- sigma.SampleData^2 # #Variance based on CV = 5 and log10 scale data
    sigma.SampleData
    var.SampleData
    
    # #Estimate 99 additional measurements based on the coefficient of variation and an assumed normal distribution
    # #of the trait values
    me.no <- 99 # #Number of additional estimated trait values used to account for within species variation
    me.values <- t(mapply(rnorm, n = me.no, mean = SampleData$Value, sd = sigma.SampleData))
    SampleData <- cbind(SampleData, me.values) # #Add sampled values
    
    # #Remove previously generated SampleData.txt file(s) - since we use append in the generating funtion, this initial
    # #step is necessary (otherwise newly generated SampleData will just be appended to older one
    SampleData.to.delete <- list.files(path = "./BayesTraits/", glob2rx(paste("SampleData.txt", sep="")),full.names=T)
    file.remove(SampleData.to.delete) # #Remove SampleData.txt file(s)
    
    # #Generate SampleData.txt file
    # #See also https://stackoverflow.com/questions/24349527/in-r-save-text-file-with-different-separators-for-each-column?rq=1
    seps <- c(rep(" ", 2), rep(",", me.no), "\n") # #Different separators are necessary for SampeData.txt file (see Manual of BayesTraits)
    apply(SampleData, 1, function(x, seps) 
      cat(x, file = "./BayesTraits/SampleData.txt", sep = seps, append = TRUE), seps = seps)
  }
}


##############
#Generate BayesTraits script
#Heterogeneous rates with local transformations (in this case kappa)
# BayesTraits_hetRates_Script.cmd <- paste("7
#           2
#           VarRates
#           iterations 1010000
#           sample 10000
#           burnin 10000
#           stones 100 10000
#           Logfile ", "Archosauromorpha.pca.scores.pruned.txt.",i,".log.txt\n", sep="",
#           "Info
#           Run") # This command does NOT work: SaveTrees ", myclade_PalAss,".",length,".pruned.txt.",i,".PP.trees\n",

# #Change the way numbers are stored
scipen_default <- getOption("scipen") #get default setting (should be 0)
options(scipen = 999) #Disable scientific notation in R

BayesTraits_hetRates_kappa_Script.cmd <- list()
# #The command ScaleTrees only works with BayesTraits V3
# #(but we don't need a separate command file for BayesTraits V2.0.2 as it just ignores the command)
# #ScaleTrees rescales the original branch lengths to have a mean as given in the command - this prevents the rates
# #becoming small, hard to estimate or search for (see Manual for BayesTraits V3) --> but this should NOT be used for continous data (A. Meade, pers. comm., 2018)
# #Setting TestCorrel to "0" forces the multiple variables not to be correlated (which is correct for PC axes - see Cooney et al. (2017),
# #but see Felice & Goswami (2018) and Adams & Collyer (2018))
for(i in 1:length(BayesTraits.trees)){
  BayesTraits_hetRates_kappa_Script.cmd[[i]] <- paste("7\n",
                                           "2\n",
                                           #"DistData SampleData.txt\n", # #To incorporate within species variation
                                           "VarRates\n",
                                           "iterations ", bayes_iterations, "\n",
                                           "sample ", bayes_sample, "\n",
                                           "burnin ", bayes_burnin, "\n",
                                           "RJLocalTransform kappa\n",
                                           "stones ", bayes_stones, " ", bayes_stones_iterations, "\n",
                                           #"ScaleTrees ", bayes_scaletrees, "\n", # #Do NOT use the ScaleTrees parameter with continuous data
                                           #"TestCorrel 0\n",
                                           "Logfile ", myclade,".pca.scores.pruned.txt.heterogeneous.kappa.",i,".log.txt\n", sep="",
                                           "Info\n",
                                           "Run") # This command does NOT work: SaveTrees ", myclade_PalAss,".",length,".pruned.txt.",i,".PP.trees\n",
}


# #TestCorrel 0: ONLY NECESSARY FOR PCA ANALYSES, BUT NOT FOR BODY SIZE!
# #TestCorrel: NO "0" FOR PC AXES AS THEY ARE EVOLUTIONARILY CORRELATED (SEE Felice & Goswami (2018) and Adams & Collyer (2018))

# #If we do not want to incorporate within species variation, we have to exclude the "DistData SampleData.txt\n"
# #parameter
if(exists("measurement.error") == "FALSE" || measurement.error == "NO"){
  # #If the object "measurement.error" does not exist OR if "measurement.error" is set to "NO", we do NOT
  # #incorporate within species variation in body size
  for(i in 1:length(BayesTraits.trees)){
    BayesTraits_hetRates_kappa_Script.cmd[[i]] <- gsub('DistData SampleData.txt\n','', BayesTraits_hetRates_kappa_Script.cmd[[i]]) #Remove "DistData SampleData.txt\n" (within species variation is ignored)
  }
}

# #Heterogeneous rates without or with (lambda, delta) local transformations
BayesTraits_hetRates_Script.cmd <- list() #Het. rates without any local transformations (i. e. without kappa, lambda, delta)
BayesTraits_hetRates_lambda_Script.cmd <- list() #Het. rates with lambda local transformation
BayesTraits_hetRates_delta_Script.cmd <- list() #Het. rates with delta local transformation
for(i in 1:length(BayesTraits.trees)){
  BayesTraits_hetRates_Script.cmd[[i]] <- gsub('RJLocalTransform kappa\n','', BayesTraits_hetRates_kappa_Script.cmd[[i]]) #Het. rates without any local transformations (i. e. without kappa, lambda, delta)
  BayesTraits_hetRates_Script.cmd[[i]] <- gsub(paste("Logfile ", myclade,".pca.scores.pruned.txt.heterogeneous.kappa.",i,".log.txt\n", sep=""),
                                               paste("Logfile ", myclade,".pca.scores.pruned.txt.",i,".log.txt\n", sep=""), BayesTraits_hetRates_Script.cmd[[i]])
  
  BayesTraits_hetRates_lambda_Script.cmd[[i]] <- gsub('kappa','lambda', BayesTraits_hetRates_kappa_Script.cmd[[i]]) #Het. rates with lambda local transformation
  BayesTraits_hetRates_delta_Script.cmd[[i]] <- gsub('kappa','delta', BayesTraits_hetRates_kappa_Script.cmd[[i]]) #Het. rates with delta local transformation
}

# #Homogeneous rates without or with (lambda, delta) local transformations
BayesTraits_homRates_Script.cmd <- list() #Hom. rates without any local transformations (i. e. without kappa, lambda, delta)
BayesTraits_homRates_kappa_Script.cmd <- list() #Hom. rates with kappa local transformation
BayesTraits_homRates_lambda_Script.cmd <- list() #Hom. rates with lambda local transformation
BayesTraits_homRates_delta_Script.cmd <- list() #Hom. rates with delta local transformation

for(i in 1:length(BayesTraits.trees)){
  BayesTraits_homRates_Script.cmd[[i]] <- gsub('VarRates','', BayesTraits_hetRates_kappa_Script.cmd[[i]])
  BayesTraits_homRates_Script.cmd[[i]] <- gsub('RJLocalTransform kappa\n','', BayesTraits_homRates_Script.cmd[[i]])
  BayesTraits_homRates_Script.cmd[[i]] <- gsub(paste("Logfile ", myclade,".pca.scores.pruned.txt.heterogeneous.kappa.",i,".log.txt\n", sep=""),
                                               paste("Logfile ", myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt\n", sep=""), BayesTraits_homRates_Script.cmd[[i]])
  
  BayesTraits_homRates_kappa_Script.cmd[[i]] <- gsub('VarRates','', BayesTraits_hetRates_kappa_Script.cmd[[i]])
  BayesTraits_homRates_kappa_Script.cmd[[i]] <- gsub('heterogeneous','homogeneous', BayesTraits_homRates_kappa_Script.cmd[[i]])

  BayesTraits_homRates_lambda_Script.cmd[[i]] <- gsub('VarRates','', BayesTraits_hetRates_kappa_Script.cmd[[i]])
  BayesTraits_homRates_lambda_Script.cmd[[i]] <- gsub('kappa','lambda', BayesTraits_homRates_lambda_Script.cmd[[i]])
  BayesTraits_homRates_lambda_Script.cmd[[i]] <- gsub('heterogeneous','homogeneous', BayesTraits_homRates_lambda_Script.cmd[[i]]) 
  
  BayesTraits_homRates_delta_Script.cmd[[i]] <- gsub('VarRates','', BayesTraits_hetRates_kappa_Script.cmd[[i]])
  BayesTraits_homRates_delta_Script.cmd[[i]] <- gsub('kappa','delta', BayesTraits_homRates_delta_Script.cmd[[i]])
  BayesTraits_homRates_delta_Script.cmd[[i]] <- gsub('heterogeneous','homogeneous', BayesTraits_homRates_delta_Script.cmd[[i]]) 
}

# #Not needed anymore
# for(i in 1:length(BayesTraits.trees)){
#   #SaveTrees command does NOT work!
#   #BayesTraits_homRates_Script.cmd <- gsub(paste("SaveTrees ", myclade_PalAss,".",length,".pruned.txt.",i,".PP.trees\n", sep=""),'', BayesTraits_homRates_Script.cmd)
#   BayesTraits_homRates_Script.cmd[[i]] <- gsub(paste("Logfile ", myclade,".pca.scores.pruned.txt.",i,".log.txt\n", sep=""),
#                                         paste("Logfile ", myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt\n", sep=""), BayesTraits_homRates_Script.cmd[[i]])
# 
# }


if(BT_globalTrans == "YES"){
  # #Whole tree transformations (kappa, lambda, delta)
  # #We can just modify the code for local transformations to whole tree transformations
  for(i in 1:length(BayesTraits.trees)){
    # #Kappa
    BayesTraits_homRates_kappa_Script.cmd[[i]] <- gsub('RJLocalTransform kappa','Kappa', BayesTraits_homRates_kappa_Script.cmd[[i]]) # #Hom. rates
    BayesTraits_hetRates_kappa_Script.cmd[[i]] <- gsub('RJLocalTransform kappa','Kappa', BayesTraits_hetRates_kappa_Script.cmd[[i]]) # #Het. rates
    
    # #Lambda
    BayesTraits_homRates_lambda_Script.cmd[[i]] <- gsub('RJLocalTransform lambda','Lambda', BayesTraits_homRates_lambda_Script.cmd[[i]]) # #Hom. rates
    BayesTraits_hetRates_lambda_Script.cmd[[i]] <- gsub('RJLocalTransform lambda','Lambda', BayesTraits_hetRates_lambda_Script.cmd[[i]]) # #Het. rates
    
    # #Delta
    BayesTraits_homRates_delta_Script.cmd[[i]] <- gsub('RJLocalTransform delta','Delta', BayesTraits_homRates_delta_Script.cmd[[i]]) # #Hom. rates
    BayesTraits_hetRates_delta_Script.cmd[[i]] <- gsub('RJLocalTransform delta','Delta', BayesTraits_hetRates_delta_Script.cmd[[i]]) # #Het. rates
  }
}




for(i in 1:length(BayesTraits.trees)){
  # #No local transformations: heterogeneous and homogeneous rates
  write(BayesTraits_hetRates_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_hetRates_Script.",i,".cmd", sep=""))
  write(BayesTraits_homRates_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_homRates_Script.",i,".cmd", sep=""))
  
  
  # #With local or whole tree transformations (kappa, lambda, delta): heterogeneous and homogeneous rates
  # #Depending on BT_localTrans and BT_globalTrans setting either local or whole tree transformations are used
  # #Kappa
  write(BayesTraits_hetRates_kappa_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_hetRates_kappa_Script.",i,".cmd", sep=""))
  write(BayesTraits_homRates_kappa_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_homRates_kappa_Script.",i,".cmd", sep=""))
  
  # #Lambda
  write(BayesTraits_hetRates_lambda_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_hetRates_lambda_Script.",i,".cmd", sep=""))
  write(BayesTraits_homRates_lambda_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_homRates_lambda_Script.",i,".cmd", sep=""))
  
  # #Delta
  write(BayesTraits_hetRates_delta_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_hetRates_delta_Script.",i,".cmd", sep=""))
  write(BayesTraits_homRates_delta_Script.cmd[[i]], file=paste("./BayesTraits/BayesTraits_homRates_delta_Script.",i,".cmd", sep=""))
}

# #Change back to default settings for numbers
options(scipen = scipen_default) #Reenable default scientific notation in R

#Save my workspace before the BayesTraits runs
save.image(file = paste(myclade,".beforeBayesonly.Workspace.RData", sep=""))
#load(file = paste(myclade,".beforeBayesonly.Workspace.RData", sep="")) #If you want to save time and only load the calculated results

# #Register parallel back end as number of cores available - 2:
#registerDoParallel(cores = no_cores)
if(myOS == "Windows"){
  foreach(i = 1:length(BayesTraits.trees)) %dopar% {
    # #No local transformations: heterogeneous and homogeneous rates
    shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_Script.',i,'.cmd'))
    shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_Script.',i,'.cmd'))
    
    if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
      # #With local or whole tree transformations (kappa, lambda, delta): heterogeneous and homogeneous rates
      # #Kappa
      shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_kappa_Script.',i,'.cmd'))
      shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_kappa_Script.',i,'.cmd'))
      
      # #Lambda
      shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_lambda_Script.',i,'.cmd'))
      shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_lambda_Script.',i,'.cmd'))
     
      # #Delta
      shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_delta_Script.',i,'.cmd'))
      shell(paste('cd ',wd,'/BayesTraits/ & ',BT_version,'.exe ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_delta_Script.',i,'.cmd'))
    }
  }
} else{
  foreach(i = 1:length(BayesTraits.trees)) %dopar% {
    #If it doesn't work, you might need to set BayesTraits as executable, using "chmod +x BayesTraitsV2.0.2"
    #The additional brackets () and && are necessary to created an enclosed environment (subshell) where the working directory is set to the BayesTraits folder
    #If you want to run BayesTraits directly from BlueCrystal (without uploading the binary), just replace "./BayesTraitsV2.0.2" with "BayesTraitsV2" (depending on the version you want to use)
    #The following two lines are only necessary, if you haven't added BayesTraits to your BlueCrystal installation (i.e., if no alias has been created)
    #system(paste('(cd ./BayesTraits && ./',BT_version,' ' ,"Archosauromorpha.pca.scores.pruned.singletree.",i,".nex ", "Archosauromorpha.pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_Script.',i,'.cmd)'))
    #system(paste('(cd ./BayesTraits && ./',BT_version,' ' ,"Archosauromorpha.pca.scores.pruned.singletree.",i,".nex ", "Archosauromorpha.pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_Script.',i,'.cmd)'))
    #If BayesTraits has been installed on BlueCrystal, you can call it directly:
    # #No local transformations: heterogeneous and homogeneous rates
    system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_Script.',i,'.cmd)'))
    system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_Script.',i,'.cmd)'))
    #if we run the code on multiple nodes, we have to specify our working directory "wd"!
    
    # #Not needed anymore
    #system(paste('(cd ./BayesTraits && ',BT_version,' ' ,"Archosauromorpha.pca.scores.pruned.singletree.",i,".nex ", "Archosauromorpha.pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_Script.',i,'.cmd)'))
    #system(paste('(cd ./BayesTraits && ',BT_version,' ' ,"Archosauromorpha.pca.scores.pruned.singletree.",i,".nex ", "Archosauromorpha.pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_Script.',i,'.cmd)'))
    
    if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
      # #With local or whole tree transformations (kappa, lambda, delta): heterogeneous and homogeneous rates
      # #Kappa
      system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_kappa_Script.',i,'.cmd)'))
      system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_kappa_Script.',i,'.cmd)'))
      
      # #Lambda
      system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_lambda_Script.',i,'.cmd)'))
      system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_lambda_Script.',i,'.cmd)'))
      
      # #Delta
      system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_hetRates_delta_Script.',i,'.cmd)'))
      system(paste('(cd ',wd,'/BayesTraits && ',BT_version,' ' ,myclade,".pca.scores.pruned.singletree.",i,".nex ", myclade,".pca.scores.pruned.",i,".txt", sep="",' < BayesTraits_homRates_delta_Script.',i,'.cmd)'))
    }
  }
}

# #Check time
end_bayes <- Sys.time()
# #How long did it take to run up to this point?
end_bayes - start_all

#Save my workspace after the BayesTraits runs
save.image(file = paste(myclade,".Bayesonly.Workspace.RData", sep=""))
#load(file = paste(myclade,".Bayesonly.Workspace.RData", sep="")) #If you want to save time and only load the calculated results

# ####Check convergence####
# #Modified from Randi H. Griffin's BayesTraits wrapper: https://github.com/rgriff23/btw/blob/master/R/Discrete.R
# #We import the results needed to assess convergence (and skip those bits, that we don't need)
# #No local transformations
skip_homRates_convergence <- list()
bayes_homRates_convergence <- list()
skip_varRates_convergence <- list()
bayes_varRates_convergence <- list()

# #Local or whole tree transformations
# #Kappa
bayes_homRates_kappa_convergence <- list()
bayes_varRates_kappa_convergence <- list()

# #Lambda
bayes_homRates_lambda_convergence <- list()
bayes_varRates_lambda_convergence <- list()

# #Delta
bayes_homRates_delta_convergence <- list()
bayes_varRates_delta_convergence <- list()

if(BT_version == "BayesTraitsV2.0.2"){
  for(i in 1:length(BayesTraits.trees)){
    # #Homogeneous rates
    skip_homRates_convergence[[i]] = grep("Tree No", scan(file = paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt", sep=""),
                                                          what="c", quiet=T, sep="\n", blank.lines.skip=FALSE)) - 1
    bayes_homRates_convergence[[i]] = read.table(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt", sep=""), 
                                                 skip = skip_homRates_convergence[[i]], sep = "\t",  quote="\"", header = TRUE)
    bayes_homRates_convergence[[i]] = bayes_homRates_convergence[[i]][,-ncol(bayes_homRates_convergence[[i]])] 
    
    # #Variable Rates
    skip_varRates_convergence[[i]] = grep("Tree No", scan(file = paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",i,".log.txt", sep=""),
                                                          what="c", quiet=T, sep="\n", blank.lines.skip=FALSE)) - 1
    bayes_varRates_convergence[[i]] = read.table(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",i,".log.txt", sep=""), 
                                                 skip = skip_varRates_convergence[[i]], sep = "\t",  quote="\"", header = TRUE)
    bayes_varRates_convergence[[i]] = bayes_varRates_convergence[[i]][,-ncol(bayes_varRates_convergence[[i]])] 
  }
} else{#when using BayesTraitsV3
  for(i in 1:length(BayesTraits.trees)){
    # #No local transformations
    # #Homogeneous rates
    skip_homRates_convergence[[i]] = grep("Tree No", scan(file = paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt.Log.txt", sep=""),
                                                          what="c", quiet=T, sep="\n", blank.lines.skip=FALSE)) - 1
    bayes_homRates_convergence[[i]] = read.table(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt.Log.txt", sep=""), 
                                                 skip = skip_homRates_convergence[[i]], sep = "\t",  quote="\"", header = TRUE)
    bayes_homRates_convergence[[i]] = bayes_homRates_convergence[[i]][,-ncol(bayes_homRates_convergence[[i]])] 
    
    # #Variable Rates
    skip_varRates_convergence[[i]] = grep("Tree No", scan(file = paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",i,".log.txt.Log.txt", sep=""),
                                                          what="c", quiet=T, sep="\n", blank.lines.skip=FALSE)) - 1
    bayes_varRates_convergence[[i]] = read.table(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",i,".log.txt.Log.txt", sep=""), 
                                                 skip = skip_varRates_convergence[[i]], sep = "\t",  quote="\"", header = TRUE)
    bayes_varRates_convergence[[i]] = bayes_varRates_convergence[[i]][,-ncol(bayes_varRates_convergence[[i]])]
  }
}
# #Let's create a small function to ease the import for convergence diagnostics
# #The function takes 4 arguments:
# #1) myclade = our myclade parameter definied earlier in the script
# #2) tree.number = our loop parameter i (= currently selected tree in loop)
# #3) evol.rate = choose "homogeneous" or "heterogeneous"
# #4) evol.model = choose "kappa", "lambda", or "delta"
convergence_import <- function(myclade, tree.number, evol.rate, evol.model){
  skip_convergence = grep("Tree No", scan(file = paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",evol.rate,".",evol.model,".",tree.number,".log.txt.Log.txt", sep=""),
                                                              what="c", quiet=T, sep="\n", blank.lines.skip=FALSE)) - 1
  out = read.table(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",evol.rate,".",evol.model,".",i,".log.txt.Log.txt", sep=""), 
                                                   skip = skip_convergence, sep = "\t",  quote="\"", header = TRUE)
  out = out[,-ncol(out)]
  return(out)
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Kappa
    bayes_homRates_kappa_convergence[[i]] <- convergence_import(myclade = myclade, tree.number = i, evol.rate = "homogeneous", evol.model = "kappa")
    bayes_varRates_kappa_convergence[[i]] <- convergence_import(myclade = myclade, tree.number = i, evol.rate = "heterogeneous", evol.model = "kappa")
    
    # #Lambda
    bayes_homRates_lambda_convergence[[i]] <- convergence_import(myclade = myclade, tree.number = i, evol.rate = "homogeneous", evol.model = "lambda")
    bayes_varRates_lambda_convergence[[i]] <- convergence_import(myclade = myclade, tree.number = i, evol.rate = "heterogeneous", evol.model = "lambda")
    
    # #Delta
    bayes_homRates_delta_convergence[[i]] <- convergence_import(myclade = myclade, tree.number = i, evol.rate = "homogeneous", evol.model = "delta")
    bayes_varRates_delta_convergence[[i]] <- convergence_import(myclade = myclade, tree.number = i, evol.rate = "heterogeneous", evol.model = "delta")
  }
}



# #Run different convergence tests
# #Check densplot, traceplot and autocorr.plot for likelihood
# #No local transformations
for(i in 1:length(BayesTraits.trees)){
  # #Homogeneous rates
  densplot(mcmc(bayes_homRates_convergence[[i]]$Lh), main = paste("Homogeneous rates: Tree Nr. ",i, sep=""))
  traceplot(mcmc(bayes_homRates_convergence[[i]]$Lh), main = paste("Homogeneous rates: Tree Nr. ",i, sep=""))
  autocorr.plot(mcmc(bayes_homRates_convergence[[i]]$Lh), main = paste("Homogeneous rates: Tree Nr. ",i, sep=""))
}
for(i in 1:length(BayesTraits.trees)){
  # #Variable rates
  densplot(mcmc(bayes_varRates_convergence[[i]]$Lh), main = paste("Variable rates: Tree Nr. ",i, sep=""))
  traceplot(mcmc(bayes_varRates_convergence[[i]]$Lh), main = paste("Variable rates: Tree Nr. ",i, sep=""))
  autocorr.plot(mcmc(bayes_varRates_convergence[[i]]$Lh), main = paste("Variable rates: Tree Nr. ",i, sep=""))
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Local or whole tree transformations
    # #Kappa
    # #Homogeneous rates
    densplot(mcmc(bayes_homRates_kappa_convergence[[i]]$Lh), main = paste("Kappa - Homogeneous rates: Tree Nr. ",i, sep=""))
    traceplot(mcmc(bayes_homRates_kappa_convergence[[i]]$Lh), main = paste("Kappa - Homogeneous rates: Tree Nr. ",i, sep=""))
    autocorr.plot(mcmc(bayes_homRates_kappa_convergence[[i]]$Lh), main = paste("Kappa - Homogeneous rates: Tree Nr. ",i, sep=""))
    # #Variable rates
    densplot(mcmc(bayes_varRates_kappa_convergence[[i]]$Lh), main = paste("Kappa - Variable rates: Tree Nr. ",i, sep=""))
    traceplot(mcmc(bayes_varRates_kappa_convergence[[i]]$Lh), main = paste("Kappa - Variable rates: Tree Nr. ",i, sep=""))
    autocorr.plot(mcmc(bayes_varRates_kappa_convergence[[i]]$Lh), main = paste("Kappa - Variable rates: Tree Nr. ",i, sep=""))
    
    # #Lambda
    # #Homogeneous rates
    densplot(mcmc(bayes_homRates_lambda_convergence[[i]]$Lh), main = paste("Lambda - Homogeneous rates: Tree Nr. ",i, sep=""))
    traceplot(mcmc(bayes_homRates_lambda_convergence[[i]]$Lh), main = paste("Lambda - Homogeneous rates: Tree Nr. ",i, sep=""))
    autocorr.plot(mcmc(bayes_homRates_lambda_convergence[[i]]$Lh), main = paste("Lambda - Homogeneous rates: Tree Nr. ",i, sep=""))
    # #Variable rates
    densplot(mcmc(bayes_varRates_lambda_convergence[[i]]$Lh), main = paste("Lambda - Variable rates: Tree Nr. ",i, sep=""))
    traceplot(mcmc(bayes_varRates_lambda_convergence[[i]]$Lh), main = paste("Lambda - Variable rates: Tree Nr. ",i, sep=""))
    autocorr.plot(mcmc(bayes_varRates_lambda_convergence[[i]]$Lh), main = paste("Lambda - Variable rates: Tree Nr. ",i, sep=""))
    
    # #Delta
    # #Homogeneous rates
    densplot(mcmc(bayes_homRates_delta_convergence[[i]]$Lh), main = paste("Delta - Homogeneous rates: Tree Nr. ",i, sep=""))
    traceplot(mcmc(bayes_homRates_delta_convergence[[i]]$Lh), main = paste("Delta - Homogeneous rates: Tree Nr. ",i, sep=""))
    autocorr.plot(mcmc(bayes_homRates_delta_convergence[[i]]$Lh), main = paste("Delta - Homogeneous rates: Tree Nr. ",i, sep=""))
    # #Variable rates
    densplot(mcmc(bayes_varRates_delta_convergence[[i]]$Lh), main = paste("Delta - Variable rates: Tree Nr. ",i, sep=""))
    traceplot(mcmc(bayes_varRates_delta_convergence[[i]]$Lh), main = paste("Delta - Variable rates: Tree Nr. ",i, sep=""))
    autocorr.plot(mcmc(bayes_varRates_delta_convergence[[i]]$Lh), main = paste("Delta - Variable rates: Tree Nr. ",i, sep=""))
  }
}


# #Import the BayesTrait results in coda format
# #We exclude the columns "Iteration", "Harmonic Mean" and "Tree No" as they are not relevant for
# #the convergence analyses (e.g., some of them have a fixed value)
res_hom <- list() # #No local transformations: hom. rates
res <- list() # #No local transformations: het. rates
res_hom_kappa <- list() # #Kappa: hom. rates
res_het_kappa <- list() # #Kappa: het. rates
res_hom_lambda <- list() # #Lambda: hom. rates
res_het_lambda <- list() # #Lambda: het. rates
res_hom_delta <- list() # #Delta: hom. rates
res_het_delta <- list() # #Delta: het. rates


# #No local transformations
if(BT_version == "BayesTraitsV2.0.2"){
  for(i in 1:length(BayesTraits.trees)){
    # #Homogeneous rates
    res_hom[[i]] <- mcmc(subset(bayes_homRates_convergence[[i]], select=-c(Iteration, Harmonic.Mean, Tree.No)),
                         start=min(bayes_homRates_convergence[[i]]$Iteration),
                         end=max(bayes_homRates_convergence[[i]]$Iteration),thin=bayes_sample)
    
    # #Variable rates
    res[[i]] <- mcmc(subset(bayes_varRates_convergence[[i]], select=-c(Iteration, Harmonic.Mean, Tree.No)),
                     start=min(bayes_varRates_convergence[[i]]$Iteration),
                     end=max(bayes_varRates_convergence[[i]]$Iteration),thin=bayes_sample)
  }
}else{#when using BayesTraitsV3
  # #"Harmonic Mean" is NOT excluded, as BayesTraitsV3 does NOT produce "Harmonic Mean" as output
  for(i in 1:length(BayesTraits.trees)){
    # #Homogeneous rates
    res_hom[[i]] <- mcmc(subset(bayes_homRates_convergence[[i]], select=-c(Iteration, Tree.No)),
                         start=min(bayes_homRates_convergence[[i]]$Iteration),
                         end=max(bayes_homRates_convergence[[i]]$Iteration),thin=bayes_sample)
    
    # #Variable rates
    res[[i]] <- mcmc(subset(bayes_varRates_convergence[[i]], select=-c(Iteration, Tree.No)),
                     start=min(bayes_varRates_convergence[[i]]$Iteration),
                     end=max(bayes_varRates_convergence[[i]]$Iteration),thin=bayes_sample)
  }
}
# #Let's create again a small function to ease the coda import
# #It has just one parameter, which indicates which convergence dataframe should be used
convergence_mcmc <- function(convergence.df){
  mcmc(subset(convergence.df, select=-c(Iteration, Tree.No)), 
       start=min(convergence.df$Iteration), 
       end=max(convergence.df$Iteration),thin=bayes_sample)
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Local or whole tree transformations
    # #Kappa
    res_hom_kappa[[i]] <- convergence_mcmc(bayes_homRates_kappa_convergence[[i]]) # #Hom. rates
    res_het_kappa[[i]] <- convergence_mcmc(bayes_varRates_kappa_convergence[[i]]) # #Het. rates
    
    # #Lambda
    res_hom_lambda[[i]] <- convergence_mcmc(bayes_homRates_lambda_convergence[[i]]) # #Hom. rates
    res_het_lambda[[i]] <- convergence_mcmc(bayes_varRates_lambda_convergence[[i]]) # #Het. rates
    
    # #Delta
    res_hom_delta[[i]] <- convergence_mcmc(bayes_homRates_delta_convergence[[i]]) # #Hom. rates
    res_het_delta[[i]] <- convergence_mcmc(bayes_varRates_delta_convergence[[i]]) # #Het. rates
  }
}

# #Trace plots
# #Look at the trace plots for LH and the PC axes (alpha parameter)
# #No local transformations
for(i in 1:length(BayesTraits.trees)){
  # #Homogeneous rates
  op <- par(mfrow=c(4,6))
  # #traceplot(res_hom[[i]][,c(1:24)], sub = paste("Hom. rates: Tree ",i, sep="")) #old: manually selecting columns 1:24
  traceplot(res_hom[[i]][,c(1:(1+2*PCA_No))], sub = paste("Hom. rates: Tree ",i, sep=""))
  par(op)
}
if(BT_version == "BayesTraitsV2.0.2"){
  for(i in 1:length(BayesTraits.trees)){
    # #Variable rates
    op <- par(mfrow=c(4,6))
    # #traceplot(res[[i]][,c(1:24)], sub = paste("Var. rates: Tree ",i, sep="")) #old: manually selecting columns 1:24
    traceplot(res[[i]][,c(1:(2+2*PCA_No))], sub = paste("Var. rates: Tree ",i, sep=""))
    par(op)
  }
}else{#when using BayesTraitsV3
  for(i in 1:length(BayesTraits.trees)){
    # #Variable rates
    op <- par(mfrow=c(4,6))
    # #traceplot(res[[i]][,c(1:24)], sub = paste("Var. rates: Tree ",i, sep="")) #old: manually selecting columns 1:24
    traceplot(res[[i]][,c(1:(3+2*PCA_No))], sub = paste("Var. rates: Tree ",i, sep="")) # #BayesTraits v.3 reports one additional parameter
    # #compared to BayesTraits v.2
    par(op)
  }
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Local or whole tree transformations
    # #Kappa
    # #Homogeneous rates
    op <- par(mfrow=c(4,6))
    traceplot(res_hom_kappa[[i]][,c(1:(2+2*PCA_No))], sub = paste("Kappa - Hom. rates: Tree ",i, sep=""))
    # #One additional parameter is reported compared to runs without local transformations
    par(op)
    # #Variable rates
    op <- par(mfrow=c(4,6))
    traceplot(res_het_kappa[[i]][,c(1:(4+2*PCA_No))], sub = paste("Kappa - Var. rates: Tree ",i, sep=""))
    # #One additional parameter is reported compared to runs without local transformations
    par(op)
    
    # #Lambda
    # #Homogeneous rates
    op <- par(mfrow=c(4,6))
    traceplot(res_hom_lambda[[i]][,c(1:(2+2*PCA_No))], sub = paste("Lambda - Hom. rates: Tree ",i, sep=""))
    # #One additional parameter is reported compared to runs without local transformations
    par(op)
    # #Variable rates
    op <- par(mfrow=c(4,6))
    traceplot(res_het_lambda[[i]][,c(1:(4+2*PCA_No))], sub = paste("Lambda - Var. rates: Tree ",i, sep=""))
    # #One additional parameter is reported compared to runs without local transformations
    par(op)
    
    # #Delta
    # #Homogeneous rates
    op <- par(mfrow=c(4,6))
    traceplot(res_hom_delta[[i]][,c(1:(2+2*PCA_No))], sub = paste("Delta - Hom. rates: Tree ",i, sep=""))
    # #One additional parameter is reported compared to runs without local transformations
    par(op)
    # #Variable rates
    op <- par(mfrow=c(4,6))
    traceplot(res_het_delta[[i]][,c(1:(4+2*PCA_No))], sub = paste("Delta - Var. rates: Tree ",i, sep=""))
    # #One additional parameter is reported compared to runs without local transformations
    par(op)
  }
}



# #Autocorrelation plots
# #No local transformations
for(i in 1:length(BayesTraits.trees)){
  # #acfplot(res_hom[[i]][,c(1:24)], main = paste("Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates (old): manually selecting columns 1:24
  acfplot(res_hom[[i]][,c(1:(1+2*PCA_No))], main = paste("Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
}
if(BT_version == "BayesTraitsV2.0.2"){
  for(i in 1:length(BayesTraits.trees)){
    # #acfplot(res[[i]][,c(1:24)], main = paste("Variable rates: Tree No. ",i, sep="")) # #Variable rates (old): manually selecting columns 1:24
    acfplot(res[[i]][,c(1:(2+2*PCA_No))], main = paste("Variable rates: Tree No. ",i, sep="")) # #Variable rates
  }
}else{#when using BayesTraitsV3
  for(i in 1:length(BayesTraits.trees)){
    # #acfplot(res[[i]][,c(1:24)], main = paste("Variable rates: Tree No. ",i, sep="")) # #Variable rates (old): manually selecting columns 1:24
    acfplot(res[[i]][,c(1:(3+2*PCA_No))], main = paste("Variable rates: Tree No. ",i, sep="")) # #Variable rates
    # #BayesTraits v.3 reports one additional parameter compared to BayesTraits v.2
  }
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Local or whole tree transformations
    # #Kappa
    acfplot(res_hom_kappa[[i]][,c(1:(2+2*PCA_No))], main = paste("Kappa - Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
    acfplot(res_het_kappa[[i]][,c(1:(4+2*PCA_No))], main = paste("Kappa - Variable rates: Tree No. ",i, sep="")) # #Variable rates
    
    # #Lambda
    acfplot(res_hom_lambda[[i]][,c(1:(2+2*PCA_No))], main = paste("Lambda - Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
    acfplot(res_het_lambda[[i]][,c(1:(4+2*PCA_No))], main = paste("Lambda - Variable rates: Tree No. ",i, sep="")) # #Variable rates
    
    # #Delta
    acfplot(res_hom_delta[[i]][,c(1:(2+2*PCA_No))], main = paste("Delta - Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
    acfplot(res_het_delta[[i]][,c(1:(4+2*PCA_No))], main = paste("Delta - Variable rates: Tree No. ",i, sep="")) # #Variable rates
  }
}


# #Get effective sizes (should be > 200)
ess_hom_list <- list() # #No local transformations: hom. rates
ess_list <- list() # #No local transformations: het. rates

# #Local or whole tree transformations
ess_hom_kappa_list <- list() # #Kappa: hom. rates
ess_het_kappa_list <- list() # #Kappa: het. rates
ess_hom_lambda_list <- list() # #Lambda: hom. rates
ess_het_lambda_list <- list() # #Lambda: het. rates
ess_hom_delta_list <- list() # #Delta: hom. rates
ess_het_delta_list <- list() # #Delta: het. rates

for(i in 1:length(BayesTraits.trees)){
  # #No local transformations: 
  ess_hom_list[[i]] <- effectiveSize(res_hom[[i]]) # #Homogeneous rates
  ess_list[[i]] <- effectiveSize(res[[i]]) # #Variable rates
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Local or whole tree transformations
    # #Kappa
    ess_hom_kappa_list[[i]] <- effectiveSize(res_hom_kappa[[i]]) # #Homogeneous rates
    ess_het_kappa_list[[i]] <- effectiveSize(res_het_kappa[[i]]) # #Variable rates
    
    # #Lambda
    ess_hom_lambda_list[[i]] <- effectiveSize(res_hom_lambda[[i]]) # #Homogeneous rates
    ess_het_lambda_list[[i]] <- effectiveSize(res_het_lambda[[i]]) # #Variable rates
    
    # #Delta
    ess_hom_delta_list[[i]] <- effectiveSize(res_hom_delta[[i]]) # #Homogeneous rates
    ess_het_delta_list[[i]] <- effectiveSize(res_het_delta[[i]]) # #Variable rates
  }
}
# #No local transformations:
ess_hom_list # #Homogeneous rates
ess_list # #Variable rates

# #Local or whole tree transformations
# #Kappa
ess_hom_kappa_list # #Homogeneous rates
ess_het_kappa_list # #Variable rates
# #Lambda
ess_hom_lambda_list # #Homogeneous rates
ess_het_lambda_list # #Variable rates
# #Delta
ess_hom_delta_list # #Homogeneous rates
ess_het_delta_list # #Variable rates


# #Find min. ESS (all should be >200)
# #No local transformations:
# #Homogeneous rates
tmp_ess_hom_min <- do.call(rbind, ess_hom_list)
ess_hom_min <- min(tmp_ess_hom_min)
ess_hom_min

# #Variable rates
tmp_ess_min <- do.call(rbind, ess_list)
#which(tmp_ess_min < 200, arr.ind = TRUE) # #Find rows with ess < 200
ess_min <- min(tmp_ess_min)
ess_min


# #Let's create a small function to ease the calculation of the min. ESS
convergence_ess_min <- function(ess_values){
  tmp_ess_minimum <- do.call(rbind, ess_values)
  ess_minimum <- min(tmp_ess_minimum)
  return(ess_minimum)
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Local or whole tree transformations:
  # #Kappa
  ess_hom_kappa_min <- convergence_ess_min(ess_hom_kappa_list) # #Hom. rates
  ess_het_kappa_min <- convergence_ess_min(ess_het_kappa_list) # #Het. rates
  ess_hom_kappa_min # #Hom. rates
  ess_het_kappa_min # #Het. rates
  
  # #Lambda
  ess_hom_lambda_min <- convergence_ess_min(ess_hom_lambda_list) # #Hom. rates
  ess_het_lambda_min <- convergence_ess_min(ess_het_lambda_list) # #Het. rates
  ess_hom_lambda_min # #Hom. rates
  ess_het_lambda_min # #Het. rates
  
  # #Delta
  ess_hom_delta_min <- convergence_ess_min(ess_hom_delta_list) # #Hom. rates
  ess_het_delta_min <- convergence_ess_min(ess_het_delta_list) # #Het. rates
  ess_hom_delta_min # #Hom. rates
  ess_het_delta_min # #Het. rates
}

if(BT_localTrans == "NO" && BT_globalTrans == "NO"){
  if(ess_hom_min>=200 & ess_min>=200){
    cat("Good, all homogeneous and var. rates ESS values are larger than 200.\n",
        "The smallest homogeneous ESS value is ", ess_hom_min, ".\n",
        "The smallest var. rates ESS value is ", ess_min, ".", sep="")
  } else{
    cat("Careful, some homogeneous or var. rates ESS values are NOT larger than 200! Chain has NOT converged!\n",
        "The smallest homogeneous ESS value is ", ess_hom_min, ".\n",
        "The smallest var. rates ESS value is ", ess_min, ".", sep="")
  }
}else if(BT_localTrans == "YES" || BT_globalTrans == "YES"){# #If we include local or whole tree transformations as well
  if(all(c(ess_hom_min, ess_min, ess_hom_kappa_min, ess_het_kappa_min,
     ess_hom_lambda_min, ess_het_lambda_min, ess_hom_delta_min, ess_het_delta_min)>=200)){
    cat("Good, all homogeneous and var. rates ESS values (incl. local or whole tree transformations) are larger than 200.\n",
        "The smallest homogeneous ESS value is ", ess_hom_min, ".\n",
        "The smallest var. rates ESS value is ", ess_min, ".\n",
        "The smallest homogeneous ESS value (Kappa) is ", ess_hom_kappa_min, ".\n",
        "The smallest var. rates ESS value (Kappa) is ", ess_het_kappa_min, ".\n",
        "The smallest homogeneous ESS value (Lambda) is ", ess_hom_lambda_min, ".\n",
        "The smallest var. rates ESS value (Lambda) is ", ess_het_lambda_min, ".\n",
        "The smallest homogeneous ESS value (Delta) is ", ess_hom_delta_min, ".\n",
        "The smallest var. rates ESS value (Delta) is ", ess_het_delta_min, ".", sep="")
  } else{
    cat("Careful, some homogeneous or var. rates ESS values (incl. local or whole tree transformations) are NOT larger than 200! Chain has NOT converged!\n",
        "The smallest homogeneous ESS value is ", ess_hom_min, ".\n",
        "The smallest var. rates ESS value is ", ess_min, ".\n",
        "The smallest homogeneous ESS value (Kappa) is ", ess_hom_kappa_min, ".\n",
        "The smallest var. rates ESS value (Kappa) is ", ess_het_kappa_min, ".\n",
        "The smallest homogeneous ESS value (Lambda) is ", ess_hom_lambda_min, ".\n",
        "The smallest var. rates ESS value (Lambda) is ", ess_het_lambda_min, ".\n",
        "The smallest homogeneous ESS value (Delta) is ", ess_hom_delta_min, ".\n",
        "The smallest var. rates ESS value (Delta) is ", ess_het_delta_min, ".", sep="")
  }
}



# #Gelman and Rubin's convergence diagnostic: Potential Scale Reduction Factor (PSRF)
# #If the values are above 1.05, you should run the chains longer
# #Works ONLY when multiple chains were run!
# #As we don't run the single trees multiple times, we can't use the PSRF

# #Density Plots
# #requires lattice library
density_hom_list <- list() # #No local transformations: Hom. rates
density_list <- list() # #No local transformations: Het. rates

# #Local or whole tree transformations:
density_hom_kappa_list <- list() # #Kappa: Hom. rates
density_het_kappa_list <- list() # #Kappa: Het. rates
density_hom_lambda_list <- list() # #Lambda: Hom. rates
density_het_lambda_list <- list() # #Lambda: Het. rates
density_hom_delta_list <- list() # #Delta: Hom. rates
density_het_delta_list <- list() # #Delta: Het. rates

# #No local transformations:
for(i in 1:length(BayesTraits.trees)){
  density_hom_list[[i]] <- densityplot(res_hom[[i]], main = paste("Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
  density_list[[i]] <- densityplot(res[[i]], main = paste("Variable rates: Tree No. ",i, sep="")) # #Variable rates
}
density_hom_list[[i]] # #Homogeneous rates (we plot just one tree)
density_list[[i]] # #Variable rates (we plot just one tree)

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Local or whole tree transformations:
  for(i in 1:length(BayesTraits.trees)){
    # #Kappa
    density_hom_kappa_list[[i]] <- densityplot(res_hom_kappa[[i]], main = paste("Kappa - Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
    density_het_kappa_list[[i]] <- densityplot(res_het_kappa[[i]], main = paste("Kappa - Variable rates: Tree No. ",i, sep="")) # #Variable rates
    
    # #Lambda
    density_hom_lambda_list[[i]] <- densityplot(res_hom_lambda[[i]], main = paste("Lambda - Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
    density_het_lambda_list[[i]] <- densityplot(res_het_lambda[[i]], main = paste("Lambda - Variable rates: Tree No. ",i, sep="")) # #Variable rates
    
    # #Delta
    density_hom_delta_list[[i]] <- densityplot(res_hom_delta[[i]], main = paste("Delta - Homogeneous rates: Tree No. ",i, sep="")) # #Homogeneous rates
    density_het_delta_list[[i]] <- densityplot(res_het_delta[[i]], main = paste("Delta - Variable rates: Tree No. ",i, sep="")) # #Variable rates
  }
}
if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Kappa
  density_hom_kappa_list[[i]] # #Homogeneous rates (we plot just one tree)
  density_het_kappa_list[[i]] # #Variable rates (we plot just one tree)
  
  # #Lambda
  density_hom_lambda_list[[i]] # #Homogeneous rates (we plot just one tree)
  density_het_lambda_list[[i]] # #Variable rates (we plot just one tree)
  
  # #Delta
  density_hom_delta_list[[i]] # #Homogeneous rates (we plot just one tree)
  density_het_delta_list[[i]] # #Variable rates (we plot just one tree)
}



# #Parameter summary
para_sum_hom <- list() # #No local transformations: Hom. rates
para_sum <- list() # #No local transformations: Het. rates

# #Local or whole tree transformations:
para_sum_hom_kappa <- list() # #Kappa: Hom. rates
para_sum_het_kappa <- list() # #Kappa: Het. rates
para_sum_hom_lambda <- list() # #Lambda: Hom. rates
para_sum_het_lambda <- list() # #Lambda: Het. rates
para_sum_hom_delta <- list() # #Delta: Hom. rates
para_sum_het_delta <- list() # #Delta: Het. rates

# #No local transformations:
for(i in 1:length(BayesTraits.trees)){
  para_sum_hom[[i]] <- summary(res_hom[[i]]) # #Homogeneous rates
  para_sum[[i]] <- summary(res[[i]]) # #Variable rates
}
para_sum_hom # #Homogeneous rates
para_sum # #Variable rates

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Local or whole tree transformations:
  for(i in 1:length(BayesTraits.trees)){
    # #Kappa
    para_sum_hom_kappa[[i]] <- summary(res_hom_kappa[[i]]) # #Homogeneous rates
    para_sum_het_kappa[[i]] <- summary(res_het_kappa[[i]]) # #Variable rates
    # #Lambda
    para_sum_hom_lambda[[i]] <- summary(res_hom_lambda[[i]]) # #Homogeneous rates
    para_sum_het_lambda[[i]] <- summary(res_het_lambda[[i]]) # #Variable rates
    # #Delta
    para_sum_hom_delta[[i]] <- summary(res_hom_delta[[i]]) # #Homogeneous rates
    para_sum_het_delta[[i]] <- summary(res_het_delta[[i]]) # #Variable rates
  }
}
# #Kappa
para_sum_hom_kappa # #Homogeneous rates
para_sum_het_kappa # #Variable rates
# #Lambda
para_sum_hom_lambda # #Homogeneous rates
para_sum_het_lambda # #Variable rates
# #Delta
para_sum_hom_delta # #Homogeneous rates
para_sum_het_delta # #Variable rates


# #Highest Posterior Density intervals
hpd_hom_list <- list() # #No local transformations: Hom. rates
hpd_list <- list() # #No local transformations: Het. rates

# #Local or whole tree transformations:
# #Kappa
hpd_hom_kappa_list <- list() # #Hom. rates
hpd_het_kappa_list <- list() # #Het. rates
# #Lambda
hpd_hom_lambda_list <- list() # #Hom. rates
hpd_het_lambda_list <- list() # #Het. rates
# #Delta
hpd_hom_delta_list <- list() # #Hom. rates
hpd_het_delta_list <- list() # #Het. rates

# #No local transformations:
for(i in 1:length(BayesTraits.trees)){
  hpd_hom_list[[i]] <- HPDinterval(res_hom[[i]]) # #Homogeneous rates
  hpd_list[[i]] <- HPDinterval(res[[i]]) # #Variable rates
}
hpd_hom_list # #Homogeneous rates
hpd_list # #Variable rates

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Local or whole tree transformations:
  for(i in 1:length(BayesTraits.trees)){
    # #Kappa
    hpd_hom_kappa_list[[i]] <- HPDinterval(res_hom_kappa[[i]]) # #Homogeneous rates
    hpd_het_kappa_list[[i]] <- HPDinterval(res_het_kappa[[i]]) # #Variable rates
    # #Lambda
    hpd_hom_lambda_list[[i]] <- HPDinterval(res_hom_lambda[[i]]) # #Homogeneous rates
    hpd_het_lambda_list[[i]] <- HPDinterval(res_het_lambda[[i]]) # #Variable rates
    # #Delta
    hpd_hom_delta_list[[i]] <- HPDinterval(res_hom_delta[[i]]) # #Homogeneous rates
    hpd_het_delta_list[[i]] <- HPDinterval(res_het_delta[[i]]) # #Variable rates
  }
}
# #Kappa
hpd_hom_kappa_list # #Homogeneous rates
hpd_het_kappa_list # #Variable rates

# #Lambda
hpd_hom_lambda_list # #Homogeneous rates
hpd_het_lambda_list # #Variable rates

# #Delta
hpd_hom_delta_list # #Homogeneous rates
hpd_het_delta_list # #Variable rates




# # ####"Multiple chains" (not really, but useful code in case we have multiple chains)######
# # #See also: https://github.com/simjoly/CourseComparativeMethods/blob/master/lecture4/BayesTraits.Rmd
# # #We combine the different trees (actually we should combine
# # #multiple chains/runs for the SAME tree - but all the trees
# # #are so similar, so I guess it won't make much of a difference)
# res_hom_combine <- do.call(mcmc.list, res_hom) #Combine the coda results in one mcmc.list object
# res_combine <- do.call(mcmc.list, res) #Combine the coda results in one mcmc.list object
# 
# # #Trace plots
# # #Look at the trace plots for LH and the PC axes (alpha parameter)
# # #Homogeneous rates
# op <- par(mfrow=c(4,6))
# traceplot(res_hom_combine[,c(1:24)], sub = "Homogeneous rates")
# par(op)
# 
# # #Variable rates
# op <- par(mfrow=c(4,6))
# traceplot(res_combine[,c(1:24)], sub = "Variable rates")
# par(op)
# 
# # #Autocorrelation plots
# acfplot(res_hom_combine[,c(1:24)], main = "Homogeneous rates") # #Homogeneous rates
# acfplot(res_combine[,c(1:24)], main = "Variable rates") # #Variable rates
# 
# 
# # #Get effective sizes (should be > 200)
# effectiveSize(res_hom_combine) # #Homogeneous rates
# effectiveSize(res_combine) # #Variable rates
# 
# # #Gelman and Rubin's convergence diagnostic: Potential Scale Reduction Factor (PSRF)
# # #If the values are above 1.05, you should run the chains longer
# # #Works ONLY when multiple chains were run!
# gelman.diag(res_hom_combine,autoburnin=FALSE,multivariate=TRUE) # #Homogeneous rates
# gelman.diag(res_combine,autoburnin=FALSE,multivariate=TRUE) # #Variable rates (does not work sometimes - multivariate seems
# #to be buggy: if it does not work, you can set it to FALSE)
# 
# # #Density Plots
# # #requires lattice library
# densityplot(res_hom_combine, main = "Homogeneous rates") # #Homogeneous rates
# densityplot(res_combine, main = "Variable rates") # #Variable rates
# # #Parameter summary
# summary(res_hom_combine) # #Homogeneous rates
# summary(res_combine) # #Variable rates
# # #Highest Posterior Density intervals
# HPDinterval(res_hom_combine) # #Homogeneous rates
# HPDinterval(res_combine) # #Variable rates
# ########End of convergence checks###########


# ###Calculate logBF####
#Import marginal likelihood from BayesTraits runs
# #No local transformations:
varRates.marg.logLik <- list()
for(i in 1:length(BayesTraits.trees)){
  varRates.marg.logLik[[i]] <- readLines(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",i,".log.txt.Stones.txt", sep=""))
  varRates.marg.logLik[[i]] <- tail(varRates.marg.logLik[[i]], 1)
  varRates.marg.logLik[[i]] <- as.numeric(gsub("[^[:digit:].-]", "", varRates.marg.logLik[[i]]))
  varRates.marg.logLik[[i]]
}


homRates.marg.logLik <- list()
for(i in 1:length(BayesTraits.trees)){
  homRates.marg.logLik[[i]] <- readLines(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.homogeneous.",i,".log.txt.Stones.txt", sep=""))
  homRates.marg.logLik[[i]] <- tail(homRates.marg.logLik[[i]], 1)
  homRates.marg.logLik[[i]] <- as.numeric(gsub("[^[:digit:].-]", "", homRates.marg.logLik[[i]]))
  homRates.marg.logLik[[i]]
}

# #Create a small function that eases the import the marginal likelihood
marglogLik_import <- function(myclade, tree.number, evol.rate, evol.model){
  marg.logLik <- readLines(paste("./BayesTraits/",myclade,".pca.scores.pruned.txt.",evol.rate,".",evol.model,".",tree.number,".log.txt.Stones.txt", sep=""))
  marg.logLik <- tail(marg.logLik, 1)
  marg.logLik <- as.numeric(gsub("[^[:digit:].-]", "", marg.logLik))
  return(marg.logLik)
}

# #Local or whole tree transformations:
homRates.kappa.marg.logLik <- list() # #Kappa: Hom. rates
hetRates.kappa.marg.logLik <- list() # #Kappa: Het. rates
homRates.lambda.marg.logLik <- list() # #Lambda: Hom. rates
hetRates.lambda.marg.logLik <- list() # #Lambda: Het. rates
homRates.delta.marg.logLik <- list() # #Delta: Hom. rates
hetRates.delta.marg.logLik <- list() # #Delta: Het. rates
if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  for(i in 1:length(BayesTraits.trees)){
    # #Kappa
    homRates.kappa.marg.logLik[[i]] <- marglogLik_import(myclade = myclade, tree.number = i, evol.rate = "homogeneous", evol.model = "kappa")
    hetRates.kappa.marg.logLik[[i]] <- marglogLik_import(myclade = myclade, tree.number = i, evol.rate = "heterogeneous", evol.model = "kappa")
    # #Lambda
    homRates.lambda.marg.logLik[[i]] <- marglogLik_import(myclade = myclade, tree.number = i, evol.rate = "homogeneous", evol.model = "lambda")
    hetRates.lambda.marg.logLik[[i]] <- marglogLik_import(myclade = myclade, tree.number = i, evol.rate = "heterogeneous", evol.model = "lambda")
    # #Delta
    homRates.delta.marg.logLik[[i]] <- marglogLik_import(myclade = myclade, tree.number = i, evol.rate = "homogeneous", evol.model = "delta")
    hetRates.delta.marg.logLik[[i]] <- marglogLik_import(myclade = myclade, tree.number = i, evol.rate = "heterogeneous", evol.model = "delta")
  }
}


# #Create dataframe that contains the marginal likelihoods of all models
if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  marg.logLik <- data.frame(TreeNo = seq(1:length(BayesTraits.trees)), 
                            bm.hom = unlist(homRates.marg.logLik), bm.het = unlist(varRates.marg.logLik),
                            kappa.hom = unlist(homRates.kappa.marg.logLik), kappa.het = unlist(hetRates.kappa.marg.logLik),
                            lambda.hom = unlist(homRates.lambda.marg.logLik), lambda.het = unlist(hetRates.lambda.marg.logLik),
                            delta.hom = unlist(homRates.delta.marg.logLik), delta.het = unlist(hetRates.delta.marg.logLik))
  marg.logLik.dropCol <- c("TreeNo", "max", "min", "max.model", "min.model", "max_no.bm.hom", "max.model_no.bm.hom", "min_no.bm.hom", "min.model_no.bm.hom") # #Columns, that can be dropped for certain calculations
  # #Find for each tree the model with the max. marginal likelihood
  # #We calculate the max. value for each row, therefore we have to exclude certain columns (e.g., "TreeNo")
  # #Note, that if the likelihoods of two models in a row are exactly the same (unlikely to be the case), the first likelihood will be picked
  marg.logLik$max <- apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)], 1, max) # #Find max. marginal likelihood in each row
  marg.logLik$max.model <- colnames(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)])[max.col(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)],ties.method="first")] # #Find model with max. likelihood in each row
  # #Find for each tree the model with the min. marginal likelihood
  marg.logLik$min <- apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)], 1, min) # #Find min. marginal likelihood in each row
  marg.logLik$min.model <- colnames(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)])[apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)],1,which.min)] # #Find model with min. likelihood in each row
  
  # #Repeat previous calculations but exclude bm.hom model
  # #Might not be needed?
  marg.logLik$max_no.bm.hom <- apply(marg.logLik[, !(colnames(marg.logLik) %in% c(marg.logLik.dropCol, "bm.hom"))], 1, max) # #Find max. marginal likelihood in each row
  marg.logLik$max.model_no.bm.hom <- colnames(marg.logLik[, !(colnames(marg.logLik) %in% c(marg.logLik.dropCol, "bm.hom"))])[max.col(marg.logLik[, !(colnames(marg.logLik) %in% c(marg.logLik.dropCol, "bm.hom"))],ties.method="first")] # #Find model with max. likelihood in each row
  marg.logLik$min_no.bm.hom <- apply(marg.logLik[, !(colnames(marg.logLik) %in% c(marg.logLik.dropCol, "bm.hom"))], 1, min) # #Find min. marginal likelihood in each row
  marg.logLik$min.model_no.bm.hom <- colnames(marg.logLik[, !(colnames(marg.logLik) %in% c(marg.logLik.dropCol, "bm.hom"))])[apply(marg.logLik[, !(colnames(marg.logLik) %in% c(marg.logLik.dropCol, "bm.hom"))],1,which.min)] # #Find model with min. likelihood in each row
  
  marg.logLik
}



# #Create dataframe that contains the summary of the marginal logLikelihood of all models
if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  marg.logLik.summary <- data.frame(Models = names(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)]), 
                              Mean = unname(colMeans(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)])), # #we also use unname, otherwise the row.names will be replaced
                              Median = unname(apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)], 2, median)),
                              SD = unname(apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)], 2, sd)),
                              Max = unname(apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)], 2, max)),
                              Min = unname(apply(marg.logLik[, !(colnames(marg.logLik) %in% marg.logLik.dropCol)], 2, min)))
  
  # #Count the number of times a certain evol. model is preferred over all other models, i. e. the number of times
  # #a certain evol. model is our max.model and create a respective column
  # #Repeat the same for the min.model, i. e. the model with the least amount of support
  # #This could be done using the table() function (see Bayes factors), but then we would have to use factor() for our marg.logLik object
  # #to implement all possible levels - instead we choose this approach for the marg.logLik.summary (alternative approach: see logBF.summary)
  marg.logLik.summary$Max.model.No <- NA
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "bm.hom"] <- length((which(marg.logLik$max.model == "bm.hom")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "bm.het"] <- length((which(marg.logLik$max.model == "bm.het")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "kappa.hom"] <- length((which(marg.logLik$max.model == "kappa.hom")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "kappa.het"] <- length((which(marg.logLik$max.model == "kappa.het")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "lambda.hom"] <- length((which(marg.logLik$max.model == "lambda.hom")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "lambda.het"] <- length((which(marg.logLik$max.model == "lambda.het")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "delta.hom"] <- length((which(marg.logLik$max.model == "delta.hom")))
  marg.logLik.summary$Max.model.No[marg.logLik.summary$Models == "delta.het"] <- length((which(marg.logLik$max.model == "delta.het")))
  marg.logLik.summary$Max.model.perc <- 100*(marg.logLik.summary$Max.model.No/sum(marg.logLik.summary$Max.model.No)) # #Calculate percentage of max. model
  # #Count number of times an evol. model is min. model
  marg.logLik.summary$Min.model.No <- NA
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "bm.hom"] <- length((which(marg.logLik$min.model == "bm.hom")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "bm.het"] <- length((which(marg.logLik$min.model == "bm.het")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "kappa.hom"] <- length((which(marg.logLik$min.model == "kappa.hom")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "kappa.het"] <- length((which(marg.logLik$min.model == "kappa.het")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "lambda.hom"] <- length((which(marg.logLik$min.model == "lambda.hom")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "lambda.het"] <- length((which(marg.logLik$min.model == "lambda.het")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "delta.hom"] <- length((which(marg.logLik$min.model == "delta.hom")))
  marg.logLik.summary$Min.model.No[marg.logLik.summary$Models == "delta.het"] <- length((which(marg.logLik$min.model == "delta.het")))
  marg.logLik.summary$Min.model.perc <- 100*(marg.logLik.summary$Min.model.No/sum(marg.logLik.summary$Min.model.No)) # #Calculate percentage of min. model
  
  marg.logLik.summary
  
  marg.logLik.summary <- marg.logLik.summary[, !(colnames(marg.logLik.summary) %in% "Max.model.No.Var1")] # #Remove "Max.model.No.Var1" column
  names(marg.logLik.summary)[names(marg.logLik.summary) == "Max.model.No.Freq"] <- "Max.model.No" # #Replace column name
  names(marg.logLik.summary)[names(marg.logLik.summary) == "Min.model.No.Freq"] <- "Min.model.No" # #Replace column name
  names(marg.logLik.summary)[names(marg.logLik.summary) == "Min.model.No.Var1"] <- "Max.model.perc" # #Replace column name
  marg.logLik.summary$Max.model.perc <- 100*(marg.logLik.summary$Max.model.No/sum(marg.logLik.summary$Max.model.No)) # #Calculate percentage of max. model
  marg.logLik.summary$Min.model.perc <- 100*(marg.logLik.summary$Min.model.No/sum(marg.logLik.summary$Min.model.No)) # #Calculate percentage of min. model
  
  # #Repeat (some of the) previous calculations but exclude bm.hom model
  # #Might not be needed?
  marg.logLik.summary$Max.model.No_no.bm.hom <- NA
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "bm.hom"] <- length((which(marg.logLik$max.model_no.bm.hom == "bm.hom")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "bm.het"] <- length((which(marg.logLik$max.model_no.bm.hom == "bm.het")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "kappa.hom"] <- length((which(marg.logLik$max.model_no.bm.hom == "kappa.hom")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "kappa.het"] <- length((which(marg.logLik$max.model_no.bm.hom == "kappa.het")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "lambda.hom"] <- length((which(marg.logLik$max.model_no.bm.hom == "lambda.hom")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "lambda.het"] <- length((which(marg.logLik$max.model_no.bm.hom == "lambda.het")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "delta.hom"] <- length((which(marg.logLik$max.model_no.bm.hom == "delta.hom")))
  marg.logLik.summary$Max.model.No_no.bm.hom[marg.logLik.summary$Models == "delta.het"] <- length((which(marg.logLik$max.model_no.bm.hom == "delta.het")))
  marg.logLik.summary$Max.model.perc_no.bm.hom <- 100*(marg.logLik.summary$Max.model.No_no.bm.hom/sum(marg.logLik.summary$Max.model.No_no.bm.hom, na.rm=TRUE)) # #Calculate percentage of max. model
  
  marg.logLik.summary$Min.model.No_no.bm.hom <- NA
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "bm.hom"] <- length((which(marg.logLik$min.model_no.bm.hom == "bm.hom")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "bm.het"] <- length((which(marg.logLik$min.model_no.bm.hom == "bm.het")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "kappa.hom"] <- length((which(marg.logLik$min.model_no.bm.hom == "kappa.hom")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "kappa.het"] <- length((which(marg.logLik$min.model_no.bm.hom == "kappa.het")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "lambda.hom"] <- length((which(marg.logLik$min.model_no.bm.hom == "lambda.hom")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "lambda.het"] <- length((which(marg.logLik$min.model_no.bm.hom == "lambda.het")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "delta.hom"] <- length((which(marg.logLik$min.model_no.bm.hom == "delta.hom")))
  marg.logLik.summary$Min.model.No_no.bm.hom[marg.logLik.summary$Models == "delta.het"] <- length((which(marg.logLik$min.model_no.bm.hom == "delta.het")))
  marg.logLik.summary$Min.model.perc_no.bm.hom <- 100*(marg.logLik.summary$Min.model.No_no.bm.hom/sum(marg.logLik.summary$Min.model.No_no.bm.hom, na.rm=TRUE)) # #Calculate percentage of min. model
  
  marg.logLik.summary
}




# #Create dataframe that contains the Bayes factor of all models
# #Calculate Log BF (remember this is the natural log - see Baker et al., 2016!)
# #Log BF = 2(log marginal likelihood complex model - log marginal likelihood simple model)
# #We use the homogeneous rate (Brownian motion) model as the simple reference model
# #Note, that we also calculated the logBF for the homogeneous rate (Brownian motion) model, which will obviously always be 0
# #This is only done to detect trees, for which a homogeneous rate model is always favoured (i. e. bm.hom falls into max.model)
# #or never favoured (i. e. bm.hom falls into min.model)
if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  logBF.df <- data.frame(TreeNo = seq(1:length(BayesTraits.trees)), 
                         bm.hom = 2*(marg.logLik$bm.hom-marg.logLik$bm.hom), bm.het = 2*(marg.logLik$bm.het-marg.logLik$bm.hom),
                         kappa.hom = 2*(marg.logLik$kappa.hom-marg.logLik$bm.hom), kappa.het = 2*(marg.logLik$kappa.het-marg.logLik$bm.hom),
                         lambda.hom = 2*(marg.logLik$lambda.hom-marg.logLik$bm.hom), lambda.het = 2*(marg.logLik$lambda.het-marg.logLik$bm.hom),
                         delta.hom = 2*(marg.logLik$delta.hom-marg.logLik$bm.hom), delta.het = 2*(marg.logLik$delta.het-marg.logLik$bm.hom))
  # #Find for each tree/row the model with the max. Bayes factor
  # #We calculate the max. value for each row, therefore we have to exclude certain columns (e.g., "TreeNo")
  # #Note, that if the Bayes factors of two models in a row are exactly the same (unlikely to be the case), the first Bayes factor will be picked
  logBF.df$max <- apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)], 1, max) # #Find max. Bayes factor in each row
  logBF.df$max.model <- factor(colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)])[max.col(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)],ties.method="first")], # #Find model with max. Bayes factor in each row
                                  levels = colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)]))  # #We also specify factor levels within the factor() function (necessary, as sometimes one model might never end up as a max.model)
  # #NEVER specify factor levels manually using the levels() command (see below) --> this can totally mess up your data/analysis!!!!
  # # #####levels(logBF.df$max.model) <- colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)]) # #NEVER EVER USE levels() MANUALLY TO SPECIFY FACTOR LEVELS!!!
  
  # #Find for each tree the model with the min. marginal likelihood
  logBF.df$min <- apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)], 1, min) # #Find min. Bayes factor in each row
  logBF.df$min.model <- factor(colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)])[apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)],1,which.min)], # #Find model with min. Bayes factor in each row 
                               levels = colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)]))  # #We also specify factor levels within the factor() function (necessary, as sometimes one model might never end up as a min.model)
  # #NEVER specify factor levels manually using the levels() command (see below) --> this can totally mess up your data/analysis!!!!
  # # ####logBF.df$min.model <- factor(colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)])[apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)],1,which.min)]) # #NEVER EVER USE as.factor() or factor() first and then levels() MANUALLY TO SPECIFY FACTOR LEVELS!!!
  # # ####levels(logBF.df$min.model) <- colnames(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)])  # #NEVER EVER USE levels() MANUALLY TO SPECIFY FACTOR LEVELS!!!
  
  # #Repeat previous calculations but exclude bm.hom model
  # #Might not be needed?
  logBF.df$max_no.bm.hom <- apply(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))], 1, max) # #Find max. Bayes factor in each row
  logBF.df$max.model_no.bm.hom <- factor(colnames(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))])[max.col(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))],ties.method="first")], # #Find model with max. Bayes factor in each row
                               levels = colnames(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))]))
  logBF.df$min_no.bm.hom <- apply(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))], 1, min) # #Find min. Bayes factor in each row
  logBF.df$min.model_no.bm.hom <- factor(colnames(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))])[apply(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))],1,which.min)], # #Find model with min. Bayes factor in each row 
                               levels = colnames(logBF.df[, !(colnames(logBF.df) %in% c(marg.logLik.dropCol, "bm.hom"))])) 
  
  
  # #Quick check - models with the highest Bayes factor should also have the highest marginal likelihood. If that is not the case,
  # #something went wrong (might be factor() in that case). Similarly, models with the lowest Bayes factor should have the lowest marginal likelihood
  # #(if the BF for the bm.hom is not calculated - as it will always be 0 - this check would need to be modifed accordingly).
  if(all(logBF.df$max.model == marg.logLik$max.model & logBF.df$min.model == marg.logLik$min.model) == TRUE){
    cat("Good, models with the highest/lowest Bayes factor also have the highest/lowest marginal likelihood.\n", sep="")
  } else{
    cat("Careful, models with the highest/lowest Bayes factor DO NOT have the highest/lowest marginal likelihood. Something went wrong.\n", sep="")
  }
  logBF.df
}

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Create dataframe that contains the summary of the Bayes factor of all models
  logBF.summary <- data.frame(Models = names(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)]), 
                              Mean = unname(colMeans(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)])), # #we also use unname, otherwise the row.names will be replaced
                              Median = unname(apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)], 2, median)),
                              SD = unname(apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)], 2, sd)),
                              Max = unname(apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)], 2, max)),
                              Min = unname(apply(logBF.df[, !(colnames(logBF.df) %in% marg.logLik.dropCol)], 2, min)),
                              # #Count the number of times a certain evol. model is preferred over all other models, i. e. the number of times
                              # #a certain evol. model is our max.model
                              # #Repeat the same for the min.model, i. e. the model with the least amount of support
                              Max.model.No = table(logBF.df$max.model), Min.model.No = table(logBF.df$min.model))
  logBF.summary
  # #Check, that the model counts were assigned to the correct model
  if(all(logBF.summary$Models == logBF.summary$Max.model.No.Var1 & logBF.summary$Models == logBF.summary$Min.model.No.Var1 & 
         length((which(logBF.df$max.model == "bm.het"))) == logBF.summary[logBF.summary["Models"] == "bm.het",]$Max.model.No.Freq & 
         length((which(logBF.df$min.model == "kappa.hom"))) == logBF.summary[logBF.summary["Models"] == "kappa.hom",]$Min.model.No.Freq) == TRUE){
    cat("Good, model counts were assigned to correct model.\n", sep="")
  } else{
    cat("Careful, model counts were assigned to correct model. Something went wrong.\n", sep="")
  }
  
  logBF.summary <- logBF.summary[, !(colnames(logBF.summary) %in% "Max.model.No.Var1")] # #Remove "Max.model.No.Var1" column
  names(logBF.summary)[names(logBF.summary) == "Max.model.No.Freq"] <- "Max.model.No" # #Replace column name
  names(logBF.summary)[names(logBF.summary) == "Min.model.No.Freq"] <- "Min.model.No" # #Replace column name
  names(logBF.summary)[names(logBF.summary) == "Min.model.No.Var1"] <- "Max.model.perc" # #Replace column name
  logBF.summary$Max.model.perc <- 100*(logBF.summary$Max.model.No/sum(logBF.summary$Max.model.No)) # #Calculate percentage of max. model
  logBF.summary$Min.model.perc <- 100*(logBF.summary$Min.model.No/sum(logBF.summary$Min.model.No)) # #Calculate percentage of min. model
  
  # #Repeat (some of the) previous calculations but exclude bm.hom model
  # #Might not be needed?
  logBF.summary$Max.model.No_no.bm.hom <- c(NA, table(logBF.df$max.model_no.bm.hom))
  logBF.summary$Min.model.No_no.bm.hom <- c(NA, table(logBF.df$min.model_no.bm.hom))
  logBF.summary$Max.model.perc_no.bm.hom <- 100*(logBF.summary$Max.model.No_no.bm.hom/sum(logBF.summary$Max.model.No_no.bm.hom, na.rm=TRUE)) # #Calculate percentage of max. model
  logBF.summary$Min.model.perc_no.bm.hom <- 100*(logBF.summary$Min.model.No_no.bm.hom/sum(logBF.summary$Min.model.No_no.bm.hom, na.rm=TRUE)) # #Calculate percentage of min. model
  
  logBF.summary
  
  # #Very basic bar graph showing the percentage of trees explained by a certain model
  # ggplot(data=logBF.summary, aes(x=logBF.summary$Models, y=logBF.summary$Max.model.perc)) +
  #   geom_bar(stat="identity")
}






# #Calculate Log BF (remember this is the natural log - see Baker et al., 2016!)
# #Log BF = 2(log marginal likelihood complex model - log marginal likelihood simple model)
# #No local transformations
logBF <- vector()
for(i in 1:length(BayesTraits.trees)){
  logBF[[i]] <- 2*(varRates.marg.logLik[[i]]-homRates.marg.logLik[[i]])
  logBF[[i]]
}
logBF
logBF <- as.data.frame(logBF) # #Convert vector logBF to data.frame (this way, we can use a column name and we get the same format as
# #with the local/whole tree transformations approach (see below)) - if this causes any issues, go back to vector format and revise the 
# #local/whole tree transformations approach
names(logBF) <- "bm.het"
logBF

if(BT_localTrans == "YES" || BT_globalTrans == "YES"){
  # #Taking into account local or whole tree transformations
  # #Find model with highest mean logBF (we could also use the marginal likelihood instead - should give the same answer in this case)
  # #Alternatively, we could also use the median or the model, which is favoured most of the time (i.e. max(logBF.summary$Max.model.No) or max(logBF.summary$Max.model.perc))
  prefmodel_mean <- as.character(logBF.summary$Models[which.max(logBF.summary$Mean)]) # #Model with highest mean logBF
  # #If we don't use as.character(), prefmodel_mean is stored as a factor (with all levels), which causes issues when using it in subsequent functions
  prefmodel_mean
  # prefmodel_median <- as.character(logBF.summary$Models[which.max(logBF.summary$Median)]) # #Model with highest median logBF
  # prefmodel_median
  # prefmodel_maxNo <- as.character(logBF.summary$Models[which.max(logBF.summary$Max.model.No)]) # #Model which is favoured most of the time
  # prefmodel_maxNo
  logBF <- logBF.df[prefmodel_mean]
  logBF
}

if(min(logBF)>=10){
  cat("Good, lnBF is larger than 10. A ",names(logBF)," model is favoured for all trees!
      The smallest lnBF is ", min(logBF),".", sep="")
} else{
  no.large.logBF2 <- NROW(logBF[logBF>= 2]) # #Number of models/trees with a lnBF >=2
  no.large.logBF5 <- NROW(logBF[logBF>= 5]) # #Number of models/trees with a lnBF >=5
  no.large.logBF10 <- NROW(logBF[logBF>= 10]) # #Number of models/trees with a lnBF >=10
  cat("Careful, some lnBF are smaller than 10. A ",names(logBF)," model might NOT be favoured for all trees!
      The smallest lnBF is ", min(logBF),".
      There are only ",no.large.logBF2," models out of ",NROW(logBF)," with a lnBF >= 2 (''positive evidence'').
      There are only ",no.large.logBF5," models out of ",NROW(logBF)," with a lnBF >= 5 (''strong evidence'').
      There are only ",no.large.logBF10," models out of ",NROW(logBF)," with a lnBF >= 10 (''very strong evidence'').", sep="")
  small.logBF <- matrix(ncol = 2, nrow = NROW(logBF[logBF>= 2]))
  colnames(small.logBF) <- c("TreeNo", "logBF")
  small.logBF[,"TreeNo"] <- which(logBF >= 2)
  small.logBF[,"logBF"] <- logBF[logBF>= 2]
  #small.logBF # #All trees/models with a lnBF >=2
  # #Models/trees with a logBF that is equal/larger than 2 but smaller than 5
  small.logBF2to5 <- matrix(ncol = 2, nrow = NROW(logBF[logBF >= 2 & logBF < 5]))
  colnames(small.logBF2to5) <- c("TreeNo", "logBF")
  small.logBF2to5[,"TreeNo"] <- which(logBF >= 2 & logBF < 5)
  small.logBF2to5[,"logBF"] <- logBF[logBF >= 2 & logBF < 5]
  #small.logBF2to5 # #Models/trees with logBF >= 2 and < 5
  # #Models/trees with a logBF that is equal/larger than 5 but equal/smaller than 10
  small.logBF5to10 <- matrix(ncol = 2, nrow = NROW(logBF[logBF >= 5 & logBF <= 10]))
  colnames(small.logBF5to10) <- c("TreeNo", "logBF")
  small.logBF5to10[,"TreeNo"] <- which(logBF >= 5 & logBF <= 10)
  small.logBF5to10[,"logBF"] <- logBF[logBF >= 5 & logBF <= 10]
  #small.logBF5to10 # #Models/trees with logBF >= 5 and <= 10
  # #Models/trees with a logBF that is equal/larger than 2 but equal/smaller than 10
  small.logBF2to10 <- matrix(ncol = 2, nrow = NROW(logBF[logBF >= 2 & logBF <= 10]))
  colnames(small.logBF2to10) <- c("TreeNo", "logBF")
  small.logBF2to10[,"TreeNo"] <- which(logBF >= 2 & logBF <= 10)
  small.logBF2to10[,"logBF"] <- logBF[logBF >= 2 & logBF <= 10]
  #small.logBF2to10 # #Models/trees with logBF >= 2 and <= 10
  # #Models/trees with a logBF that is equal/larger than 2 but equal/smaller than 10
  small.logBF2below <- matrix(ncol = 2, nrow = NROW(logBF[logBF < 2]))
  colnames(small.logBF2below) <- c("TreeNo", "logBF")
  small.logBF2below[,"TreeNo"] <- which(logBF < 2)
  small.logBF2below[,"logBF"] <- logBF[logBF < 2]
  #small.logBF2below # #Models/trees with logBF < 2
}


# ####Postprocessing####
#Move txt.PP.txt files to postprocessing folder
# identify the folders
bayes.folder <- "./BayesTraits/"
bayes.postproc.folder <- "./BayesTraits_PostProcessing_Results/"
bayes.tmp.folder <- "./BayesTraits_tmp/" # #temp folder

# find the files that you want
if(BT_version == "BayesTraitsV2.0.2"){
  list.of.files <- list.files(bayes.folder, glob2rx(paste(myclade,".pca.scores.pruned.*.txt.PP.txt", sep="")),full.names=T)
  list.of.files
} else{ #BayesTraits V3 saves output files differently
  list.of.files <- list.files(bayes.folder, glob2rx(paste(myclade,".pca.scores.pruned.txt.*.log.txt.VarRates.txt", sep="")),full.names=T)
  list.of.files
}

if(names(logBF) == "bm.het"){
  # #Note, that for whole tree transformations (kappa, lambda, delta) with the homogeneous rate model it is nonsensical to use the 
  # #postprocessing tool to get variable rates, as the there is just a single rate modifier. Indeed, for these models the postprocessing tool
  # #will not work
  list.of.files_bestModel <- list.of.files[grep("kappa|lambda|delta", list.of.files, invert = TRUE)] # #Find all files, which do NOT contain kappa OR lambda OR delta
} else if(names(logBF) == "kappa.hom"){
  list.of.files_bestModel <- list.of.files[grep("homogeneous.kappa", list.of.files)] # #Find all files, which contain homogeneous kappa
} else if(names(logBF) == "kappa.het"){
  list.of.files_bestModel <- list.of.files[grep("heterogeneous.kappa", list.of.files)] # #Find all files, which contain heterogeneous kappa
} else if(names(logBF) == "lambda.hom"){
  list.of.files_bestModel <- list.of.files[grep("homogeneous.lambda", list.of.files)] # #Find all files, which contain homogeneous lambda
} else if(names(logBF) == "lambda.het"){
  list.of.files_bestModel <- list.of.files[grep("heterogeneous.lambda", list.of.files)] # #Find all files, which contain heterogeneous lambda
} else if(names(logBF) == "delta.hom"){
  list.of.files_bestModel <- list.of.files[grep("homogeneous.delta", list.of.files)] # #Find all files, which contain homogeneous delta
} else if(names(logBF) == "delta.het"){
  list.of.files_bestModel <- list.of.files[grep("heterogeneous.delta", list.of.files)] # #Find all files, which contain heterogeneous delta
} else if(names(logBF) == "bm.hom"){
  cat("A ",names(logBF)," is apparently favoured over all competing models. Therefore it does not appear necessary to carry out any postprocessing.", sep="")
  # #If we still want to run the postprocessing, we could just use 
  # #logBF.summary_no.bm.hom <- logBF.summary[logBF.summary$Models != "bm.hom",] to get logBF.summary dataframe without bm.hom
  # #and then
  # #prefmodel_mean_no.bm.hom <- as.character(logBF.summary_no.bm.hom$Models[which.max(logBF.summary_no.bm.hom$Mean)])
  # #logBF_no.bm.hom <- logBF.df[prefmodel_mean_no.bm.hom]
  # #logBF_no.bm.hom
  # #to determine the next highest logBF after excluding bm.hom
  # #But I guess this is something, that we don't want to automate (for now?) and we will rather do it manually
}

# #Clear tmp folder first
if(BT_version == "BayesTraitsV2.0.2"){
  files.to.delete <- list.files(bayes.tmp.folder, glob2rx(paste(myclade,".pca.scores.pruned.*.txt.PP.txt", sep="")),full.names=T)
} else{ #BayesTraits V3 saves output files differently
  files.to.delete <- list.files(bayes.tmp.folder, glob2rx(paste(myclade,".pca.scores.pruned.txt.*.log.txt.VarRates.txt", sep="")),full.names=T)
}
file.remove(files.to.delete)

# #Copy the files to the tmp folder and then rename them
# #By using a tmp folder, we avoid clashing of file names (as we can't just clear the bayes.postproc.folder safely)
file.copy(list.of.files_bestModel, bayes.tmp.folder, overwrite = TRUE)
if(BT_version == "BayesTraitsV2.0.2"){
  list.of.files_bestModel_tmp <- list.files(bayes.tmp.folder, glob2rx(paste(myclade,".pca.scores.pruned.*.txt.PP.txt", sep="")),full.names=T)
} else{ #BayesTraits V3 saves output files differently
  list.of.files_bestModel_tmp <- list.files(bayes.tmp.folder, glob2rx(paste(myclade,".pca.scores.pruned.txt.*.log.txt.VarRates.txt", sep="")),full.names=T)
}
list.of.files_bestModel.renamed <- gsub("homogeneous.|heterogeneous.|kappa.|lambda.|delta.", "", list.of.files_bestModel_tmp)
file.rename(from = list.of.files_bestModel_tmp, to = list.of.files_bestModel.renamed) # #we rename the files because we don't want rewrite
# #a lot of the downstream code - furthermore, we want to only postprocess the best alternative model (otherwise we would just run lots of analyses
# #that can require quite some performance without actually using the results)

# #Now copy the files to the postprocessing folder
file.copy(list.of.files_bestModel.renamed, bayes.postproc.folder, overwrite = TRUE)



#Upload files for postprocessing
#Generate curl upload files
#Relative file paths don't work under Windows+curl (THEY DO, HOWEVER, UNDER LINUX!)
curl_upload <- list()
if(BT_version == "BayesTraitsV2.0.2"){
  if(myOS == "Windows"){
    #ABSOLTE PATH (WORKS WITH WINDOWS)
    for(i in 1:length(BayesTraits.trees)){
      curl_upload[[i]] <- paste("curl -v -F fileToUpload=@C:/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.",i,".txt.PP.txt -F submit='Upload File' -o C:/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.postproc.",i,".html http://www.evolution.reading.ac.uk/VarRatesWebPP/VarRatesWebPP.php", sep="")
    }
  } else{
    #RELATIVE PATH (WORKS WITH LINUX)
      for(i in 1:length(BayesTraits.trees)){
        curl_upload[[i]] <- paste("curl -v -F fileToUpload=@./BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.",i,".txt.PP.txt -F submit='Upload File' -o ./BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.postproc.",i,".html http://www.evolution.reading.ac.uk/VarRatesWebPP/VarRatesWebPP.php", sep="")
      }
  }
} else{#again, for BayesTraits V3 we have to use slightly different names (as the default output is different)
  if(myOS == "Windows"){
    #ABSOLTE PATH (WORKS WITH WINDOWS)
    for(i in 1:length(BayesTraits.trees)){
      curl_upload[[i]] <- paste("curl -v -F fileToUpload=@C:/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.txt.",i,".log.txt.VarRates.txt -F submit='Upload File' -o C:/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.postproc.",i,".html http://www.evolution.reading.ac.uk/VarRatesWebPP/VarRatesWebPP.php", sep="")
    }
  } else{
    #RELATIVE PATH (WORKS WITH LINUX)
    for(i in 1:length(BayesTraits.trees)){
      curl_upload[[i]] <- paste("curl -v -F fileToUpload=@./BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.txt.",i,".log.txt.VarRates.txt -F submit='Upload File' -o ./BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.postproc.",i,".html http://www.evolution.reading.ac.uk/VarRatesWebPP/VarRatesWebPP.php", sep="")
    }
  }
}



#Write curl upload files
for(i in 1:length(BayesTraits.trees)){
  cat(curl_upload[[i]], file=paste("./BayesTraits_PostProcessing_Results/curl_upload.",i,".sh", sep=""))
}


# #Phylogenetically corrected rates for subclades
# #We extract subclades from BayesTraits.trees using the node labels of our myclade.tree object
# #We only need one of the BayesTraits.trees, as all trees will have the same tip labels

# #Prepare options file for phylogenetically corrected mean evolutionary rates through time
# #So far, this is only implemented for the local postprocessing tool
postproc_slidesNo <- paste("NoCats", NoCats)

# #Create options file
 
postprocOptions <- paste(postproc_slidesNo, "\n")

write(postprocOptions, file=paste("./BayesTraits_PostProcessing_Results/postprocOptions.txt", sep=""))

# #Check whether we postprocess locally or whether we do it remotely
# #Execute postprocessing accordingly (we also account for the version of BayesTraits we are using, as they produce
# #differently named output files)

# #Do postprocessing locally
# #Note, that the postprocessing tool is currently not capable of dealing with models that involve local transformations
# #It can deal, however, with whole tree transformations
# #If we want to estimate the phylogenetically corrected evol. rates for local transformations, currently we would have to use
# #the alternative rate import (based on the stretched trees in the "Output.trees" output of BayesTraits) and then use Manabu's
# #solution to the phylogenetic correction - note, that this approach is much slower/computationally demanding, than the use of
# #the postprocessing tool
if(PP_mode == "local"){
  if(BT_version == "BayesTraitsV2.0.2"){
    if(myOS == "Windows"){
      foreach(i = 1:length(BayesTraits.trees)) %dopar% {
        shell(paste('cd ',wd,'/BayesTraits_PostProcessing_Results/ & PPPostProcess.exe ' ,myclade,".pca.scores.pruned.",i,".txt.PP.txt postprocOptions.txt > ",myclade,".pca.scores.pruned.postproc.",i,".txt", sep=""))
      }
      # #Normal loop (NOT parallel!)
      # for(i in 1:length(BayesTraits.trees)){
      #   shell(paste('cd C:/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/ & PPPostProcess.exe ' ,"Archosauromorpha.pca.scores.pruned.",i,".txt.PP.txt > Archosauromorpha.pca.scores.pruned.postproc.",i,".txt", sep=""))
      #   # update progress bar
      #   #Sys.sleep(0.1)
      #   #setTxtProgressBar(pb, i)
      #   }
      # #close(pb)
      } else{
          #WORKS WITH LINUX!
          foreach(i = 1:length(BayesTraits.trees)) %dopar% {
            system(paste('PPPostProcess ',wd,"/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.",i,".txt.PP.txt ",wd,"/BayesTraits_PostProcessing_Results/postprocOptions.txt > ",wd,"/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.postproc.",i,".txt", sep=""))
            
            # #Not needed anymore
            #system(paste('PPPostProcess ' ,"./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.",i,".txt.PP.txt > ./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.postproc.",i,".txt", sep=""))
          }
          # #Normal loop (NOT parallel!)
          # for(i in 1:length(BayesTraits.trees)){
          #   system(paste('./BayesTraits_PostProcessing_Results/PPPostProcess ' ,"./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.",i,".txt.PP.txt > ./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.postproc.",i,".txt", sep=""))
          #   # update progress bar
          #   #Sys.sleep(0.1)
          #   #setTxtProgressBar(pb, i)
          # }
          # #close(pb)
      }
  } else{#when using BayesTraits V3
      if(myOS == "Windows"){
        foreach(i = 1:length(BayesTraits.trees)) %dopar% {
          shell(paste('cd ',wd,'/BayesTraits_PostProcessing_Results/ & PPPostProcess.exe ' ,myclade,".pca.scores.pruned.txt.",i,".log.txt.VarRates.txt postprocOptions.txt > ",myclade,".pca.scores.pruned.postproc.",i,".txt", sep=""))
        }
      # #Normal loop (NOT parallel!)
      #   for(i in 1:length(BayesTraits.trees)){
      #   shell(paste('cd C:/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/ & PPPostProcess.exe ' ,"Archosauromorpha.pca.scores.pruned.txt.",i,".log.txt.VarRates.txt > Archosauromorpha.pca.scores.pruned.postproc.",i,".txt", sep=""))
      #   # update progress bar
      #   #Sys.sleep(0.1)
      #   #setTxtProgressBar(pb, i)
      # }
    #close(pb)
    } else{
      #WORKS WITH LINUX!
      foreach(i = 1:length(BayesTraits.trees)) %dopar% {
        system(paste('PPPostProcess ' ,wd,"/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.txt.",i,".log.txt.VarRates.txt ",wd,"/BayesTraits_PostProcessing_Results/postprocOptions.txt > ",wd,"/BayesTraits_PostProcessing_Results/",myclade,".pca.scores.pruned.postproc.",i,".txt", sep="")) 
      }
      # #Normal loop (NOT parallel!)
      # for(i in 1:length(BayesTraits.trees)){
      #   system(paste('./BayesTraits_PostProcessing_Results/PPPostProcess ' ,"./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.txt.",i,".log.txt.VarRates.txt > ./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.postproc.",i,".txt", sep=""))
      #   # update progress bar
      #   #Sys.sleep(0.1)
      #   #setTxtProgressBar(pb, i)
      # }
      # #close(pb)
      }
  }
} else{
  #Execute curl upload files
  #We upload our results and download the postprocessed html file
  #Works for Linux but not for Windows (tried both curl in Anaconda for Windows and the Ubuntu Bash in Win10)
  if(myOS == "Windows"){
    #For Windows we have to use Cygwin AND absolute paths! Cygwin needs to be installed on Windows.
    for(i in 1:length(BayesTraits.trees)){
      system(paste('cmd.exe /c c:\\cygwin64\\bin\\env /cygdrive/c/Users/Internet/PhD/Related/UG_project/Analysis/Supertree/BayesTraits_PostProcessing_Results/curl_upload.',i,'.sh', sep=""), TRUE)
      # update progress bar
      Sys.sleep(0.1)
      setTxtProgressBar(pb, i)
    }
    close(pb)} else{
      #WORKS WITH LINUX!
      for(i in 1:length(BayesTraits.trees)){
        system(paste('bash ',wd,'/BayesTraits_PostProcessing_Results/curl_upload.',i,'.sh', sep=""))
        # update progress bar
        Sys.sleep(0.1)
        setTxtProgressBar(pb, i)
      }
      close(pb)}
}


# #Save my workspace after the postprocessing
save.image(file = paste(myclade,".PostProcess.Workspace.RData", sep=""))
#load(file = paste(myclade,".PostProcess.Workspace.RData", sep="")) #If you want to save time and only load the calculated results

#' #'
#' #' #########################################################################################