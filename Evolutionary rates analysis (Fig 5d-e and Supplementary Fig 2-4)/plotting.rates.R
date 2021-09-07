# #Author: Armin Elsler and Tom Stubbs
# #Please cite the associated publication when using this script or parts of it, and the following thesis
# 1. https://research-information.bris.ac.uk/en/studentTheses/macroevolution-of-early-tetrapods 

# #Start logfile
# sink(file="mylogfile.txt")
# #Check that working directory is properly set
wd <- getwd() 
wd 

#2. Install and load packages####

# install.packages("doParallel")        #Install packages if necessary
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


myclade_PalAss <- myclade


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

which(logBF < 2)
#not.het.remove <- which(logBF < 2)





# #Import the postprocessed txt/html files
# #We check whether we locally or remotely postprocessed our files and then treat them accordingly
# #This could also be achieved using "XML" library and the "readHTMLTable()" function,
# #but using htmltab is much more convenient ;)
# #Here just an example of how we might import the files using readHTMLTable (NOT NEEDED)
# library(XML)
# example.import <- readHTMLTable("./BayesTraits_PostProcessing_Results/Archosauromorpha.pca.scores.pruned.postproc.1.html", header=TRUE, as.data.frame = FALSE)
# example.import
# #Unfortunately htmltab appears to be a bit slow when importing the files - we might have to go back to readHTMLTable for larger analyses
bayes.postproc <- list()
bayes.postproc_phylcor <- list() # #list containing the phylogenetically corrected evol. rates
tmp_phylcor_row <- list() # #temporary list needed to store the row number which contains the string "Phylogenetically Corrected Rates For Root"
tmp_dropRows <- list() # #temporary list to store the row numbers we want to drop, which contain the string "Phylogenetically"


if(PP_mode == "local"){#Local import
  for(i in 1:length(BayesTraits.trees)){
    # #We have to import the postprocessed file twice as we need to determine, which rows should be imported and which should not first
    # #Then we only import those rows that contain the postprocessed results
    # #We exclude the rows that contain the results necessary for our rates-through-time plots, that are phylogenetically corrected
    bayes.postproc[[i]] <- read.delim(paste(wd,'/BayesTraits_PostProcessing_Results/',myclade,'.pca.scores.pruned.postproc.',i,'.txt', sep = ""),
                                      header = TRUE, check.names=FALSE, sep = "\t", colClasses = c(rep(NA, 22), rep("NULL", 1))) #check.names: Don't replace spaces with dots; colClasses: Don't import last column (it's a void column)
    tmp_phylcor_row[[i]] <- which(bayes.postproc[[i]] == "Phylogenetically Corrected Rates For Root") # #Find the row number containing the string "Phylogenetically Corrected Rates For Root"
    
    # #Find row numbers which contain "Phylogenetically Corrected Rates ..." string (not needed anymore)
    # #tmp_phylcor_Parareptilia_row[[i]] <- which(bayes.postproc[[i]] == "Phylogenetically Corrected Rates For Parareptilia") # #Find the row number containign the string "Phylogenetically Corrected Rates For Parareptilia"
    
    bayes.postproc[[i]] <- read.delim(paste(wd,'/BayesTraits_PostProcessing_Results/',myclade,'.pca.scores.pruned.postproc.',i,'.txt', sep = ""),
                                      header = TRUE, check.names=FALSE, sep = "\t", colClasses = c(rep(NA, 22), rep("NULL", 1)),
                                      nrows = tmp_phylcor_row[[i]]-1) #check.names: Don't replace spaces with dots; colClasses: Don't import last column (it's a void column)
    # #we exclude all rows following the row containing the string "Phylogenetically Corrected Rates For Root"
    
    # #Now we import the phylogenetically corrected rates into a separate list
    # #For "Root", i. e. the complete clade
    bayes.postproc_phylcor[[i]] <- read.delim(paste(wd,'/BayesTraits_PostProcessing_Results/',myclade,'.pca.scores.pruned.postproc.',i,'.txt', sep = ""),
                                              header = TRUE, check.names=FALSE, sep = "\t", #colClasses = c(rep(NA, 8), rep("NULL", 1)),
                                              skip = tmp_phylcor_row[[i]]+5)#, nrows = NoCats) #check.names: Don't replace spaces with dots; colClasses: Don't import last column (it's a void column)
    # #we skip all rows before the row containing the string "Phylogenetically Corrected Rates For Root"
    # #we could exclude all rows, that report phylogenetically corrected rates for subclades: "nrows = NoCats" (i. e. all rows, that do not belong
    # #to "Phylogenetically Corrected Rates For Root") --> but let's subset our dataset afterwards
    
    # #Let's replace the "Slice Height" column name (better not to include spaces in column names)
    colnames(bayes.postproc_phylcor[[i]])[colnames(bayes.postproc_phylcor[[i]])=="Slice Height"] <- "SliceHeight"
    
    # #Find rows to drop (containing the string "Phylogenetically [...]")
    # #We also drop the following row, as it contains just column names (e.g., "Rates")
    tmp_dropRows[[i]] <- grep("Phylogenetically", bayes.postproc_phylcor[[i]]$Rates)
    tmp_dropRows[[i]] <- c(tmp_dropRows[[i]], tmp_dropRows[[i]]+1)
    
    # #Now drop the respective rows
    bayes.postproc_phylcor[[i]] <- bayes.postproc_phylcor[[i]][-tmp_dropRows[[i]], ]
    # #We also need to drop the rows containing the string "Node at slice has not started yet"
    bayes.postproc_phylcor[[i]] <- bayes.postproc_phylcor[[i]][!(bayes.postproc_phylcor[[i]]$SliceHeight == "Node at slice has not started yet"),]
    
    # #As we initially imported the dataset including subclades, we had some strings in the columns
    # #These were therefore recognized as factors - let's make them numeric again
    bayes.postproc_phylcor[[i]]$Slice <- as.numeric(as.character(bayes.postproc_phylcor[[i]]$Slice))
    bayes.postproc_phylcor[[i]]$SliceHeight <- as.numeric(as.character(bayes.postproc_phylcor[[i]]$SliceHeight))
    bayes.postproc_phylcor[[i]]$Tips <- as.numeric(as.character(bayes.postproc_phylcor[[i]]$Tips))
    bayes.postproc_phylcor[[i]]$Lh <- as.numeric(as.character(bayes.postproc_phylcor[[i]]$Lh))
    bayes.postproc_phylcor[[i]]$Alpha <- as.numeric(as.character(bayes.postproc_phylcor[[i]]$Alpha))
    bayes.postproc_phylcor[[i]]$Sigma <- as.numeric(as.character(bayes.postproc_phylcor[[i]]$Sigma))
    
  }
} else{#Remote import
  library(htmltab)
  for(i in 1:length(BayesTraits.trees)){
    bayes.postproc[[i]] <- htmltab(paste(wd,'/BayesTraits_PostProcessing_Results/',myclade,'.pca.scores.pruned.postproc.',i,'.html', sep=""), which=1, header=1)
    # update progress bar
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  }
  close(pb)}

# #The postprocessing tool, provides the age of the time slices in a rather unusual format IF the ScaleTrees parameter is used
# #If the ScaleTrees parameter is NOT used (default setting), the SliceHeight column reports the height above the root
# #This makes calculating the slice age quite easy - but we try the alternative as well (as it might be needed if we were to use
# #the ScaleTrees command + it gives a nice way of checking, that the postprocessing tool is doing its job properly)
# #Instead of using the "SliceHeight" column, we could therefore just calculate the ages of the timeslices by ourselves
# #For this, we just need to calculate the total length of the trees from the root to the highest tip (or actually the
# #age of the highest tip) and then divide it by the no. of time slices we have chosen (e.g., 1000)
tmp_youngAge_Alternative <- list()
tmp_SliceAge_Alternative <- list()
# #Not needed anymore
#tmp_bayes.postproc_phylcor <- list() # #Create temporary "bayes.postproc_phylcor" (needed for merging dataframes)
for(i in 1:length(BayesTraits.trees)){
  tmp_youngAge_Alternative[[i]] <- BayesTraits.trees[[i]]$root.time - max(nodeHeights(BayesTraits.trees[[i]])) # #Calculate age of highest/youngest tip
  tmp_SliceAge_Alternative[[i]] <- data.frame(Slice = seq(from = 0, to = NoCats-1, by = 1), # #PostProcessing tool counts starting from "0" - therefore we need an appropriate index
                                              SliceAge_Alternative = seq(from = BayesTraits.trees[[i]]$root.time, to = tmp_youngAge_Alternative[[i]], length.out = NoCats)) # #Create sequence of
  # #time slices starting from the root to the age of the youngest/highest tip (no. of steps given by "NoCats" parameter)
  
  bayes.postproc_phylcor[[i]]$rowId <- attr(bayes.postproc_phylcor[[i]], "row.names") # #Create a row ID columm to restore the original
  # #order after merging the dataframes
  
  # #If the ScaleTrees parameter is NOT used, the height age of the time slices can be calculated directly
  # #Only the following line of code is then necessary
  bayes.postproc_phylcor[[i]]$SliceAge <- BayesTraits.trees[[i]]$root.time - bayes.postproc_phylcor[[i]]$SliceHeight # #Calculate slice age
  
  bayes.postproc_phylcor[[i]] <- merge(x = bayes.postproc_phylcor[[i]], y = tmp_SliceAge_Alternative[[i]], by = "Slice", all = FALSE) # #Add "SliceAge" column
  # #to "bayes.postproc_phylcor" object
  bayes.postproc_phylcor[[i]] <- bayes.postproc_phylcor[[i]][order(bayes.postproc_phylcor[[i]]$rowId), ] # #Restore original order of rows
  
  # #Note that the ages of the SliceAge column is slightly different from the ages of the SliceAge_Alternative column - this is because the
  # #ages of the Slice Age column do NOT start from the root age, but slightly later, while the ages of the SliceAge_Alternative column start
  # #from the root age (I guess the postprocessing tool is creating the time slices starting from the heighest tip of the tree instead of
  # #the root - but this needs to be tested)
}


# #Previously, we calculated phylogenetically corrected evolutionary rates through time here, but we can do this later on as well



#################################################################
# ####EXCLUDE BAYESTRAITS ANALYSES, WHICH HAVE NOT CONVERGED#####
# #Find trees for which BayesTraits analyses have not converged (i.e., where ESS < 200)
# #Note, that the current implementation DOES NOT ACCOUNT FOR local or whole tree transformations (kappa, lambda, delta)
# #Furthermore note, that the current implementation does not affect the logBF calculation, as the logBF is now calculated
# #before excluding some analyses - a quick way to obtain the logBF after excluding some analyses is just to rerun the logBF
# #calculation manually after having run the code below
# #Homogeneous rates
tmp_ess_hom_notconverged <- which(tmp_ess_hom_min<200, arr.ind=TRUE)
tmp_ess_hom_trees_exclude <- unique(tmp_ess_hom_notconverged[,"row"]) # #Find unique trees for which analyses have NOT converged
# #Variable rates
tmp_ess_notconverged <- which(tmp_ess_min<200, arr.ind=TRUE)
tmp_ess_trees_exclude <- unique(tmp_ess_notconverged[,"row"]) # #Find unique trees for which analyses have NOT converged

tmp_trees_exclude <- unique(c(tmp_ess_hom_trees_exclude,tmp_ess_trees_exclude)) # #Trees that need to be excluded
tmp_trees_exclude

# exclude_trees <- "YES"
# #Exclude trees (and associated data) which have not converged
if(exclude_trees == "YES"){
  all_BayesTraits.trees <- BayesTraits.trees # #Store all trees in separate object
  BayesTraits.trees <- BayesTraits.trees[-tmp_trees_exclude] # #Delete trees, which have not converged
  all_bayes.postproc <- bayes.postproc # #Store all data in separate object
  bayes.postproc <- bayes.postproc[-tmp_trees_exclude] # #Delete data, which has not converged
}
#######################################################


# #Convert class of first 21 columns from character to numeric
# #Column 22 represents the species names and is obviously not converted
# #Furthermore we convert the spaces in the column names to underscores "_" (to avoid problems, but may not be necessary)
cols = c(1:21)
for(i in 1:length(BayesTraits.trees)){
  bayes.postproc[[i]][,cols] <- apply(bayes.postproc[[i]][,cols], 2, function(x) as.numeric(as.character(x))) #Convert class
  colnames(bayes.postproc[[i]]) <- gsub(" ", "_", colnames(bayes.postproc[[i]])) #Replace spaces with underscores
  bayes.postproc[[i]]$Taxa_List <- sapply(strsplit(as.character(bayes.postproc[[i]]$Taxa_List), ","), "[") #Split reported strings/names, so that we can use them with getMRCA
  bayes.postproc[[i]]["NEW_Edge_ID"] <- NA #Create new edge ID column
  bayes.postproc[[i]]["NEW_Node_ID"] <- NA #Create new node ID column
}


# #Find new node and edge IDs of our data
# #An alternative to this approach might be postorder_nodes_phylo4_return_table (see http://blog.phytools.org/2012/01/function-to-get-descendant-node-numbers.html)
# #but I haven't really checked that function, so let's do it manually
# #it appears that the original Node_ID actually represents the edge IDs (except for the root edge, which is given as "0", while in R it usually has the highest edge number),
# #so it might not even necessary to go through all this effort (but it is nice, that it works anyway ;))
class(BayesTraits.trees) <- "multiPhylo" #we might want to do this right from the start, otherwise some other methods might not work
for(i in 1:length(BayesTraits.trees)){
  for(j in 1:length(bayes.postproc[[i]]$Taxa_List)){
    if(length(bayes.postproc[[i]]$Taxa_List[[j]]) == 1){
      #Single tips: find tip nodes and edges
      bayes.postproc[[i]]$NEW_Node_ID[[j]]  <- which(BayesTraits.trees[[i]]$tip.label == bayes.postproc[[i]]$Taxa_List[[j]]) #find tip nodes
      bayes.postproc[[i]]$NEW_Edge_ID[[j]] <- which.edge(BayesTraits.trees[[i]], bayes.postproc[[i]]$Taxa_List[[j]]) #find tip edges
    }  else{
      #Multiple tips: find internal nodes and edges of MRCA
      #Notice that the root edge is not assigned an edge ID, i.e. it stays NA
      bayes.postproc[[i]]$NEW_Node_ID[[j]]  <- getMRCA(BayesTraits.trees[[i]], bayes.postproc[[i]]$Taxa_List[[j]]) #find internal node
      bayes.postproc[[i]]$NEW_Edge_ID[[j]] <- which.edge(BayesTraits.trees[[i]], getMRCA(BayesTraits.trees[[i]], bayes.postproc[[i]]$Taxa_List[[j]])) #find internal edges
    }
  }
}

# #Check visually that edge and node IDs are correct
plot(BayesTraits.trees[[1]], cex=0.4)
nodelabels(frame="none", cex=0.5)
plot(BayesTraits.trees[[1]], cex=0.4)
edgelabels(frame="none", cex=0.5)



# #Replace root edge ID "NA" with length(bayes.postproc[[i]]$NEW_Edge_ID), i. e. root is added as the last edge element, NOT the first
# #The root edge is stored at the end of all other edge lengths in R - in BayesTraits it is treated as the first (= "0")
# #Furthermore we replace the rownames with the NEW_Edge_IDs and order the rows according to the rownumbers (= NEW_Edge_IDs)
# #This is necessary, as plotBranchbyTrait assigns the branch values (e.g. rates) to the edge lengths by going from the top
# #to the bottom of the column, i. e. the rows in our datafile HAVE to be exactly sorted as the edge lengths, otherwise the
# #branch values will be assigned to the wrong edges
for(i in 1:length(BayesTraits.trees)){
  bayes.postproc[[i]]$NEW_Edge_ID[is.na(bayes.postproc[[i]]$NEW_Edge_ID)] <- length(bayes.postproc[[i]]$NEW_Edge_ID) #Replace root "NA" with nr. of last edge element
  rownames(bayes.postproc[[i]]) <- bayes.postproc[[i]]$NEW_Edge_ID #Replace rownames with NEW_Edge_ID
  bayes.postproc[[i]] <- bayes.postproc[[i]][ order(as.numeric(row.names(bayes.postproc[[i]]))),] #Order rows according to rownumbers
}


# #We also create a new column "PPSelection" which records whether positive phenotypic selection occured or not
# #according to the criteria of Baker et al. (2016), i. e. a) delta_v/delta_b > 2 (= rate scalar > 2) and
# #b) delta_v/delta_b > 2 must be observed in more than 95% of the posterior distribution of rate scalars for the
# #branch in question.
# #Note, that Baker et al. (2016) used the individual rescaled trees for their magnitude comparison (C. Venditti, pers. comm., 2018)
# #Unfortunately the output of the PPPostProcessing tool does not provide the individual values, but only mode, mean and median
# #Using the mode should give very similar results to using the individual values (C. Venditti, pers. comm., 2018)
# #and is also computationally more feasible given that we run this analysis for 100 timescaled trees (otherwise we
# #would have to deal with 100*10,000 trees assuming that 10,000 trees were sampled from the posterior distribution reported
# #by BayesTraits)
for(i in 1:length(BayesTraits.trees)){
  # #OTUs that fit the criteria of positive phenotypic selection are assigned the value 2 (instead of 1)
  bayes.postproc[[i]][["PPSelection"]][bayes.postproc[[i]][["Mode_Scalar"]]>2 & bayes.postproc[[i]][["Pct_time_scaled"]]>95] <- 2
  # #All other OTUs, that do not fit this criterion are assigned the value 1 (instead of 0)
  # #We don't use the value 0, since it might cause issues when trying to calculate the global consensus tree (many branches
  # #would have lengths of 0)
  bayes.postproc[[i]][["PPSelection"]][is.na(bayes.postproc[[i]][["PPSelection"]])] <- 1
}




# #Select the variable rates parameter you are interested in (e.g., mean scalar)
myvarpar <- "Mean_Scalar" #we could also go for the median scalar ("Median_Scalar") or other parameters

# #Check visually that branch values are assigned to the correct edges
for(i in 1:length(BayesTraits.trees)){
  plotBranchbyTrait(BayesTraits.trees[[i]],bayes.postproc[[i]][[myvarpar]], mode="edges", palette="rainbow",
                    type="phylogram", show.tip.label=TRUE, show.node.label=TRUE, cex=0.2)#, use.edge.length=FALSE)
  edgelabels(round(bayes.postproc[[i]][[myvarpar]],6),cex=0.2, frame="none", col="red")
}


# #Previously we calculated logBF here, but we can do this earlier on (even before importing the postprocessed results)

#####END OF BAYESTRAITS ANALYSES######

###Plot heterogeneous rates with ggtree###
#edge=data.frame(compare.trees$trees[[1]]$edge, edge_value=ratio)
library("ggtree")

bayes.edge <- list()
for(i in 1:length(BayesTraits.trees)){
  bayes.edge[[i]]=data.frame(BayesTraits.trees[[i]]$edge,
                             #bayes.postproc[[i]][[myvarpar]][1:(length(bayes.postproc[[i]][[myvarpar]])-1)]) # #Old way (not including "PPSelection"): can be deleted
                             bayes.postproc[[i]][1:(length(bayes.postproc[[i]][[myvarpar]])-1),c(myvarpar, "Pct_time_scaled", "PPSelection")]) # #we don't include
  #the last row (= root edge), as it is not found in the trees (probably has to do with the way "ape" stores phylogenetic trees)
  colnames(bayes.edge[[i]])=c("parent", "node", "branch_ratio", "Pct_time_scaled", "PPSelection")
}




#Check: branch_ratio and ratio (e.g., mean scalar) are in the same order! If yes: Good!
for(i in 1:length(BayesTraits.trees)){
  if (all(bayes.edge[[i]]$branch_ratio == bayes.postproc[[i]][[myvarpar]][1:(length(bayes.postproc[[i]][[myvarpar]])-1)]) == TRUE){
    print("OK! 'branch_ratio' and 'ratio' are in the same order! Good!")
  }  else {
    print("Careful! 'branch_ratio' and 'ratio' are NOT IN THE SAME ORDER! DON'T CONTINUE PLOTTING AND CHECK WHAT'S GOING ON!")
  }
}

#Let's also check whether Pct_time_scaled and PPSelection are in the same order in both bayes.postproc and bayes.edge! If yes: Good!
for(i in 1:length(BayesTraits.trees)){
  if (all(bayes.edge[[i]]$Pct_time_scaled == bayes.postproc[[i]][["Pct_time_scaled"]][1:(length(bayes.postproc[[i]][["Pct_time_scaled"]])-1)]) == TRUE
      & all(bayes.edge[[i]]$PPSelection == bayes.postproc[[i]][["PPSelection"]][1:(length(bayes.postproc[[i]][["PPSelection"]])-1)]) == TRUE){
    print("OK! 'Pct_time_scaled' and 'PPSelection' are in the same order in both bayes.postproc and bayes.edge! Good!")
  }  else {
    print("Careful! 'Pct_time_scaled' and 'PPSelection' are NOT IN THE SAME ORDER IN bayes.postproc AND bayes.edge! DON'T CONTINUE PLOTTING AND CHECK WHAT'S GOING ON!")
  }
}


#2. Alternative to create species labels for ggtree
bayes.lb <- list()
bayes.lb.df <- list()
for(i in 1:length(BayesTraits.trees)){
  bayes.lb[[i]] = BayesTraits.trees[[i]]$tip.label #find species names/labels
  bayes.lb.df[[i]] = data.frame(label=bayes.lb[[i]], label2 = sub("_", " ", BayesTraits.trees[[i]]$tip.label)) #Make species names italic and remove underscore
}
#p <- p %<+% lb.df + geom_tiplab(aes(label = label2), size=3, fontface="italic")
#p

#Phylogram plot
bayes.p <- list()
for(i in 1:length(BayesTraits.trees)){
  bayes.p[[i]] <- ggtree(BayesTraits.trees[[i]], aes(color = branch_ratio))#, size = 0.5) #%<+% edge +
  bayes.p[[i]] <- bayes.p[[i]] %<+% bayes.edge[[i]] +
    xlim(0, max(nodeHeights(BayesTraits.trees[[i]]))+15) + #give enough space to plot all species names
    geom_treescale(offset=1.25) +
    scale_color_continuous(low='darkgreen', high='red', name= 'rel. rate') +
    #geom_label(aes(x=branch, label=round(branch_ratio, 6)), size=1) +
    ggtitle(paste(myclade_PalAss, " tree")) +
    theme(legend.position="right")# + theme_tree2(legend.position="right") #theme_tree2 provides scale similar to the geological timescale
  
  bayes.p[[i]] #without labels
}
bayes.p[[i]]

# #Sometimes you will encounter trees with ESS values > 200, but which have absurdly high rates (compared to the other trees)
# #Let's find these trees
# #Here i have chosen a rate of 500 as the threshold, but this is dataset dependent
absurd.high.branch.values.issue <- sapply(bayes.p, function(x){any(x[["data"]][["branch_ratio"]] > 200)})
which(absurd.high.branch.values.issue == TRUE) # #These trees have rates that are absurdly high - you probably want to rerun them
rerun_absurd <- which(absurd.high.branch.values.issue == TRUE) # #These trees have rates that are absurdly high - you probably want to rerun them
rerun_absurd

for(i in 1:length(BayesTraits.trees)){
  bayes.p[[i]] <- bayes.p[[i]] %<+% bayes.lb.df[[i]] + geom_tiplab(aes(label = label2), size=3, fontface="italic")
  bayes.p[[i]] #with labels
}
bayes.p[[i]]

#####Calculate a (global) consensus tree with averaged rate parameters (e.g., mean scalar) for ALL timescaled trees######
# #Assign original/unmodified timescaled trees to new tree object "BayesTraits.varRates.trees"
# #Replace edge lengths of new tree object with variable rates parameter of BayesTraits analysis (e.g., mean scalar)
# #This allows us to calculate a consensus tree (using phytools) of the resulting trees, thereby getting the mean
# #rate across a set of different timescaled trees
BayesTraits.varRates.trees <- BayesTraits.trees
for(i in 1:length(BayesTraits.trees)){
  BayesTraits.varRates.trees[[i]]$edge.length <- bayes.postproc[[i]][[myvarpar]][1:(length(bayes.postproc[[i]][[myvarpar]])-1)] #assign new edge length
}
# #we don't include the last row (= root edge), as it is not found in the trees (probably has to do with the way "ape" stores phylogenetic trees)
# #see also bayes.edge

plot(1, type="n", xlab="", ylab="", xlim = c(0,170), ylim=c(0,350))
for(i in 1:length(BayesTraits.varRates.trees)){
  lines(1:length(BayesTraits.varRates.trees[[1]]$edge.length), BayesTraits.varRates.trees[[i]]$edge.length, col=adjustcolor( "gray30", alpha.f = 0.1))
}

#BayesTraits.varRates.trees <- BayesTraits.varRates.trees[-c(tmp_ess_trees_exclude, rerun_absurd)]
#BayesTraits.trees <- BayesTraits.trees[-c(tmp_ess_trees_exclude, rerun_absurd)]

#consensus.BayesTraits.varRates.trees: consensus tree of the variable rates (rescaled) trees that we just created by assigning
#the respective parameters/rates, which were calculated by BayesTraits
#consensus.BayesTraits.trees: consensus tree of the input timescaled trees (i.e., this is just our starting input supertree,
#but with averaged branch lengths)
#we could use myclade.ts.trees.pruned instead of BayesTraits.trees (as they are the same, but the first one also includes node
#labels) as input --> unfortunately the consensus tree won't contain the original node labels anyway
# #For randomly resolved trees it does not make sense to calculate the mean.edge for p=0.50 (i. e. a 50% majority rule consensus tree),
# #and instead we can opt for a strict consensus tree - it can make sense, however, if you are dealing with MPTs or the
# #posterior of a Bayesian analysis
consensus.BayesTraits.varRates.tree <- consensus.edges(BayesTraits.varRates.trees,method="mean.edge",p=1.00,if.absent="ignore")
consensus.BayesTraits.tree <- consensus.edges(BayesTraits.trees,method="mean.edge",p=1.00,if.absent="ignore")

#Compare the two global consensus trees
all.equal.phylo(consensus.BayesTraits.varRates.tree,consensus.BayesTraits.tree,
                use.edge.length=FALSE,index.return=TRUE)
#Node IDs appear to be the same, but to be sure, I'll use cophylo anyway, to find the correspoding nodes
global.compare.BayesTraits.trees <- cophylo(consensus.BayesTraits.tree,consensus.BayesTraits.varRates.tree)
plot(global.compare.BayesTraits.trees)


#Check that 'edge IDs' are the same
if (all(global.compare.BayesTraits.trees$trees[[1]]$edge == global.compare.BayesTraits.trees$trees[[2]]$edge) == TRUE){
  print("OK! 'Edge IDs' of the two trees are identical!")
}  else {
  print("Careful! 'Edge IDs' are NOT IDENTICAL! Don't use edge lengths to plot rescaled branches!")
}


global.compare.BayesTraits.trees$trees[[2]]$edge.length #These branch lengths represent the mean of the rate/parameter (e.g. mean
# #scalar across our timescaled trees)
# #The branch lengths should be equal to these:
#consensus.BayesTraits.varRates.tree$edge.length (but here the root is included with a branch length of 0)

#Plot rates on timescaled tree
plotBranchbyTrait(global.compare.BayesTraits.trees$trees[[1]],global.compare.BayesTraits.trees$trees[[2]]$edge.length, mode="edges", palette="rainbow",
                  type="phylogram", show.tip.label=TRUE, show.node.label=TRUE, cex=0.2)#, use.edge.length=FALSE)
edgelabels(round(global.compare.BayesTraits.trees$trees[[2]]$edge.length,3),cex=0.4, frame="none", col="red")


library(colourvalues)
library(RColorBrewer)
spectrum <-colour_values(1:11, palette = "rdylbu")
colfunc <- colorRampPalette(c(spectrum))
palette=colorRampPalette(c("blue", "orange", "#ff6f00","red"))

# # now make plot with time tree and branches with rate colour
plotBranchbyTrait(global.compare.BayesTraits.trees$trees[[1]],global.compare.BayesTraits.trees$trees[[2]]$edge.length, mode="edges", palette=colorRampPalette(rev(c(spectrum))),
                  type="phylogram", show.tip.label=TRUE, show.node.label=TRUE, cex=0.5, edge.width = 1.5)

plotBranchbyTrait(global.compare.BayesTraits.trees$trees[[1]],global.compare.BayesTraits.trees$trees[[2]]$edge.length, mode="edges", palette=colorRampPalette(c("blue", "orange", "#ff6f00","red")),
                  type="phylogram", show.tip.label=TRUE, show.node.label=TRUE, cex=0.5, edge.width = 1.5)



dev.copy(pdf,'FINAL_RATES_######_MAIN_PAPER.pdf', width = 7, height = 6)
dev.off()

