library(ape)
library(maps)
library(phytools)
library(paleotree)
library(geoscale)
library(strap)
library(geiger)


# Clearing workspace
rm(list=ls() ) 


#Load the tree from a .tre file
#-------------------------------#
tree<-read.tree(file="Sauropterygia.tre")
tree

plotTree(tree,ftype="i",
         fsize=0.6,
         lwd=1)

class(tree)

tree$edge.length #this will say NULL if the tree doesn't have branch lengths


#Load data for tree
#-----------------#

my.data <- read.csv("Sauropterygians_trunk_and_neck.csv", 
                    header=T, 
                    row.names = 1)
my.data

#It is advised to have the taxa in the tree and in the data file in same order
#this code checks if order is the same
tree$tip.label
rownames(my.data)
identical(tree$tip.label,rownames(my.data))

#If output is ‘FALSE’ then rearrange the rows of the data frame to match the order of tip 

#Load data
my.data <- my.data[match(tree$tip.label,rownames(my.data)),]
my.data

#now the code to check identical tips and names should output ‘TRUE’
identical(tree$tip.label,rownames(my.data))

#Add geological time data
my.geological.data <- my.data[,c('FAD','LAD')]
my.geological.data


#DATING THE PHYLOGENETIC TREE
#---------------------------#

#2-Dating using "minMax"(a variant of the "equal" method that selects a random single age between FAD and LAD)
#-------------------------------------------------------------------------------------#
my.equal.timetree.minMax <-timePaleoPhy(tree, my.geological.data,
                                        type="equal", 
                                        vartime=2, 
                                        ntrees=1, 
                                        dateTreatment="minMax", 
                                        plot=TRUE)

my.equal.timetree.minMax

plotTree(my.equal.timetree.minMax,ftype="i",
         fsize=0.6,
         lwd=1)


write.tree(my.equal.timetree.minMax, file="SauropterygianMinMax.tre")
write.nexus(my.equal.timetree.minMax, file="SauropterygianMinMax.nex")



#the warning is just a reminder that this method selects a single sample date
#Therefore, by running the analysis multiple times, results will be slightly different

my.equal.timetree.minMax$root.time


#########################
#########################


# ANCESTRAL STATE RECONSTRUCTION (function fastAnc, phytools)
#---------------------------------------------------------------#


#Extract TRUNK length data and transform it into a vector
my.trunk.data <- as.vector(my.data[,c('Trunk')])  #Add or not log before (my.data to long-transform
names(my.trunk.data) <- rownames(my.data) # add names to the vector of sizes
my.trunk.data

#simple barplot TRUNK 
plotTree.barplot(tree, my.trunk.data, 
                 args.barplot=list(xlab="Trunk length (cm)", 
                                   col="gray"))

#######################


#Extract NECK RATIO data and transform it into a vector
my.neck.data <- as.vector(my.data[,c('Neck_ratio')])
names(my.neck.data) <- rownames(my.data) # add names to the vector of sizes
my.neck.data


#simple barplot NECK RATIO 
plotTree.barplot(tree, my.neck.data, 
                 args.barplot=list(xlab="Neck ratio", 
                                   col="gray"))



#TRUNK 
#-----#
ace.fit.trunk <- fastAnc(my.equal.timetree.minMax, 
                         my.trunk.data, 
                         vars=TRUE, 
                         CI=TRUE)

print(ace.fit.trunk)


#NECK RATIO
#----------#
ace.fit.neckR <- fastAnc(my.equal.timetree.minMax, 
                         my.neck.data, 
                         vars=TRUE, 
                         CI=TRUE)
print(ace.fit.neckR)



#Visualise nodes
#----------------
plot_tree <- ladderize(my.equal.timetree.minMax) # select tree

plot(plot_tree,
     cex=0.4)

nodelabels(cex=0.5)
tiplabels(cex=0.5)


#Phenograms with coloured clades 
#http://blog.phytools.org/2012/05/painting-different-clades-with.html


#TRUNK PHENOGRAM (Figure 5b)
#----------------------------#
plot_tree <- ladderize(my.equal.timetree.minMax) # select tree
plot(plot_tree)

plot_tree<-paintSubTree(plot_tree,node=148,state="2") #Elasmosauridae
plot_tree<-paintSubTree(plot_tree,node=126,state="3") #Thalassophonea
plot_tree<-paintSubTree(plot_tree,node=145,state="4") #Polycotilidae
plot_tree<-paintSubTree(plot_tree,node=132,state="5") #Plesiosauridae
plot_tree<-paintSubTree(plot_tree,node=115,state="6") #Rhomaleosauridae


cols<-c("grey50","#609040","#FF9933","#CC5500","#d1a3ff","#AE80FF"); names(cols)<-1:6
plotSimmap(plot_tree,
           cols,
           pts=F,
           lwd=1,
           fsize=0,
           node.numbers=T)

plot_data <-my.trunk.data  # select data

trait <- plot_data
traitname <- "Trunk (cm)"


plot.new()
treeheight<-max(vcv(plot_tree))
y <- c(0,(max(plot_data)))
# y <- c(round(min(crop.data[, plot_metric2])-(0.1*max(crop.data[, plot_metric2]))):round(max(crop.data[, plot_metric2])+(0.1*max(crop.data[, plot_metric2]))))
x <-seq(0,treeheight,treeheight)
#line1<-plot_tree$root.time-250  #for some reason root time wasn`t stored. I read it directly: 249,7278
line1<-249.7278-250
line2<-249.7278-200
line3<-249.7278-150
line4<-249.7278-100
xat<-c(line1, line2, line3, line4)
xat
xlabels<-c("250","200","150", "100")
xlabels

par(mai=c(0.7,0.7,0.2,0.2), #numerical vector c(bottom, left, top, right) which gives the margin size specified in inches
    mgp=c(2.2,0.5,0),       #margin line (in mex units) for the axis title, axis labels and axis line
    las=1)                  #numeric in {0,1,2,3}; the style of axis labels   
plot(x,y,
     type="n",
     axes=FALSE, 
     ylab=traitname, 
     xlab="Mya", 
     cex.lab=1.2)
#abline(v=c(pbound, jbound,cbound),col="grey",lwd=1.5,lty=2)     #this line gives an error, what are pbound, jbound and cbound used for??
axis(2, cex.axis=1,lwd=0.6)
axis(side = 1, at = xat, labels = xlabels, tck=-.02, cex.lab=1,cex.axis=1,lwd=0.6)
box(lwd=0.6)

try <- phenogram(plot_tree, 
                 trait,
                 fsize=0,
                 lwd=1,
                 spread.labels=F, 
                 colors=cols,   #how to apply here the colors representing the evolutionary rates?
                 add=TRUE)


dev.copy(pdf,'Phenogram-trunk-colors.pdf', width = 4.8, height = 3.2)
dev.off()


###############################

#NECK RATIO PHENOGRAM (Figure 5c)
#----------------------------#
plot_tree <- ladderize(my.equal.timetree.minMax) # select tree
plot(plot_tree)


plot_tree<-paintSubTree(plot_tree,node=148,state="2") #Elasmosauridae
plot_tree<-paintSubTree(plot_tree,node=126,state="3") #Thalassophonea
plot_tree<-paintSubTree(plot_tree,node=145,state="4") #Polycotilidae
plot_tree<-paintSubTree(plot_tree,node=132,state="5") #Plesiosauridae
plot_tree<-paintSubTree(plot_tree,node=115,state="6") #Rhomaleosauridae

cols<-c("grey50","#609040","#FF9933","#CC5500","#d1a3ff","#AE80FF"); names(cols)<-1:6
plotSimmap(plot_tree,
           cols,
           pts=F,
           lwd=1,
           fsize=0,
           node.numbers=T)

plot_data <-my.neck.data  # select data

trait <- plot_data
traitname <- "Neck ratio"


plot.new()
treeheight<-max(vcv(plot_tree))
y <- c(0,(max(plot_data)))
# y <- c(round(min(crop.data[, plot_metric2])-(0.1*max(crop.data[, plot_metric2]))):round(max(crop.data[, plot_metric2])+(0.1*max(crop.data[, plot_metric2]))))
x <-seq(0,treeheight,treeheight)
#line1<-plot_tree$root.time-250  #for some reason root time wasn`t stored. I read it directly: 249,7278
line1<-249.7278-250
line2<-249.7278-200
line3<-249.7278-150
line4<-249.7278-100
xat<-c(line1, line2, line3, line4)
xlabels<-c("250","200","150", "100")

par(mai=c(0.7,0.7,0.2,0.2), #numerical vector c(bottom, left, top, right) which gives the margin size specified in inches
    mgp=c(2.2,0.5,0),       #margin line (in mex units) for the axis title, axis labels and axis line
    las=1)                  #numeric in {0,1,2,3}; the style of axis labels   
plot(x,y,
     type="n",
     axes=FALSE, 
     ylab=traitname, 
     xlab="Mya", 
     cex.lab=1.2)
#abline(v=c(pbound, jbound,cbound),col="grey",lwd=1.5,lty=2)     #this line gives an error, what are pbound, jbound and cbound used for??
axis(2, cex.axis=1,lwd=0.6)
axis(side = 1, at = xat, labels = xlabels, tck=-.02, cex.lab=1,cex.axis=1,lwd=0.6)
box(lwd=0.6)

try <- phenogram(plot_tree, 
                 trait,
                 fsize=0,
                 spread.labels=F, 
                 lwd=1,
                 colors=cols,   #indicate "grey50" if all branches are same color
                 add=TRUE)


dev.copy(pdf,'Phenogram-neck-colors.pdf', width = 4.8, height = 3.2)
dev.off()




