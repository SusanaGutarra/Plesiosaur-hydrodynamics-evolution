# Clearing workspace
rm(list=ls() ) 

library(ggplot2)
library(plyr) #for hulls
library(ggExtra)



#### Reading in a data file ####
dframe1<-read.csv("Sauropterygians_trunk_and_neck.csv", header = T)
dframe1


#Create a subset of the data with the group categories that to plot
dframe1<- subset(dframe1,Group %in% c("Eosauropterygia_basal","Placodontia","Pachypleurosauria","Nothosauroidea","Pistosauroidea","Rhomaleosauridae","Plesiosauridae","Pliosauridae_basal","Thalassophonea", "Microcleididae","Cryptoclididae","Leptocleididae","Polycotilidae","Elasmosauridae"))
dframe1


#Create a factor and indicate the order (by default, the categorical variable is plotted in alphabetical order!)
Group1 =factor(dframe1$Group, 
               levels = c("Eosauropterygia_basal","Placodontia","Pachypleurosauria","Nothosauroidea","Pistosauroidea","Rhomaleosauridae","Plesiosauridae","Pliosauridae_basal","Thalassophonea", "Microcleididae","Cryptoclididae","Leptocleididae","Polycotilidae","Elasmosauridae"),
               labels = c("Basal eosauropterygia","Placodontia","Pachypleurosauria","Nothosauroidea","Pistosauroidea","Rhomaleosauridae","Plesiosauridae","Basal pliosauridae","Thalassophonea","Microcleididae","Cryptoclididae","Leptocleididae","Polycotilidae","Elasmosauridae"))

Group1

# TRUNK VERSUS NECK (FIGURE 5.a)
#-------------------------------#

# Calculate the hulls for each group
find_hull <- function(df) df[chull(df$Trunk, df$Neck), ]
hulls <- ddply(dframe1, .(Group1), find_hull)

 
#Make plots with dots and hulls
p1 <- ggplot(dframe1, aes(x = Trunk, y = Neck_ratio, fill = Group1, colour=Group1, shape=Group1)) + #I need to specify color here so that ggMarginal can take it
  geom_point(size=2.5, stroke=0.3) +
  geom_polygon(data = hulls, size = 0, alpha = 0.2)+      #0 is the size of the polygon line
  scale_color_manual(values = c("#01665e","#80cdc1","#c7eae5","#b2d4eb","#80eaff","#d8c2ff","#AD80FF","#ffd8b0","#FF9833","#EEEEEE","#CCCCCC","#999999","#CB5500","#6da348"))+
  scale_fill_manual(values =c("#01665e","#80cdc1","#c7eae5","#b2d4eb","#80eaff", "#d8c2ff","#AD80FF","#ffd8b0","#FF9833","#EEEEEE","#CCCCCC","#999999","#CB5500","#6da348"))+
  #scale_color_manual(values = c("#01665e","#80cdc1","#c7eae5","#b2d4eb","#80eaff","#d8c2ff","#AD80FF","#FF9833","#8c510a","red","darkgrey","#CB5500","#6da348"))+
  #scale_fill_manual(values =c("#01665e","#80cdc1","#c7eae5","#b2d4eb","#80eaff", "#d8c2ff","#AD80FF","#FF9833","#8c510a","red","darkgrey","#CB5500","#6da348"))+
  scale_shape_manual(values= c(21,21,21,21,24,24,24,22,22,23,23,23,23,23))+
  labs(x = "Trunk (cm)",
       y = "Neck ratio")+
  theme_light()

p1

#Add another layer of dots, so that the rim is black
p2 <- p1 + geom_point(color="black",size = 2.5, stroke=0.3) 

p2

#Format:
p3<-p2 + theme (#text=element_text(family="Arial"),
            axis.text.x = element_text(size=12, color="#303030", hjust=0.5),
            axis.text.y = element_text(size=12,color="#303030"),
            axis.title.x  = element_text(size=14,color="#303030", margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y  = element_text(size=14,color="#303030", margin = margin(t = 0, r = 10, b = 0, l = 0)),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks = element_line(colour = "#303030"),
            panel.border = element_rect(color="#303030", fill= NA, size=0.6),
            aspect.ratio = 1,
            #legend.title = element_blank(),
            legend.text = element_text(colour="#303030", size=3),
            legend.position = "none")

p3

#Save to double size, then scale to 6 cm
ggsave("Sauropterygian Trunk_vs_Neck_Ratio.pdf", plot = last_plot(), device = "pdf", path = NULL,
       scale = 1, width = 12, height = 12, units = "cm",
       dpi = 300, limitsize = TRUE) 


##########################

#Add marginal plots with ggMarginal (ggplot2-based package)
#---------------------------------------------------------#

#https://cran.r-project.org/web/packages/ggExtra/vignettes/ggExtra.html
#https://cran.r-project.org/web/packages/ggExtra/readme/README.html


#This function uses the plot as an argument (not the dataframe)


ggMarginal(p3, 
           colour="#303030",
           groupFill =  TRUE, #The aesthetics feed from the ones in the plot, could also take color of line: groupColor = TRUE
           type = "boxplot",               #here we can also use boxplot, histogram, density...
           size = 3,                      #control size of marginal plots compared to the main plot   
           margins = c("x"),
           xparams = list(size = 0.3),    #adjust parameters of the x - marginal plot. e.g: size controls line thickness
           yparams = list(size = 0.3))    #adjust parameters of the y - marginal plot  e.g: size controls line thickness                               


#Save directly from graph Export: size 5.5 x 6, then scale to 7 cm

    © 2021 GitHub, Inc.
    Terms
    Privacy
    Security
    Status
    Docs

    Contact GitHub
    Pricing
    API
    Training
    Blog
    About


