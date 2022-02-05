# #Different functions
# #Author: Armin Elsler
# #Please cite the associated publication when using this script or parts of it


# ####Plotting####
# library(ggtree)

# #Function to prepare plots for Powerpoint presentations with black background
# #Creates a transparent background
ppt.transparent <- function(myggtree.object) myggtree.object + theme(
  #rect = element_rect(fill = "transparent") # bg of the panel
  panel.background = element_blank() # get rid of bg of the panel
  #panel.background = element_rect(fill = "transparent") # bg of the panel
  , plot.title = element_text(colour = 'white') # make panel title white
  , plot.background = element_blank() # get rid of bg of the plot
  #, plot.background = element_rect(fill = "transparent") # bg of the plot
  #, panel.grid.major = element_blank() # get rid of major grid
  #, panel.grid.minor = element_blank() # get rid of minor grid
  , legend.background = element_blank() # get rid of legend bg
  #, legend.background = element_rect(fill = "transparent") # get rid of legend bg
  #, legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  , legend.title = element_text(colour = 'white') # Change colour of legend text to white
  , legend.text = element_text(colour = 'white') # Change colour of legend text to white
  , strip.background = element_blank() # get rid of bg of panel title
  , strip.text = element_text(colour = 'white') # Change colour of panel titles to white
  #, strip.background = element_rect(fill='blue', colour='white') # modify bg of panel title
)



