# Randomisation tests on evolutionary rates #
# ========================================= #

# required libraries
library(ape)
library(magrittr)

# This randomisation test uses the difference between the mean rates on branches
# inside and outside the clade of interest. The steps are as follows:
#
# 1. Get the number of branches in the clade of interest (COI).
# 2. Randomly sample rates on that number of branches from the whole tree
#    (without replacement).
# 3. Calculate difference in means between sampled branches and non-sampled
#    branches.
# 4. Repeat steps 2–3 until reaching 9999 replicates.
# 5. Calculate true difference between mean rates of branches inside COI and
#    outside COI.
# 6. Find ranking of results from step 5 in replicates from step 4 to give
#    p-value (dividing by 10,000 comparisons).


#PART 1: Tree 1 - Tree 6

# 1. Assemble data #
# ---------------- #

# gather trees and rates into a list
data <- list(Tree1 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.1.nex"),
                              rates = read.table("Sauropterygia.pca.scores.pruned.postproc.1.txt",
                                                 sep = "\t",
                                                 header = TRUE,
                                                 row.names = 1)),
             Tree2 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.2.nex"),
                              rates = read.table("Sauropterygia.pca.scores.pruned.postproc.2.txt",
                                                 sep = "\t",
                                                 header = TRUE,
                                                 row.names = 1)),
             Tree3   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.3.nex"),
                              rates = read.table("Sauropterygia.pca.scores.pruned.postproc.3.txt",
                                                 sep = "\t",
                                                 header = TRUE,
                                                 row.names = 1)),
             Tree4   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.4.nex"),
                              rates = read.table("Sauropterygia.pca.scores.pruned.postproc.4.txt",
                                                 sep = "\t",
                                                 header = TRUE,
                                                 row.names = 1)),
             Tree5   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.5.nex"),
                              rates = read.table("Sauropterygia.pca.scores.pruned.postproc.5.txt",
                                                 sep = "\t",
                                                 header = TRUE,
                                                 row.names = 1)),
             Tree6   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.6.nex"),
                              rates = read.table("Sauropterygia.pca.scores.pruned.postproc.6.txt",
                                                 sep = "\t",
                                                 header = TRUE,
                                                 row.names = 1)))
             
             
             
# remove first (blank) row of rates tables
# this is from the root branch
data <- lapply(data, function (run) {
          # remove row from rates table
          tbl <- run$rates[-1, ]

          # return data
          return(list(tree  = run$tree,
                      rates = tbl))
})


# 2. Identify taxa in relevant clades #
# ----------------------------------- #

# Elasmosauridae
elasmo_tax <- c("Albertonectes_vanderveldei","Aristonectes_quiriquinensis",
                "Callawayasaurus_colombiensis","Elasmosaurus_platyurus",
                "Hydrotherosaurus_alexandrae","Kaiwhekea_katiki",
                "Libonectes_atlasense","Morenosaurus_stocki",
                "Styxosaurus_sp","Thalassomedon_haningtoni")

# Policotylidae
poli_tax <- c("Dolichorhynchops_osborni","Manemergus_anguirostris",
              "Polycotylus_latipinnis","Trinacromerum_bentonianum")

# Thalassophonea
thal_tax <- c("Kronosaurus_boyacensis",
              "Kronosaurus_queenslandicus","Liopleurodon_ferox",
              "Luskhan_itilensis","Peloneustes_philarchus",
              "Sachicasaurus_vitae")


# get relevant banches = rows in rates tables
data <- lapply(data, function (run) {
          # get edge numbers and return data
          return(c(run,
                   list(elasmo_edge = which.edge(run$tree, elasmo_tax),
                        poli_edge  = which.edge(run$tree, poli_tax),
                        thal_edge  = which.edge(run$tree, thal_tax))))
})


# 3. Randomisation test #
# --------------------- #

samp_mean <- function (size, data) {
  # Performs a randomisation test on a set of data including a clade which is
  # a subset. Random samples the data without replacement to the size of the
  # comparison clade. This can be incorported into the replicate funciton.
  #
  # Args:
  #   size: the number of rows to include in the sample.
  #   data:  a vector or column from a matrix of data.frame that includes the
  #          original data; must be large than size. This is the data to be
  #          sampled.
  #
  # Returns:
  #   Means of the resampled data.
  # get rows to sample or same size as clade
  row_n <- sample(seq_along(data), size, replace = FALSE)

  # calculate difference in means within and without tha sample rows
  diff_mean <- mean(data[row_n]) - mean(data[-row_n])
}

# randomly sample data and compare to each clade
samp <- lapply(data, function (run) {
          # calculate mean of clades
          mean_rates <- list(elasmo = mean(run$rates[run$elasmo_edge, "Mean.Scalar"]) -
                                    mean(run$rates[-run$elasmo_edge, "Mean.Scalar"]),
                             poli  = mean(run$rates[run$poli_edge, "Mean.Scalar"]) -
                                    mean(run$rates[-run$poli_edge, "Mean.Scalar"]),
                             thal  = mean(run$rates[run$thal_edge, "Mean.Scalar"]) -
                                    mean(run$rates[-run$thal_edge, "Mean.Scalar"]))

          # replicate samples
          reps <- list(elasmo = replicate(9999, samp_mean(length(run$elasmo_edge),
                                                        run$rates$Mean.Scalar)),
                       poli  = replicate(9999, samp_mean(length(run$poli_edge),
                                                        run$rates$Mean.Scalar)),
                       thal  = replicate(9999, samp_mean(length(run$thal_edge),
                                                        run$rates$Mean.Scalar)))

          # get differences in means
          mn_pos <- list(elasmo = length(which(abs(reps$elasmo) > abs(mean_rates$elasmo))),
                         poli  = length(which(abs(reps$poli) > abs(mean_rates$poli))),
                         thal  = length(which(abs(reps$thal) > abs(mean_rates$thal))))

          # return data
          return(list(elasmo = list(emp  = mean_rates$elasmo,
                                  rand = reps$elasmo,
                                  pos  = mn_pos$elasmo),
                      poli  = list(emp  = mean_rates$poli,
                                  rand = reps$poli,
                                  pos  = mn_pos$poli),
                      thal  = list(emp  = mean_rates$thal,
                                  rand = reps$thal,
                                  pos  = mn_pos$thal)))
        })


# 4. Plot data #
# ------------ #

{
  # plot histograms
  cairo_pdf("randomisation_hist_Neck_1-6.pdf",
            width  = 10,
            height = 14) #original 14

  # set up plot area
  par(mfrow = c(6, 3),   #original 6, 3
      mar   = c(2, 1, 1, 2),
      oma   = c(4, 4, 5, 2))

  # begin plotting
  for (run in samp) {
    # Elasmosauridae
    hist(run$elasmo$rand, xlim = range(c(run$elasmo$rand, run$elasmo$emp)),
         main = NULL)
    abline(v = run$elasmo$emp, lwd = 2, col = "red")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Elasmosauridae", 3, line = 1, font = 2)

    # add axis label
    mtext("Frequency", 2, line = 3)
    
    # Policotylidae
    hist(run$poli$rand, xlim = range(c(run$poli$rand, run$poli$emp)),
         main = NULL)
    abline(v = run$poli$emp, lwd = 2, col = "blue")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Policotylidae", 3, line = 1, font = 2)

    # Thalassophonea
    hist(run$thal$rand, xlim = range(c(run$thal$rand, run$thal$emp)),
         main = NULL)
    abline(v = run$thal$emp, lwd = 2, col = "green")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Thalassophonea", 3, line = 1, font = 2)

    # add row label
    labels <- c("Tree 1", "Tree 2",
                "Tree 3", "Tree 4",
                "Tree 5", "Tree 6")
    nrun <- 6 - (par()$fig[3] * 6)
    mtext(labels[nrun], 4, line = 2, font = 2)
  }

  # x axis title
  mtext("Difference between sampled means", 1, outer = TRUE, line = 1)

  # figure title
  mtext("Randomisation tests for time scaling methods", 3,
        outer = TRUE, cex = 1.5, font = 2, line = 3)

  # stop plotting
  dev.off()
}


# 5. Assemble table #
# ----------------- #

# assemble table of p-values from randomisation tests
# these are number of random samples with great difference in mean + 1 to find
# position of empirical data / 10000 to give p-value
tbl <- lapply(samp, function (run) {
         # assemble position
         data.frame(Elasmosauridae = (run$elasmo$pos + 1) / 10000,
                    Polocotylidae  = (run$poli$pos + 1) / 10000,
                    Thalassophonea      = (run$thal$pos + 1) / 10000)
       }) %>%
       do.call(rbind, .)

# replace rownames with relevant test
rownames(tbl) <- c("Tree 1", "Tree 2",
                   "Tree 3", "Tree 4",
                   "Tree 5", "Tree 6")

# write table to csv file
write.csv(tbl, file = "randomisation_pval_Neck_1-6.csv")


##############################
##############################


#PART 2: Tree 7 - Tree 12

# 1. Assemble data #
# ---------------- #

# gather trees and rates into a list
data <- list(Tree1 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.7.nex"),
                          rates = read.table("Sauropterygia.pca.scores.pruned.postproc.7.txt",
                                             sep = "\t",
                                             header = TRUE,
                                             row.names = 1)),
             Tree2 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.8.nex"),
                          rates = read.table("Sauropterygia.pca.scores.pruned.postproc.8.txt",
                                             sep = "\t",
                                             header = TRUE,
                                             row.names = 1)),
             Tree3   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.9.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.9.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)),
             Tree4   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.10.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.10.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)),
             Tree5   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.11.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.11.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)),
             Tree6   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.12.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.12.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)))



# remove first (blank) row of rates tables
# this is from the root branch
data <- lapply(data, function (run) {
  # remove row from rates table
  tbl <- run$rates[-1, ]
  
  # return data
  return(list(tree  = run$tree,
              rates = tbl))
})


# 2. Identify taxa in relevant clades #
# ----------------------------------- #

# Elasmosauridae
elasmo_tax <- c("Albertonectes_vanderveldei","Aristonectes_quiriquinensis",
                "Callawayasaurus_colombiensis","Elasmosaurus_platyurus",
                "Hydrotherosaurus_alexandrae","Kaiwhekea_katiki",
                "Libonectes_atlasense","Morenosaurus_stocki",
                "Styxosaurus_sp","Thalassomedon_haningtoni")

# Policotylidae
poli_tax <- c("Dolichorhynchops_osborni","Manemergus_anguirostris",
              "Polycotylus_latipinnis","Trinacromerum_bentonianum")

# Thalassophonea
thal_tax <- c("Kronosaurus_boyacensis",
              "Kronosaurus_queenslandicus","Liopleurodon_ferox",
              "Luskhan_itilensis","Peloneustes_philarchus",
              "Sachicasaurus_vitae")


# get relevant banches = rows in rates tables
data <- lapply(data, function (run) {
  # get edge numbers and return data
  return(c(run,
           list(elasmo_edge = which.edge(run$tree, elasmo_tax),
                poli_edge  = which.edge(run$tree, poli_tax),
                thal_edge  = which.edge(run$tree, thal_tax))))
})


# 3. Randomisation test #
# --------------------- #

samp_mean <- function (size, data) {
  # Performs a randomisation test on a set of data including a clade which is
  # a subset. Random samples the data without replacement to the size of the
  # comparison clade. This can be incorported into the replicate funciton.
  #
  # Args:
  #   size: the number of rows to include in the sample.
  #   data:  a vector or column from a matrix of data.frame that includes the
  #          original data; must be large than size. This is the data to be
  #          sampled.
  #
  # Returns:
  #   Means of the resampled data.
  # get rows to sample or same size as clade
  row_n <- sample(seq_along(data), size, replace = FALSE)
  
  # calculate difference in means within and without tha sample rows
  diff_mean <- mean(data[row_n]) - mean(data[-row_n])
}

# randomly sample data and compare to each clade
samp <- lapply(data, function (run) {
  # calculate mean of clades
  mean_rates <- list(elasmo = mean(run$rates[run$elasmo_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$elasmo_edge, "Mean.Scalar"]),
                     poli  = mean(run$rates[run$poli_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$poli_edge, "Mean.Scalar"]),
                     thal  = mean(run$rates[run$thal_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$thal_edge, "Mean.Scalar"]))
  
  # replicate samples
  reps <- list(elasmo = replicate(9999, samp_mean(length(run$elasmo_edge),
                                                  run$rates$Mean.Scalar)),
               poli  = replicate(9999, samp_mean(length(run$poli_edge),
                                                 run$rates$Mean.Scalar)),
               thal  = replicate(9999, samp_mean(length(run$thal_edge),
                                                 run$rates$Mean.Scalar)))
  
  # get differences in means
  mn_pos <- list(elasmo = length(which(abs(reps$elasmo) > abs(mean_rates$elasmo))),
                 poli  = length(which(abs(reps$poli) > abs(mean_rates$poli))),
                 thal  = length(which(abs(reps$thal) > abs(mean_rates$thal))))
  
  # return data
  return(list(elasmo = list(emp  = mean_rates$elasmo,
                            rand = reps$elasmo,
                            pos  = mn_pos$elasmo),
              poli  = list(emp  = mean_rates$poli,
                           rand = reps$poli,
                           pos  = mn_pos$poli),
              thal  = list(emp  = mean_rates$thal,
                           rand = reps$thal,
                           pos  = mn_pos$thal)))
})


# 4. Plot data #
# ------------ #

{
  # plot histograms
  cairo_pdf("randomisation_hist_Neck_7-12.pdf",
            width  = 10,
            height = 14) #original 14
  
  # set up plot area
  par(mfrow = c(6, 3),   #original 6, 3
      mar   = c(2, 1, 1, 2),
      oma   = c(4, 4, 5, 2))
  
  # begin plotting
  for (run in samp) {
    # Elasmosauridae
    hist(run$elasmo$rand, xlim = range(c(run$elasmo$rand, run$elasmo$emp)),
         main = NULL)
    abline(v = run$elasmo$emp, lwd = 2, col = "red")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Elasmosauridae", 3, line = 1, font = 2)
    
    # add axis label
    mtext("Frequency", 2, line = 3)
    
    # Policotylidae
    hist(run$poli$rand, xlim = range(c(run$poli$rand, run$poli$emp)),
         main = NULL)
    abline(v = run$poli$emp, lwd = 2, col = "blue")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Policotylidae", 3, line = 1, font = 2)
    
    # Thalassophonea
    hist(run$thal$rand, xlim = range(c(run$thal$rand, run$thal$emp)),
         main = NULL)
    abline(v = run$thal$emp, lwd = 2, col = "green")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Thalassophonea", 3, line = 1, font = 2)
    
    # add row label
    labels <- c("Tree 7", "Tree 8",
                "Tree 9", "Tree 10",
                "Tree 11", "Tree 12")
    nrun <- 6 - (par()$fig[3] * 6)
    mtext(labels[nrun], 4, line = 2, font = 2)
  }
  
  # x axis title
  mtext("Difference between sampled means", 1, outer = TRUE, line = 1)
  
  # figure title
  mtext("Randomisation tests for time scaling methods", 3,
        outer = TRUE, cex = 1.5, font = 2, line = 3)
  
  # stop plotting
  dev.off()
}


# 5. Assemble table #
# ----------------- #

# assemble table of p-values from randomisation tests
# these are number of random samples with great difference in mean + 1 to find
# position of empirical data / 10000 to give p-value
tbl <- lapply(samp, function (run) {
  # assemble position
  data.frame(Elasmosauridae = (run$elasmo$pos + 1) / 10000,
             Polocotylidae  = (run$poli$pos + 1) / 10000,
             Thalassophonea      = (run$thal$pos + 1) / 10000)
}) %>%
  do.call(rbind, .)

# replace rownames with relevant test
rownames(tbl) <- c("Tree 7", "Tree 8",
                   "Tree 9", "Tree 10",
                   "Tree 11", "Tree 12")

# write table to csv file
write.csv(tbl, file = "randomisation_pval_Neck_7-12.csv")


##############################
##############################

#PART 3: Tree 13 - Tree 18

# 1. Assemble data #
# ---------------- #

# gather trees and rates into a list
data <- list(Tree1 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.13.nex"),
                          rates = read.table("Sauropterygia.pca.scores.pruned.postproc.13.txt",
                                             sep = "\t",
                                             header = TRUE,
                                             row.names = 1)),
             Tree2 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.14.nex"),
                          rates = read.table("Sauropterygia.pca.scores.pruned.postproc.14.txt",
                                             sep = "\t",
                                             header = TRUE,
                                             row.names = 1)),
             Tree3   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.15.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.15.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)),
             Tree4   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.16.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.16.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)),
             Tree5   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.17.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.17.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)),
             Tree6   = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.18.nex"),
                            rates = read.table("Sauropterygia.pca.scores.pruned.postproc.18.txt",
                                               sep = "\t",
                                               header = TRUE,
                                               row.names = 1)))           


# remove first (blank) row of rates tables
# this is from the root branch
data <- lapply(data, function (run) {
  # remove row from rates table
  tbl <- run$rates[-1, ]
  
  # return data
  return(list(tree  = run$tree,
              rates = tbl))
})


# 2. Identify taxa in relevant clades #
# ----------------------------------- #

# Elasmosauridae
elasmo_tax <- c("Albertonectes_vanderveldei","Aristonectes_quiriquinensis",
                "Callawayasaurus_colombiensis","Elasmosaurus_platyurus",
                "Hydrotherosaurus_alexandrae","Kaiwhekea_katiki",
                "Libonectes_atlasense","Morenosaurus_stocki",
                "Styxosaurus_sp","Thalassomedon_haningtoni")

# Policotylidae
poli_tax <- c("Dolichorhynchops_osborni","Manemergus_anguirostris",
              "Polycotylus_latipinnis","Trinacromerum_bentonianum")

# Thalassophonea
thal_tax <- c("Kronosaurus_boyacensis",
              "Kronosaurus_queenslandicus","Liopleurodon_ferox",
              "Luskhan_itilensis","Peloneustes_philarchus",
              "Sachicasaurus_vitae")


# get relevant banches = rows in rates tables
data <- lapply(data, function (run) {
  # get edge numbers and return data
  return(c(run,
           list(elasmo_edge = which.edge(run$tree, elasmo_tax),
                poli_edge  = which.edge(run$tree, poli_tax),
                thal_edge  = which.edge(run$tree, thal_tax))))
})


# 3. Randomisation test #
# --------------------- #

samp_mean <- function (size, data) {
  # Performs a randomisation test on a set of data including a clade which is
  # a subset. Random samples the data without replacement to the size of the
  # comparison clade. This can be incorported into the replicate funciton.
  #
  # Args:
  #   size: the number of rows to include in the sample.
  #   data:  a vector or column from a matrix of data.frame that includes the
  #          original data; must be large than size. This is the data to be
  #          sampled.
  #
  # Returns:
  #   Means of the resampled data.
  # get rows to sample or same size as clade
  row_n <- sample(seq_along(data), size, replace = FALSE)
  
  # calculate difference in means within and without tha sample rows
  diff_mean <- mean(data[row_n]) - mean(data[-row_n])
}

# randomly sample data and compare to each clade
samp <- lapply(data, function (run) {
  # calculate mean of clades
  mean_rates <- list(elasmo = mean(run$rates[run$elasmo_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$elasmo_edge, "Mean.Scalar"]),
                     poli  = mean(run$rates[run$poli_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$poli_edge, "Mean.Scalar"]),
                     thal  = mean(run$rates[run$thal_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$thal_edge, "Mean.Scalar"]))
  
  # replicate samples
  reps <- list(elasmo = replicate(9999, samp_mean(length(run$elasmo_edge),
                                                  run$rates$Mean.Scalar)),
               poli  = replicate(9999, samp_mean(length(run$poli_edge),
                                                 run$rates$Mean.Scalar)),
               thal  = replicate(9999, samp_mean(length(run$thal_edge),
                                                 run$rates$Mean.Scalar)))
  
  # get differences in means
  mn_pos <- list(elasmo = length(which(abs(reps$elasmo) > abs(mean_rates$elasmo))),
                 poli  = length(which(abs(reps$poli) > abs(mean_rates$poli))),
                 thal  = length(which(abs(reps$thal) > abs(mean_rates$thal))))
  
  # return data
  return(list(elasmo = list(emp  = mean_rates$elasmo,
                            rand = reps$elasmo,
                            pos  = mn_pos$elasmo),
              poli  = list(emp  = mean_rates$poli,
                           rand = reps$poli,
                           pos  = mn_pos$poli),
              thal  = list(emp  = mean_rates$thal,
                           rand = reps$thal,
                           pos  = mn_pos$thal)))
})


# 4. Plot data #
# ------------ #

{
  # plot histograms
  cairo_pdf("randomisation_hist_Neck_13-18.pdf",
            width  = 10,
            height = 14) #original 14
  
  # set up plot area
  par(mfrow = c(6, 3),   #original 6, 3
      mar   = c(2, 1, 1, 2),
      oma   = c(4, 4, 5, 2))
  
  # begin plotting
  for (run in samp) {
    # Elasmosauridae
    hist(run$elasmo$rand, xlim = range(c(run$elasmo$rand, run$elasmo$emp)),
         main = NULL)
    abline(v = run$elasmo$emp, lwd = 2, col = "red")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Elasmosauridae", 3, line = 1, font = 2)
    
    # add axis label
    mtext("Frequency", 2, line = 3)
    
    # Policotylidae
    hist(run$poli$rand, xlim = range(c(run$poli$rand, run$poli$emp)),
         main = NULL)
    abline(v = run$poli$emp, lwd = 2, col = "blue")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Policotylidae", 3, line = 1, font = 2)
    
    # Thalassophonea
    hist(run$thal$rand, xlim = range(c(run$thal$rand, run$thal$emp)),
         main = NULL)
    abline(v = run$thal$emp, lwd = 2, col = "green")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Thalassophonea", 3, line = 1, font = 2)
    
    # add row label
    labels <- c("Tree 13", "Tree 14",
                "Tree 15", "Tree 16",
                "Tree 17", "Tree 18")
    nrun <- 6 - (par()$fig[3] * 6)
    mtext(labels[nrun], 4, line = 2, font = 2)
  }
  
  # x axis title
  mtext("Difference between sampled means", 1, outer = TRUE, line = 1)
  
  # figure title
  mtext("Randomisation tests for time scaling methods", 3,
        outer = TRUE, cex = 1.5, font = 2, line = 3)
  
  # stop plotting
  dev.off()
}


# 5. Assemble table #
# ----------------- #

# assemble table of p-values from randomisation tests
# these are number of random samples with great difference in mean + 1 to find
# position of empirical data / 10000 to give p-value
tbl <- lapply(samp, function (run) {
  # assemble position
  data.frame(Elasmosauridae = (run$elasmo$pos + 1) / 10000,
             Polocotylidae  = (run$poli$pos + 1) / 10000,
             Thalassophonea      = (run$thal$pos + 1) / 10000)
}) %>%
  do.call(rbind, .)

# replace rownames with relevant test
rownames(tbl) <- c("Tree 13", "Tree 14",
                   "Tree 15", "Tree 16",
                   "Tree 17", "Tree 18")

# write table to csv file
write.csv(tbl, file = "randomisation_pval_Neck_13-18.csv")


#########################
#########################

#PART 4: Tree 19 - Tree 20

# 1. Assemble data #
# ---------------- #

# gather trees and rates into a list
data <- list(Tree1 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.19.nex"),
                          rates = read.table("Sauropterygia.pca.scores.pruned.postproc.19.txt",
                                             sep = "\t",
                                             header = TRUE,
                                             row.names = 1)),
             Tree2 = list(tree  = read.nexus("Sauropterygia.pca.scores.pruned.singletree.20.nex"),
                          rates = read.table("Sauropterygia.pca.scores.pruned.postproc.20.txt",
                                             sep = "\t",
                                             header = TRUE,
                                             row.names = 1)))       


# remove first (blank) row of rates tables
# this is from the root branch
data <- lapply(data, function (run) {
  # remove row from rates table
  tbl <- run$rates[-1, ]
  
  # return data
  return(list(tree  = run$tree,
              rates = tbl))
})


# 2. Identify taxa in relevant clades #
# ----------------------------------- #

# Elasmosauridae
elasmo_tax <- c("Albertonectes_vanderveldei","Aristonectes_quiriquinensis",
                "Callawayasaurus_colombiensis","Elasmosaurus_platyurus",
                "Hydrotherosaurus_alexandrae","Kaiwhekea_katiki",
                "Libonectes_atlasense","Morenosaurus_stocki",
                "Styxosaurus_sp","Thalassomedon_haningtoni")

# Policotylidae
poli_tax <- c("Dolichorhynchops_osborni","Manemergus_anguirostris",
              "Polycotylus_latipinnis","Trinacromerum_bentonianum")

# Thalassophonea
thal_tax <- c("Kronosaurus_boyacensis",
              "Kronosaurus_queenslandicus","Liopleurodon_ferox",
              "Luskhan_itilensis","Peloneustes_philarchus",
              "Sachicasaurus_vitae")


# get relevant banches = rows in rates tables
data <- lapply(data, function (run) {
  # get edge numbers and return data
  return(c(run,
           list(elasmo_edge = which.edge(run$tree, elasmo_tax),
                poli_edge  = which.edge(run$tree, poli_tax),
                thal_edge  = which.edge(run$tree, thal_tax))))
})


# 3. Randomisation test #
# --------------------- #

samp_mean <- function (size, data) {
  # Performs a randomisation test on a set of data including a clade which is
  # a subset. Random samples the data without replacement to the size of the
  # comparison clade. This can be incorported into the replicate funciton.
  #
  # Args:
  #   size: the number of rows to include in the sample.
  #   data:  a vector or column from a matrix of data.frame that includes the
  #          original data; must be large than size. This is the data to be
  #          sampled.
  #
  # Returns:
  #   Means of the resampled data.
  # get rows to sample or same size as clade
  row_n <- sample(seq_along(data), size, replace = FALSE)
  
  # calculate difference in means within and without tha sample rows
  diff_mean <- mean(data[row_n]) - mean(data[-row_n])
}

# randomly sample data and compare to each clade
samp <- lapply(data, function (run) {
  # calculate mean of clades
  mean_rates <- list(elasmo = mean(run$rates[run$elasmo_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$elasmo_edge, "Mean.Scalar"]),
                     poli  = mean(run$rates[run$poli_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$poli_edge, "Mean.Scalar"]),
                     thal  = mean(run$rates[run$thal_edge, "Mean.Scalar"]) -
                       mean(run$rates[-run$thal_edge, "Mean.Scalar"]))
  
  # replicate samples
  reps <- list(elasmo = replicate(9999, samp_mean(length(run$elasmo_edge),
                                                  run$rates$Mean.Scalar)),
               poli  = replicate(9999, samp_mean(length(run$poli_edge),
                                                 run$rates$Mean.Scalar)),
               thal  = replicate(9999, samp_mean(length(run$thal_edge),
                                                 run$rates$Mean.Scalar)))
  
  # get differences in means
  mn_pos <- list(elasmo = length(which(abs(reps$elasmo) > abs(mean_rates$elasmo))),
                 poli  = length(which(abs(reps$poli) > abs(mean_rates$poli))),
                 thal  = length(which(abs(reps$thal) > abs(mean_rates$thal))))
  
  # return data
  return(list(elasmo = list(emp  = mean_rates$elasmo,
                            rand = reps$elasmo,
                            pos  = mn_pos$elasmo),
              poli  = list(emp  = mean_rates$poli,
                           rand = reps$poli,
                           pos  = mn_pos$poli),
              thal  = list(emp  = mean_rates$thal,
                           rand = reps$thal,
                           pos  = mn_pos$thal)))
})


# 4. Plot data #
# ------------ #

{
  # plot histograms
  cairo_pdf("randomisation_hist_Neck_19-20.pdf",
            width  = 10,
            height = 14) #original 14
  
  # set up plot area
  par(mfrow = c(6, 3),   #original 6, 3
      mar   = c(2, 1, 1, 2),
      oma   = c(4, 4, 5, 2))
  
  # begin plotting
  for (run in samp) {
    # Elasmosauridae
    hist(run$elasmo$rand, xlim = range(c(run$elasmo$rand, run$elasmo$emp)),
         main = NULL)
    abline(v = run$elasmo$emp, lwd = 2, col = "red")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Elasmosauridae", 3, line = 1, font = 2)
    
    # add axis label
    mtext("Frequency", 2, line = 3)
    
    # Policotylidae
    hist(run$poli$rand, xlim = range(c(run$poli$rand, run$poli$emp)),
         main = NULL)
    abline(v = run$poli$emp, lwd = 2, col = "blue")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Policotylidae", 3, line = 1, font = 2)
    
    # Thalassophonea
    hist(run$thal$rand, xlim = range(c(run$thal$rand, run$thal$emp)),
         main = NULL)
    abline(v = run$thal$emp, lwd = 2, col = "green")
    # plot column label
    if (par()$fig[4] > 0.9) mtext("Thalassophonea", 3, line = 1, font = 2)
    
    # add row label
    labels <- c("Tree 19", "Tree 20")
    nrun <- 6 - (par()$fig[3] * 6)
    mtext(labels[nrun], 4, line = 2, font = 2)
  }
  
  # x axis title
  mtext("Difference between sampled means", 1, outer = TRUE, line = 1)
  
  # figure title
  mtext("Randomisation tests for time scaling methods", 3,
        outer = TRUE, cex = 1.5, font = 2, line = 3)
  
  # stop plotting
  dev.off()
}


# 5. Assemble table #
# ----------------- #

# assemble table of p-values from randomisation tests
# these are number of random samples with great difference in mean + 1 to find
# position of empirical data / 10000 to give p-value
tbl <- lapply(samp, function (run) {
  # assemble position
  data.frame(Elasmosauridae = (run$elasmo$pos + 1) / 10000,
             Polocotylidae  = (run$poli$pos + 1) / 10000,
             Thalassophonea      = (run$thal$pos + 1) / 10000)
}) %>%
  do.call(rbind, .)

# replace rownames with relevant test
rownames(tbl) <- c("Tree 19", "Tree 20")

# write table to csv file
write.csv(tbl, file = "randomisation_pval_Neck_19-20.csv")

