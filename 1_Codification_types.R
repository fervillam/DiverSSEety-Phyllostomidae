
################################################################################
########### Phyllostomidae State-Dependent Diversification Analysis ############
########################## Part 1: Codification Types ##########################
################################################################################

#### Preliminars and foreplay ####

# Setting work directory

setwd("Documents/Phyllostomidae-Anc-Reconstruction")

# Installing and loading required packages

if(!require("pacman")){
  install.packages("pacman")
}

library(pacman)

pacman::p_load(ape, devtools, dplyr, ggplot2, phangorn, phytools, 
               readxl, tidyr)

#### Preparing the data and cleaning it ####

# Importing diet table

Diet_Table <- read_xlsx("Tabla_dietas.xlsx", guess_max = 10000)

# "Reports" column will be splitted in two columns called "reports" and 
# "species", respectively

split_names <- strsplit(Diet_Table$Reportes, "-")

# Creating a dataframe with species names and reports

species_reports <- as.data.frame(matrix(unlist(split_names), ncol = 2, 
                                        byrow = T))

names(species_reports) <- c("species", "reports")

# Creating a dataframe that contains all info revised by Daniela

revised <- as.data.frame(Diet_Table[ , 2:length(Diet_Table[1, ])])

# Binding rows of species and reports. Columns will be the reports

bind_sp_rp <- cbind(species_reports, revised)

# We will only keep the following dietary reports: carnivory, frugivory,
# hematophagy, insectivory and nectarivory. So, we need to delete everything else

Trimm_Diets <- bind_sp_rp[!bind_sp_rp$reports %in% c("obs", "matVeg", "contEst",
                                                     "heces", "isótopos", "nect",
                                                     "poli"),]

# All NA's will be taken as cero

Trimm_Diets[is.na(Trimm_Diets)] = 0

rownames(Trimm_Diets) <- 1:nrow(Trimm_Diets)

# Reports by number of individuals will be organized as binary incidences

Incidence_Matrix <- Trimm_Diets

# Changing continuous reports to binary ones

for (columns in 3:length(Trimm_Diets[1,])) {
  for (rows in 1:length(Trimm_Diets[,1])) {
    if (Incidence_Matrix[rows,columns]>0) {
      Incidence_Matrix[rows,columns] <- 1
    }
  }
}

#### Creating the codification matrices ####

### Base matrix (Bm)

# We will use the same species and names of Incidence_Matrix, but we will add
# four additional columns: count (diet reports per species), summ (total sum of
# diet reports per species), freq (total diet reports given the total reports by
# species and state (diet character state))

Preliminary_Bm <- data.frame("species" = Trimm_Diets$species,
                             "reports" = Trimm_Diets$reports,
                             "count" = 0,
                             "summ" = 0,
                             "state" = 0)
  
  ## count
  for (i in 1:length(Trimm_Diets$species)) {
    Preliminary_Bm[i,3] <- sum(Incidence_Matrix[i,3:length(Trimm_Diets[1,])],
                               na.rm = T)
  }
  
  
  ## summ
  summation <- Preliminary_Bm %>% group_by(species) %>% summarize(summ=sum(count))
  
  Preliminary_Bm$summ = rep(summation$summ,each=5)
  
  
  ## freq
  
  Preliminary_Bm$freq <- round(Preliminary_Bm$count/Preliminary_Bm$summ,digits=3)
  
  
  ## state
  # Having the diet report frequencies, we will asign them a state to each species
  # Absent (0), Complementary (1), Predominant (2) or Strict (3)
  
  freq1 <- 0.05
  freq2 <- 0.5
  freq3 <- 0.95
  
    for (i in 1:length(Preliminary_Bm$species)) { 
      if(Preliminary_Bm[i,6]<freq1) {
        Preliminary_Bm[i,5] = 0
      } 
      else if (Preliminary_Bm[i,6]==freq1){
        Preliminary_Bm[i,5]=0
      }
      else if (Preliminary_Bm[i,6]>freq1&Preliminary_Bm[i,5]<freq2){
        Preliminary_Bm[i,5]=1
      }
      else if (Preliminary_Bm[i,6]==freq2){
        Preliminary_Bm[i,5]=1
      }
      else if (Preliminary_Bm[i,6]>freq2&Preliminary_Bm[i,6]<freq3){
        Preliminary_Bm[i,5]=2
      }
      else if (Preliminary_Bm[i,6]==freq3){
        Preliminary_Bm[i,5]=3
      }
      else if (Preliminary_Bm[i,6]>freq3){
        Preliminary_Bm[i,5]=3
      }
    } 

  # Creating a dataframe whose rows and columns will be the species and the
  # diets, respectively
  
  Bm_datfr_1 <- data.frame("species" = Preliminary_Bm$species,
                           "diet" = Preliminary_Bm$reports,
                           "states" = Preliminary_Bm$state)
  
  # Creating a matrix that has six columns: one with the species' names and the
  # following five with the diets
  
  Bm_matrix <- matrix(Bm_datfr_1[,3], nrow = length(Bm_datfr_1$species)/5,
                                                    ncol = 5, byrow = T)
  
  # Creating a dataframe with the species and the diets in alphabetic order
  # This will be the Base "matrix" (Bm)
  
  Bm <- data.frame("species" = unique(Bm_datfr_1$species),
                   "carnivory" = Bm_matrix[, 3],
                   "frugivory" = Bm_matrix[, 4],
                   "hematophagy" = Bm_matrix[, 2],
                   "insectivory" = Bm_matrix[, 1],
                   "nectarivory" = Bm_matrix[, 5])
  
### Multistate "matrix" (Mult_Matrix)
  
  ## We will add the strict (3) or predominant (2) diets obtained in Bm
  
  Mult_Matrix <- data.frame("species" = unique(Bm$species), 
                            "diet" = NA)
  
  freq_mult <- matrix(Preliminary_Bm$freq,
                      nrow = length(Bm_datfr_1$species)/5,
                      ncol = 5,
                      byrow = T)

  for (i in 1:length(Bm$species)) {
    if (max(freq_mult[i,1:5])==freq_mult[i,1]) {
      Mult_Matrix[i,2]=3
    }
    else if (max(freq_mult[i,1:5])==freq_mult[i,2]) {
      Mult_Matrix[i,2]=2
    }
    else if (max(freq_mult[i,1:5])==freq_mult[i,3]) {
      Mult_Matrix[i,2]=0
    }
    else if (max(freq_mult[i,1:5])==freq_mult[i,4]) {
      Mult_Matrix[i,2]=1
    }
    else if (max(freq_mult[i,1:5])==freq_mult[i,5]) {
      Mult_Matrix[i,2]=4
    }
  }

### Binary matrices
  
  # Binary matrix #1: we will use complementary, predominant and strict diets
  # shown in Bm. This ones will be codified as presence (1). Absent ones as 0.
  
  Bin_1 <- data.frame("species" = Bm$species,
                      "carnivory" = 0,
                      "frugivory" = 0,
                      "hematophagy" = 0,
                      "insectivory" = 0,
                      "nectarivory" = 0)
  
  for (rows in 1:length(Bm$species)) {
    for (colums in 2:6) {
      if (Bm[rows,colums]==0) {
        Bin_1[rows,colums]=0
      }
      else if (Bm[rows,colums]>0) {
        Bin_1[rows,colums]=1
      }
    }
  }

  # Binary matrix #2: predominant and strict diets will be codified as presences
  # (1). Complementary and absent as absences (0).
  
  Bin_2 <- data.frame("species" = Bm$species,
                      "carnivory" = 0,
                      "frugivory" = 0,
                      "hematophagy" = 0,
                      "insectivory" = 0,
                      "nectarivory" = 0)

  for (rows in 1:length(Bm$species)) {
    for (colums in 2:6) {
      if (Bm[rows,colums]<1) {
        Bin_2[rows,colums]=0
      }
      else if (Bm[rows,colums]>1) {
        Bin_2[rows,colums]=1
      }
    }
  }
  
  # Binary matrix #3: only strict diets will be codified as presence (1). The
  # other ones will be used as absences (0).
  
  Bin_3 <- data.frame("species" = Bm$species,
                      "carnivory" = 0,
                      "frugivory" = 0,
                      "hematophagy" = 0,
                      "insectivory" = 0,
                      "nectarivory" = 0)

  
  for (rows in 1:length(Bm$species)) {
    for (colums in 2:6) {
      if (Bm[rows,colums]<2) {
        Bin_3[rows,colums]=0
      }
      else if (Bm[rows,colums]>2) {
        Bin_3[rows,colums]=1
      }
    }
  }
  
