################################################################################
########### Phyllostomidae State-Dependent Diversification Analysis ############
############### Part 3: State-Dependent Diversification Analysis ###############
################################################################################

# Calling previous scripts

source("1_Codification_types.R")
source("2_Pruning&Filtering.R")

# SSE processes only work with an ultrametric tree, so, we must force its
# ultrametricity

Ultra_Phyllo <- force.ultrametric(Ace_Tree)

#### Multiple State-Dependent Speciation and Extinction Process (MuSSE) ####

  ## Preparring MuSSE

  Mult_States <- Mult_Matrix$diet
  names(Mult_States) = Mult_Matrix$species
  
  
  Asym_Mult <- matrix(c(0,2.9,2.9,2.9,2.9,1,0,2.9,2.9,2.9,1,1,0,2.9,2.9,1,1,1
                         ,0,2.9,1,1,1,1,0,2.9,1,1,1,0),
                         nrow = 5)

  asr.marginal(make.musse(Ultra_Phyllo, Mult_States, 5), 
               Asym_Mult)
