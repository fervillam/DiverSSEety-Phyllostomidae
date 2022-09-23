################################################################################
########### Phyllostomidae State-Dependent Diversification Analysis ############
############### Part 2: Pruning the tree and filtering matrices ################
################################################################################

# Calling the previous script

source("1_Codificaion_types.R")

# The tree is taken from Upham et al. (2019)

Phyllostomidae_Tree <- read.tree("aceTree.tree",tree.names = T)

# Pruning the tree: we will only keep the species that have diet reports

No_Report <- Phyllostomidae_Tree$tip.label[!Phyllostomidae_Tree$tip.label %in% 
                                             Mult_Matrix$species]

Ace_Tree <- drop.tip(Phyllostomidae_Tree, tip = No_Report)


Mult_Matrix <- Mult_Matrix %>% filter(Mult_Matrix$species%in%Ace_Tree$tip.label)

# Filtering matrices 

  # Base matrix

  Bm <- Bm %>% filter(Bm$species%in%Ace_Tree$tip.label)

  # Binary #1
  
  Bin_1 <- Bin_1 %>% filter(Bin_1$species%in%Ace_Tree$tip.label)
  
  # Binary #2
  
  Bin_2 <- Bin_2 %>% filter(Bin_2$species%in%Ace_Tree$tip.label)

  # Binary #3
  
  Bin_3 <- Bin_3 %>% filter(Bin_3$species%in%Ace_Tree$tip.label)
  
