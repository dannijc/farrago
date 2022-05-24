########### Note #############
# in the following code the suffix 'i' or 'intra' means intraspecific and 's' or 'spec' means species
# 'f14' means fryxell 2014 and 'f18' means fryxell 2018
# for a description of each variable, see the 'VarMetadata_Fryxell2014_2018.csv' file

########### Set-up #############
## load packages
#install.packages('tidyverse') # remove the first # from the start of this line and press ctrl and enter if you haven't installed tidyverse yet
library(tidyverse)

##############################
######## Fryxell 2014 ########-------------------------------------
##############################

## load data
fryxell_2014_spec <- read.csv("./data/fryxell_2014_species.csv")
fryxell_2014_intra <- read.csv("./data/fryxell_2014_intra.csv")

## take the fryxell_2014_spec dataframe, remove any missing data,
f14s_numobs <- na.omit(fryxell_2014_spec) %>% ## 'and then do this' (that's what %>% means)
  tally() ## and then count the number of rows/observations

## same again for intra
f14i_numobs <- na.omit(fryxell_2014_intra) %>%
  tally()

#### pelagic chla ####----------------------------------------------
## subsetting the dataframe to include the three columns we need
f14s_pchla <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "pchla")))
f14i_pchla <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "pchla")))

## determine the standard deviation, ignoring missing values
f14s_pchla_sd <- sd(fryxell_2014_spec$pchla, na.rm = TRUE) 
f14i_pchla_sd <- sd(fryxell_2014_intra$pchla, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_pchla, names_from = fish_yn, values_from = pchla)  
f14i_wider <- pivot_wider(f14i_pchla, names_from = fish_treat, values_from = pchla)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_pchla_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_pchla_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_pchla_mav <- abs(mean(f14s_pchla_diffs, na.rm = TRUE)) 
f14i_pchla_mav <- abs(mean(f14i_pchla_diffs, na.rm = TRUE)) 


#### pH ####----------------------------------------------
## subsetting the dataframe to include the three columns we need
f14s_pH <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "pH")))
f14i_pH <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "pH")))

## determine the standard deviation, ignoring missing values
f14s_pH_sd <- sd(fryxell_2014_spec$pH, na.rm = TRUE) 
f14i_pH_sd <- sd(fryxell_2014_intra$pH, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_pH, names_from = fish_yn, values_from = pH)  
f14i_wider <- pivot_wider(f14i_pH, names_from = fish_treat, values_from = pH)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_pH_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_pH_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_pH_mav <- abs(mean(f14s_pH_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_pH_mav <- abs(mean(f14i_pH_diffs, na.rm = TRUE)) ## intraspecific mu



#### specific conductance ####----------------------------------------------
## subsetting the dataframe to include the three columns we need
f14s_speccond <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "speccond")))
f14i_speccond <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "speccond")))

## determine the standard deviation, ignoring missing values
f14s_speccond_sd <- sd(fryxell_2014_spec$speccond, na.rm = TRUE) 
f14i_speccond_sd <- sd(fryxell_2014_intra$speccond, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_speccond, names_from = fish_yn, values_from = speccond)  
f14i_wider <- pivot_wider(f14i_speccond, names_from = fish_treat, values_from = speccond)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_speccond_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_speccond_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_speccond_mav <- abs(mean(f14s_speccond_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_speccond_mav <- abs(mean(f14i_speccond_diffs, na.rm = TRUE)) ## intraspecific mu


#### ecosystem respiration ####----------------------------------------------
f14s_er <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "er")))
f14i_er <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "er")))

## determine the standard deviation, ignoring missing values
f14s_er_sd <- sd(fryxell_2014_spec$er, na.rm = TRUE) 
f14i_er_sd <- sd(fryxell_2014_intra$er, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_er, names_from = fish_yn, values_from = er)  
f14i_wider <- pivot_wider(f14i_er, names_from = fish_treat, values_from = er)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_er_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_er_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_er_mav <- abs(mean(f14s_er_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_er_mav <- abs(mean(f14i_er_diffs, na.rm = TRUE)) ## intraspecific mu



#### GPP ####----------------------------------------------
## subsetting the dataframe to include the three columns we need
f14s_gpp <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "gpp")))
f14i_gpp <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "gpp")))

## determine the standard deviation, ignoring missing values
f14s_gpp_sd <- sd(fryxell_2014_spec$gpp, na.rm = TRUE) 
f14i_gpp_sd <- sd(fryxell_2014_intra$gpp, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_gpp, names_from = fish_yn, values_from = gpp)  
f14i_wider <- pivot_wider(f14i_gpp, names_from = fish_treat, values_from = gpp)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_gpp_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_gpp_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_gpp_mav <- abs(mean(f14s_gpp_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_gpp_mav <- abs(mean(f14i_gpp_diffs, na.rm = TRUE)) ## intraspecific mu


#### NPP ####----------------------------------------------
## subsetting the dataframe to include the three columns we need
f14s_npp <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "npp")))
f14i_npp <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "npp")))

## determine the standard deviation, ignoring missing values
f14s_npp_sd <- sd(fryxell_2014_spec$npp, na.rm = TRUE) 
f14i_npp_sd <- sd(fryxell_2014_intra$npp, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_npp, names_from = fish_yn, values_from = npp)  
f14i_wider <- pivot_wider(f14i_npp, names_from = fish_treat, values_from = npp)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_npp_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_npp_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_npp_mav <- abs(mean(f14s_npp_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_npp_mav <- abs(mean(f14i_npp_diffs, na.rm = TRUE)) ## intraspecific mu

#### nitrate ####----------------------------------------------
## subsetting the dataframe to include the three columns we need
f14s_nox <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "nox")))
f14i_nox <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "nox")))

## determine the standard deviation, ignoring missing values
f14s_nox_sd <- sd(fryxell_2014_spec$nox, na.rm = TRUE) 
f14i_nox_sd <- sd(fryxell_2014_intra$nox, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_nox, names_from = fish_yn, values_from = nox)  
f14i_wider <- pivot_wider(f14i_nox, names_from = fish_treat, values_from = nox)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_nox_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_nox_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_nox_mav <- abs(mean(f14s_nox_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_nox_mav <- abs(mean(f14i_nox_diffs, na.rm = TRUE)) ## intraspecific mu

#### phosphate ####----------------------------------------------

## subsetting the dataframe to include the three columns we need
f14s_po4 <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "po4")))
f14i_po4 <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "po4")))

## determine the standard deviation, ignoring missing values
f14s_po4_sd <- sd(fryxell_2014_spec$po4, na.rm = TRUE) 
f14i_po4_sd <- sd(fryxell_2014_intra$po4, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_po4, names_from = fish_yn, values_from = po4)  
f14i_wider <- pivot_wider(f14i_po4, names_from = fish_treat, values_from = po4)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_po4_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_po4_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_po4_mav <- abs(mean(f14s_po4_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_po4_mav <- abs(mean(f14i_po4_diffs, na.rm = TRUE)) ## intraspecific mu


#### cladocerans ####----------------------------------------------

## subsetting the dataframe to include the three columns we need
f14s_clad <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "clad")))
f14i_clad <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "clad")))

## determine the standard deviation, ignoring missing values
f14s_clad_sd <- sd(fryxell_2014_spec$clad, na.rm = TRUE) 
f14i_clad_sd <- sd(fryxell_2014_intra$clad, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_clad, names_from = fish_yn, values_from = clad)  
f14i_wider <- pivot_wider(f14i_clad, names_from = fish_treat, values_from = clad)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_clad_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_clad_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_clad_mav <- abs(mean(f14s_clad_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_clad_mav <- abs(mean(f14i_clad_diffs, na.rm = TRUE)) ## intraspecific mu

#### copepods ####----------------------------------------------

## subsetting the dataframe to include the three columns we need
f14s_cop <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "cop")))
f14i_cop <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "cop")))

## determine the standard deviation, ignoring missing values
f14s_cop_sd <- sd(fryxell_2014_spec$cop, na.rm = TRUE) 
f14i_cop_sd <- sd(fryxell_2014_intra$cop, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_cop, names_from = fish_yn, values_from = cop)  
f14i_wider <- pivot_wider(f14i_cop, names_from = fish_treat, values_from = cop)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_cop_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_cop_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_cop_mav <- abs(mean(f14s_cop_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_cop_mav <- abs(mean(f14i_cop_diffs, na.rm = TRUE)) ## intraspecific mu


#### zooplankton ####----------------------------------------------

## subsetting the dataframe to include the three columns we need
f14s_zoop <- select(fryxell_2014_spec, starts_with(c("mesocosm", "fish_yn", "zoop")))
f14i_zoop <- select(fryxell_2014_intra, starts_with(c("mesocosm", "fish_treat", "zoop")))

## determine the standard deviation, ignoring missing values
f14s_zoop_sd <- sd(fryxell_2014_spec$zoop, na.rm = TRUE) 
f14i_zoop_sd <- sd(fryxell_2014_intra$zoop, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f14s_wider <- pivot_wider(f14s_zoop, names_from = fish_yn, values_from = zoop)  
f14i_wider <- pivot_wider(f14i_zoop, names_from = fish_treat, values_from = zoop)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f14s_zoop_diffs <- outer(f14s_wider$y, f14s_wider$n, FUN = "-")
f14i_zoop_diffs <- outer(f14i_wider$c, f14i_wider$h, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f14s_zoop_mav <- abs(mean(f14s_zoop_diffs, na.rm = TRUE)) ## species present/absent mu
f14i_zoop_mav <- abs(mean(f14i_zoop_diffs, na.rm = TRUE)) ## intraspecific mu

##############################
######## Fryxell 2018 ########-------------------------------------
##############################

## load data
fryxell_2018_spec <- read.csv("./data/fryxell_2018_species.csv")
fryxell_2018_intra <- read.csv("./data/fryxell_2018_intra.csv")

## take the fryxell_2018_spec dataframe, remove any missing data,
f18s_numobs <- na.omit(fryxell_2018_spec) %>% ## 'and then do this' (that's what %>% means)
  tally() ## and then count the number of rows/observations

## same again for intra
f18i_numobs <- na.omit(fryxell_2018_intra) %>%
  tally()

#### net primary productivity  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_npp <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "npp")))
f18i_npp <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "npp")))

## determine the standard deviation, ignoring missing values
f18s_npp_sd <- sd(fryxell_2018_spec$npp, na.rm = TRUE) 
f18i_npp_sd <- sd(fryxell_2018_intra$npp, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_npp, names_from = fish_yn, values_from = npp)  
f18i_wider <- pivot_wider(f18i_npp, names_from = fish_treat, values_from = npp)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_npp_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_npp_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_npp_mav <- abs(mean(f18s_npp_diffs, na.rm = TRUE)) 
f18i_npp_mav <- abs(mean(f18i_npp_diffs, na.rm = TRUE)) 

#### ecosystem respiration ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_er <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "er")))
f18i_er <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "er")))

## determine the standard deviation, ignoring missing values
f18s_er_sd <- sd(fryxell_2018_spec$er, na.rm = TRUE) 
f18i_er_sd <- sd(fryxell_2018_intra$er, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_er, names_from = fish_yn, values_from = er)  
f18i_wider <- pivot_wider(f18i_er, names_from = fish_treat, values_from = er)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_er_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_er_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_er_mav <- abs(mean(f18s_er_diffs, na.rm = TRUE)) 
f18i_er_mav <- abs(mean(f18i_er_diffs, na.rm = TRUE)) 

#### gross primary productivity  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_gpp <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "gpp")))
f18i_gpp <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "gpp")))

## determine the standard deviation, ignoring missing values
f18s_gpp_sd <- sd(fryxell_2018_spec$gpp, na.rm = TRUE) 
f18i_gpp_sd <- sd(fryxell_2018_intra$gpp, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_gpp, names_from = fish_yn, values_from = gpp)  
f18i_wider <- pivot_wider(f18i_gpp, names_from = fish_treat, values_from = gpp)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_gpp_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_gpp_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_gpp_mav <- abs(mean(f18s_gpp_diffs, na.rm = TRUE)) 
f18i_gpp_mav <- abs(mean(f18i_gpp_diffs, na.rm = TRUE)) 

#### pelagic chlorophyll a  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_pchla <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "pchla")))
f18i_pchla <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "pchla")))

## determine the standard deviation, ignoring missing values
f18s_pchla_sd <- sd(fryxell_2018_spec$pchla, na.rm = TRUE) 
f18i_pchla_sd <- sd(fryxell_2018_intra$pchla, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_pchla, names_from = fish_yn, values_from = pchla)  
f18i_wider <- pivot_wider(f18i_pchla, names_from = fish_treat, values_from = pchla)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_pchla_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_pchla_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_pchla_mav <- abs(mean(f18s_pchla_diffs, na.rm = TRUE)) 
f18i_pchla_mav <- abs(mean(f18i_pchla_diffs, na.rm = TRUE)) 

#### nitrate  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_nox <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "nox")))
f18i_nox <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "nox")))

## determine the standard deviation, ignoring missing values
f18s_nox_sd <- sd(fryxell_2018_spec$nox, na.rm = TRUE) 
f18i_nox_sd <- sd(fryxell_2018_intra$nox, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_nox, names_from = fish_yn, values_from = nox)  
f18i_wider <- pivot_wider(f18i_nox, names_from = fish_treat, values_from = nox)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_nox_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_nox_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_nox_mav <- abs(mean(f18s_nox_diffs, na.rm = TRUE)) 
f18i_nox_mav <- abs(mean(f18i_nox_diffs, na.rm = TRUE)) 

#### phosphate  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_po4 <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "po4")))
f18i_po4 <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "po4")))

## determine the standard deviation, ignoring missing values
f18s_po4_sd <- sd(fryxell_2018_spec$po4, na.rm = TRUE) 
f18i_po4_sd <- sd(fryxell_2018_intra$po4, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_po4, names_from = fish_yn, values_from = po4)  
f18i_wider <- pivot_wider(f18i_po4, names_from = fish_treat, values_from = po4)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_po4_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_po4_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_po4_mav <- abs(mean(f18s_po4_diffs, na.rm = TRUE)) 
f18i_po4_mav <- abs(mean(f18i_po4_diffs, na.rm = TRUE)) 

#### ammonium  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_nh4 <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "nh4")))
f18i_nh4 <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "nh4")))

## determine the standard deviation, ignoring missing values
f18s_nh4_sd <- sd(fryxell_2018_spec$nh4, na.rm = TRUE) 
f18i_nh4_sd <- sd(fryxell_2018_intra$nh4, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_nh4, names_from = fish_yn, values_from = nh4)  
f18i_wider <- pivot_wider(f18i_nh4, names_from = fish_treat, values_from = nh4)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_nh4_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_nh4_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_nh4_mav <- abs(mean(f18s_nh4_diffs, na.rm = TRUE)) 
f18i_nh4_mav <- abs(mean(f18i_nh4_diffs, na.rm = TRUE)) 

#### pH  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_pH <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "pH")))
f18i_pH <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "pH")))

## determine the standard deviation, ignoring missing values
f18s_pH_sd <- sd(fryxell_2018_spec$pH, na.rm = TRUE) 
f18i_pH_sd <- sd(fryxell_2018_intra$pH, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_pH, names_from = fish_yn, values_from = pH)  
f18i_wider <- pivot_wider(f18i_pH, names_from = fish_treat, values_from = pH)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_pH_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_pH_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_pH_mav <- abs(mean(f18s_pH_diffs, na.rm = TRUE)) 
f18i_pH_mav <- abs(mean(f18i_pH_diffs, na.rm = TRUE))

#### specific conductance  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_speccond <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "speccond")))
f18i_speccond <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "speccond")))

## determine the standard deviation, ignoring missing values
f18s_speccond_sd <- sd(fryxell_2018_spec$speccond, na.rm = TRUE) 
f18i_speccond_sd <- sd(fryxell_2018_intra$speccond, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_speccond, names_from = fish_yn, values_from = speccond)  
f18i_wider <- pivot_wider(f18i_speccond, names_from = fish_treat, values_from = speccond)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_speccond_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_speccond_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_speccond_mav <- abs(mean(f18s_speccond_diffs, na.rm = TRUE)) 
f18i_speccond_mav <- abs(mean(f18i_speccond_diffs, na.rm = TRUE)) 

#### total macroinvertebrates  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_totalmacro <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "totalmacro")))
f18i_totalmacro <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "totalmacro")))

## determine the standard deviation, ignoring missing values
f18s_totalmacro_sd <- sd(fryxell_2018_spec$totalmacro, na.rm = TRUE) 
f18i_totalmacro_sd <- sd(fryxell_2018_intra$totalmacro, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_totalmacro, names_from = fish_yn, values_from = totalmacro)  
f18i_wider <- pivot_wider(f18i_totalmacro, names_from = fish_treat, values_from = totalmacro)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_totalmacro_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_totalmacro_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_totalmacro_mav <- abs(mean(f18s_totalmacro_diffs, na.rm = TRUE)) 
f18i_totalmacro_mav <- abs(mean(f18i_totalmacro_diffs, na.rm = TRUE)) 
#### periphyton drymass  ####----------------------------------------------

# subsetting the dataframe to include the three columns we need
f18s_peri_drymass <- select(fryxell_2018_spec, starts_with(c("mesocosm", "fish_yn", "peri_drymass")))
f18i_peri_drymass <- select(fryxell_2018_intra, starts_with(c("mesocosm", "fish_treat", "peri_drymass")))

## determine the standard deviation, ignoring missing values
f18s_peri_drymass_sd <- sd(fryxell_2018_spec$peri_drymass, na.rm = TRUE) 
f18i_peri_drymass_sd <- sd(fryxell_2018_intra$peri_drymass, na.rm = TRUE) 

## convert the data frame so treatments are in individual columns 
f18s_wider <- pivot_wider(f18s_peri_drymass, names_from = fish_yn, values_from = peri_drymass)  
f18i_wider <- pivot_wider(f18i_peri_drymass, names_from = fish_treat, values_from = peri_drymass)  

## creates a matrix of the pairwise differences between the two treatments, 'a' and 'b' are the treatment codes
f18s_peri_drymass_diffs <- outer(f18s_wider$y, f18s_wider$n, FUN = "-")
f18i_peri_drymass_diffs <- outer(f18i_wider$c, f18i_wider$w, FUN = "-")

## calculating mu (spec or intra)
## calculates the mean absolute value of the pairwise differences, ignoring empty cells
f18s_peri_drymass_mav <- abs(mean(f18s_peri_drymass_diffs, na.rm = TRUE)) 
f18i_peri_drymass_mav <- abs(mean(f18i_peri_drymass_diffs, na.rm = TRUE))