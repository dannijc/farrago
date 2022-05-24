############ setup ############ -------------------------
## install packages
#install.packages("tidyverse") # remove the first '#" and press ctrl + enter if you haven't downloaded tidyverse before
#install.packages("metafor") # remove the first '#" and press ctrl + enter if you haven't downloaded metafor before

## load packages
library(tidyverse)
library(metafor)

## load data
effects <- read_csv("./data/MetaAnalysis_EffectSizes.csv")

############ hedge's g ############ -------------------------
## calculate hedges-g using metafor package, dataframe arranged with mean (m), 
## standard deviation (sd) and number of replicates (n)
### for each contrast (e.g. species (1) and intraspecific (2) mean absolute values, sd and n for each parameter in each study)

## calculates the standardized mean difference (mean effect size, yi) and variance (vi)  ##
effects_SMD <- escalc(measure = "SMD", 
                      m1i = mspecies, 
                      sd1i = sdspecies, 
                      n1i = nspecies,
                      m2i = mintra, 
                      sd2i = mintra, 
                      n2i = nintra, 
                      data = effects)  

write.csv(effects_SMD, './data/MetaAnalysis_HedgesG.csv')   ## exports dataframe to a csv file ##

## fit a random effects model to the data  ##
rand_eff_mod <- rma(yi, vi, data = effects_SMD)

## create a forest plot of the data - ordered by species ##
metafor::forest(rand_eff_mod, 
                addpred = TRUE, 
                header = "Parameter", 
                slab = effects_SMD$parameter, 
                xlim = c(-10,6),
                ilab = effects_SMD$authors, 
                ilab.xpos = -3, 
                xlab = "Hedges' G", 
                order = effects_SMD$parameter) 


