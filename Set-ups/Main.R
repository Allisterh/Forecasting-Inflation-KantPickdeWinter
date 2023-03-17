############################ Nowcasting GDP using machine learning methods #####################################
#
#                                   Dennis Kant, Andreas Pick and Jasper de Winter
#                                                 3/17/2023
#
###############################################################################################################
#
rm(list=ls())
ROOT <- rprojroot::find_rstudio_root_file()  # !!!! Adjust main path

# Models         
setwd(paste0(ROOT,"/Set-ups/"))                                                 # Adjust Estimation.r to change settings
models    <- c("RW", "AR", "MIDAS-F", "LASSO", "EN", "RS", "RP", "RF")          # DFM available via Matlab code
modelName <- models[8]                                                          # Adjust to run a specific model
source(paste0(modelName, ".r"))

# Calculate RMSFEs
setwd(paste0(ROOT,"/Set-ups/"))                                                 # Adjust Estimation.r to change settings
source("RMSFE.r")

# Diebold Mariano tests
setwd(paste0(ROOT,"/Set-ups/"))                                                 # Adjust Estimation.r to change settings
source("DM_TEST.r")

