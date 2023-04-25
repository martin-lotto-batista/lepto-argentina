## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2023
##
## SCRIPT 02: Fit models with predictors
##
## ############################################################################
##
## Script authors: 
## - Martín Lotto Batista, ORCID: 0000-0002-9437-5270
## - Dr. Eleanor Rees, ORCID: 0000-0002-4993-2795
## - Prof. Rachel Lowe, ORCID: 0000-0003-3939-7343
##
## ############################################################################

# Set up session
source("00_source_functions.R")

# Predictors
## ENSO: Niño 3.4 index  lagged 0-12 months
enso <- c("", paste0("+nino34.", c(0:12)))

## Precipitation: monthly accumulated precipitation lagged 0-5 months
prcp <- c("", paste0("+prcp.", c(0:5)))

## Paraná river: Monthly mean river height lagged 0-5 months
par <- c("", paste0("+par.", c(0:5)))

################################################################################
# FIT MODELS
################################################################################
## Labels for saving files
## - er: Entre Ríos
## - sf: Santa Fe
province.labels <- c("er", "sf")

for(i in 1:2){
  # ENSO models: fit models including the Niño 3.4 index at different lags, 
  # from zero to 12 months (i.e. test 13 models)
  fit.models(lepto[[i]], 
             var1=enso, 
             fileName=paste0(province.labels[i],"_enso_fit.rds"))
  
  # Local climate models: fit models including the precipitation and river height,
  # lagged from zero to 5 months, and all possible combinations between variables
  # (i.e. test 49 models)
  fit.models(lepto[[i]], 
             var1=par, 
             var2=prcp, 
             fileName=paste0(province.labels[i],"_clim_fit.rds"))
}

## ############################################################################
## END
## ############################################################################