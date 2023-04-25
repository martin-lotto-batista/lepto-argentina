## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2023
##
## SCRIPT 03: Compute out of sample predictions in candidate models
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

# Labels for saving files
# - er: Entre Ríos
# - sf: Santa Fe
province.labels <- c("er", "sf")

# Create a list of vectors of candidate models for each province
# - First element (empty) includes seasonal random effects only (baseline)
# - Second to last elements are chosen ENSO lags and combinations of local 
# climate candidates
f <- list(c("","+nino34.2","+nino34.3","+nino34.4","+par.1+prcp.1"), # Entre Ríos
          c("","+nino34.2","+nino34.3","+par.1+prcp.1")) # Santa Fe

# Lags for each province
# Lags are derived from the predictors (see above) and are considered for
# defining the 12 months to be removed from the sample. If a predictor is lagged 
# by 3 months, then the out of the sample period will begin three months before 
# the prediction target month. 
l <- list(c(0,2,3,4,1), # Entre Ríos
          c(0,2,3,1)) # Santa Fe

# Run out of sample predictions by removing 12 months out of the sample
for(i in 1:2){
  pred.models(data=lepto[[i]],
              forms=f[[i]], 
              lags=l[[i]], 
              fileName=paste0(province.labels[i], "_preds.rds"))
}

## ############################################################################
## END
## ############################################################################