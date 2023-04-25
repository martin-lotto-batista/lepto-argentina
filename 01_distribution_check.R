## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2023
##
## SCRIPT 01: Probability distribution check (Poisson vs Neg. Binomial)
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

# Base formula with:
# - Intercept
# - Monthly cyclic random effects using a second order random walk function (RW2)
# - Interannual random effects extracted from exchangeable Gaussian priors (IID)
res <- "cases~1+f(month, model='rw2', cyclic=TRUE)+f(ID.year, model='iid')"

poisson.check <- nb.check <- NULL
# Run models for each province
for(df in names(lepto)){
  poisson.check[[df]] <- run.mod(res, "poisson", lepto[[df]]) # Poisson
  nb.check[[df]] <- run.mod(res, "nbinomial", lepto[[df]]) # Negative binomial
}

# DIC: Lower values indicate a better fit
poisson.check$df.er$dic$dic # 493
poisson.check$df.sf$dic$dic # 667
nb.check$df.er$dic$dic # 445
nb.check$df.sf$dic$dic # 621

# Overdispersion parameter:
# Extract the parameter and assess whether it is positive and includes the 
# null (0), indicating its relevance in the model
# If there is not overdispersion in the data, then the parameter should include 0
bind_rows(nb.check$df.er$summary.hyperpar[1,c(1:5)],
          nb.check$df.sf$summary.hyperpar[1,c(1:5)]) %>%
  remove_rownames() %>% 
  mutate(province=c("Entre Ríos", "Santa Fe")) %>% 
  ggplot() +
  geom_pointrange(aes(x=province, y=mean, 
                      ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_hline(yintercept=0, linetype="dashed", col="grey80") +
  labs(x="", y="Posterior overdispersion parameter") +
  coord_flip() +
  theme_bw()

## ############################################################################
## END
## ############################################################################