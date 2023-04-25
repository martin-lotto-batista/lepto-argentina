## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2023
##
## SCRIPT 05: Create supplementary figures and tables
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

# Prepare data
df.lepto <- read.csv("data/data.csv")

df.er <- df.lepto %>% filter(prov == "Entre Ríos")
df.sf <- df.lepto %>% filter(prov == "Santa Fe")

# Model outputs
er.enso.fit <- readRDS("model_out/er_enso_fit.rds")
er.clim.fit <- readRDS("model_out/er_clim_fit.rds")
sf.enso.fit <- readRDS("model_out/sf_enso_fit.rds")
sf.clim.fit <- readRDS("model_out/sf_clim_fit.rds")
er.preds <- readRDS("model_out/er_preds.rds")
sf.preds <- readRDS("model_out/sf_preds.rds")
#####

# Figure S1: Plot Niño regions on the Pacific Ocean ####
# Map of the pacific ocean
pacific <- st_read("data/pacific_ocean.shp")

# Coordinates of the labels for each Niño region
labs <- data.frame(city=c("Niño 1+2","Niño 3","Niño 3.4","Niño 4"),
                   long=c(15807368,13580978,9128198,5232016),
                   lat=c(333958,890556,890556,890556))

# Plot Niño regions
s1 <- pacific %>% 
  ggplot() +
  geom_sf(fill="#f7f4f9") +
  geom_rect(aes(xmin=15250770, xmax=16363965, ymin=0, ymax=-1113195), 
            fill="purple", col=NA, lwd=0.1, alpha=0.008) +
  geom_rect(aes(xmin=8571601, xmax=15250770, ymin=-556597.5, ymax=556597.5), 
            fill="#5fa2ce", col=NA, lwd=0.1, alpha=0.008) +
  geom_rect(aes(xmin=3005626, xmax=8571601, ymin=-556597.5, ymax=556597.5), 
            fill="#ffbc79", col=NA, lwd=0.1, alpha=0.008) +
  geom_rect(aes(xmin=6345211, xmax=11911186, ymin=-556597.5, ymax=556597.5), 
            fill="#c85200", col="#c85210", lwd=0.6, alpha=0.01) +
  geom_text(data=labs, aes(x=long, y=lat, label=city), size=3,
            color="black", fontface="bold", check_overlap=FALSE) +
  labs(x="", y="") +
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

ggsave(plot=s1, filename="figures/sup_figure1.pdf", width=18, height=6)

# Figure S2: times series of explanatory/ predictor variables ####
# Plot river height
s2.par <- df.lepto %>%
  dplyr::select(date, prov, par.0) %>%
  mutate(date=as.Date(date),
         prov=str_replace_all(prov, c("Entre Ríos"="Paraná river (ER)", "Santa Fe"="Paraná river (SF)"))) %>% 
  ggplot() +
  geom_line(aes(date, par.0, col=prov), linewidth=1) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(breaks=c(1:6), labels=c(1:6)) +
  ggthemes::scale_colour_tableau("Color Blind") +
  labs(x="", y="River height (m/month)", col="") +
  theme_bw() +
  theme(legend.position=c(0.9,0.9),
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Plot precipitation
s2.prcp <- df.lepto %>%
  dplyr::select(date, prov, prcp.0) %>% 
  mutate(date=as.Date(date)) %>% 
  ggplot() +
  geom_line(aes(date, prcp.0, col=prov), linewidth=1) +
  geom_hline(yintercept=0, col="grey", linetype="dashed") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  labs(x="", y="Precipitation (mm/month)", col="") +
  theme_bw() +
  theme(legend.position=c(0.9,0.9),
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Plot Niño 3.4 index
s2.enso <- df.lepto %>%
  filter(prov=="Entre Ríos") %>% 
  dplyr::select(date, nino34.0) %>% 
  mutate(date=as.Date(date),
         nino34.0=scale(nino34.0)[,1],
         nino=ifelse(nino34.0>=0, nino34.0,0),
         nina=ifelse(nino34.0<0, nino34.0,0),
         event=ifelse(nino34.0>=0.5, "niño", "niña")) %>%
  select(date, nino34.0, nino, nina, event) %>%
  ggplot(aes(x=date, y=nino34.0)) +
  geom_hline(yintercept=c(0.5, -0.5), linetype="dashed", col="grey") +
  geom_ribbon(aes(ymax=nino, ymin=0), fill="red", alpha=0.7) +
  geom_ribbon(aes(ymax=nina, ymin=0), fill="blue", alpha=0.7) +
  geom_line(linewidth=.3) +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  labs(x="", y="Niño 3.4 Index", col="")+
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

s2 <- cowplot::plot_grid(s2.par, s2.prcp, s2.enso, nrow=3)

ggsave(plot=s2, filename="figures/sup_figure2.pdf", width=10, height=12)

# Figure S3: effect sizes of explanatory variables in candidate models ####
# Extract and plot fixed effects from candidate models in Entre Ríos
s3.er <- bind_rows(er.enso.fit$params[[6]],
                er.clim.fit$params[[18]]) %>%
  filter(!var=="(Intercept)") %>% 
  rename(lower=`0.025quant`,
         upper=`0.975quant`) %>%
  mutate(var=str_replace_all(var, c("par.1"="Paraná River",
                                    "nino34.3"="ENSO",
                                    "prcp.1"="Precipitation")),
         var=factor(var, levels=c("ENSO", "Paraná River", "Precipitation")),
         across(.cols=c(2:6), ~exp(.x))) %>% 
  ggplot(aes(x=var, y=mean)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3)) +
  coord_flip() +
  geom_hline(yintercept=1, alpha=0.2, linetype="dashed") +
  geom_vline(xintercept=1.5, linetype="dashed") +
  geom_text(aes(label=round(mean, 2), hjust=0, vjust=-1.5)) +
  labs(x="",
       y=expression("Exp (" ~beta[t]~ ")")) +
  theme_bw() +
  theme(legend.position="none",
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# Extract and plot fixed effects from candidate models in Santa Fe
s3.sf <- bind_rows(sf.enso.fit$params[[6]],
                sf.clim.fit$params[[18]]) %>%
  filter(!var=="(Intercept)") %>% 
  rename(lower=`0.025quant`,
         upper=`0.975quant`) %>%
  mutate(var=str_replace_all(var, c("par.1"="Paraná River",
                                    "nino34.3"="ENSO",
                                    "prcp.1"="Precipitation")),
         var=factor(var, levels=c("ENSO", "Paraná River", "Precipitation")),
         across(.cols=c(2:6), ~exp(.x))) %>% 
  ggplot(aes(x=var, y=mean)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), position=position_dodge(0.3)) +
  coord_flip() +
  geom_hline(yintercept=1, alpha=0.2, linetype="dashed") +
  geom_vline(xintercept=1.5, linetype="dashed") +
  geom_text(aes(label=round(mean, 2), hjust=0, vjust=-1.5)) +
  labs(x="",
       y=expression("Exp (" ~beta[t]~ ")")) +
  theme_bw() +
  theme(legend.position="none",
        legend.text=element_text(size=14, face="bold"),
        axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

s3 <- cowplot::plot_grid(s3.er, s3.sf, labels=c("a.", "b."))

ggsave(plot=s3, filename="figures/sup_figure3.pdf", width=10, height=4)

# Figure S4: plot observed versus predicted leptospirosis counts ####
# Extract and plot values from the posterior predictive distribution
# Entre Ríos
s4.er <- bind_rows(
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[1]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[1]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[1]], cols=c(1:1000), probs=0.975),
    mod="Random effects only"),
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.975),
    mod="ENSO"),
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.975),
    mod="Local climate")) %>%
  bind_cols(date=rep(df.er$date, 3)) %>%
  mutate(date=as.Date(date),
         mod=factor(mod, levels=c("Random effects only","ENSO","Local climate"))) %>% 
  ggplot() +
  geom_line(data=df.er, aes(as.Date(date), cases), col="grey80", linewidth=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, median, col=mod), linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"))

# Santa Fe
s4.sf <- bind_rows(
  data.frame(
    median=matrixStats::rowQuantiles(sf.preds$post.samples[[1]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(sf.preds$post.samples[[1]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(sf.preds$post.samples[[1]], cols=c(1:1000), probs=0.975),
    mod="Random effects only"),
  data.frame(
    median=matrixStats::rowQuantiles(sf.preds$post.samples[[3]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(sf.preds$post.samples[[3]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(sf.preds$post.samples[[3]], cols=c(1:1000), probs=0.975),
    mod="ENSO"),
  data.frame(
    median=matrixStats::rowQuantiles(sf.preds$post.samples[[4]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(sf.preds$post.samples[[4]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(sf.preds$post.samples[[4]], cols=c(1:1000), probs=0.975),
    mod="Local climate")) %>%
  bind_cols(date=rep(df.sf$date, 3)) %>%
  mutate(date=as.Date(date),
         mod=factor(mod, levels=c("Random effects only","ENSO","Local climate"))) %>% 
  ggplot() +
  geom_line(data=df.sf, aes(as.Date(date), cases), col="grey80", linewidth=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, median, col=mod), linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
  scale_x_date("", date_breaks="2 year", date_labels="%Y") +
  ggthemes::scale_colour_tableau("Color Blind") +
  ggthemes::scale_fill_tableau("Color Blind") +
  facet_wrap(~mod) +
  labs(y="", col="", fill="") +
  theme_bw() +
  theme(axis.text=element_text(size=14, face="bold"),
        axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.border=element_blank(),
        axis.line=element_line(color="black"),
        strip.background=element_rect(fill="white", color="white"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text=element_text(size=14, face="bold"))

s4 <- cowplot::plot_grid(s4.er, s4.sf, nrow=2, labels=c("a.", "b."))

ggsave(plot=s4, filename="figures/sup_figure4.pdf", width=18, height=10)

# Table S1: compute incidence per 100,000 inhabitants ####
df.lepto %>%
  group_by(year, prov) %>% 
  summarise(pop=unique(pop),
            cases=sum(cases),
            .groups="drop") %>%
  mutate(annual.rate=round((cases/pop)*100000, 2)) %>% 
  dplyr::select(year, prov, annual.rate) %>% 
  pivot_wider(names_from=prov, values_from=annual.rate)
  
# Tables S2 and S3: confusion matrix and measures of predictive ability ####
# Extract data from posterior probability distributions and create a binary 
# variable with outbreak predictions
data.er <- list(res=er.preds$outb.prob[[1]] %>% 
                  mutate(out.pred=ifelse(prob.out>=er.preds$trigger[[1]]$threshold, 1, 0)),
                enso=er.preds$outb.prob[[3]] %>% 
                  mutate(out.pred=ifelse(prob.out>=er.preds$trigger[[3]]$threshold, 1, 0)),
                local=er.preds$outb.prob[[5]] %>% 
                  mutate(out.pred=ifelse(prob.out>=er.preds$trigger[[5]]$threshold, 1, 0)))

data.sf <- list(res=sf.preds$outb.prob[[1]] %>% 
                  mutate(out.pred=ifelse(prob.out>=sf.preds$trigger[[1]]$threshold, 1, 0)),
                enso=sf.preds$outb.prob[[3]] %>% 
                  mutate(out.pred=ifelse(prob.out>=sf.preds$trigger[[3]]$threshold, 1, 0)),
                local=sf.preds$outb.prob[[4]] %>% 
                  mutate(out.pred=ifelse(prob.out>=sf.preds$trigger[[4]]$threshold, 1, 0)))

# Create a confusion matrix for each province
conf.er <- NULL
for(mod in c("res", "enso", "local")){
  conf.er[[mod]] <- caret::confusionMatrix(data=factor(data.er[[mod]]$out.pred),
                                           reference=factor(data.er[[mod]]$outbreak),
                                           positive="1")
}


conf.sf <- NULL
for(mod in c("res", "enso", "local")){
  conf.sf[[mod]] <- caret::confusionMatrix(data=factor(data.sf[[mod]]$out.pred),
                                           reference=factor(data.sf[[mod]]$outbreak),
                                           positive="1")
}

# Extract AUC values
list(res=er.preds$auc[[1]], enso=er.preds$auc[[3]], local=er.preds$auc[[5]])
list(res=sf.preds$auc[[1]], enso=sf.preds$auc[[3]], local=sf.preds$auc[[4]])

# Extract CRPS values
unlist(er.preds$crps)[c(1,3,5)]
unlist(sf.preds$crps)[c(1,3,4)]

# Compute CRPSS
(1-unlist(er.preds$crps)/er.preds$crps[[1]])[c(1,3,5)]
(1-unlist(sf.preds$crps)/sf.preds$crps[[1]])[c(1,3,4)]

## ############################################################################
## END
## ############################################################################

