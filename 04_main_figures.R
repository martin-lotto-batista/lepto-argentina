## ############################################################################
##
## Towards a Leptospirosis Early Warning System in North-Eastern Argentina
## Lotto Batista, M, Rees E., et al., 2023
##
## SCRIPT 04: Create main figures and tables
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

# Data
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

# Figure 2a: Map of the study area ####
# Shapefiles
# South America's contour
border <- readRDS("data/south_america_full.rds") %>% 
  mutate(a=0) %>% group_by(a) %>% summarise(.groups="drop")

# Provinces
study.area <- readRDS("data/stafe_enrios.rds") %>%  
  group_by(NAME_1) %>% 
  summarise(.groups="drop")

# Province names
prov.names <- cbind(study.area,st_coordinates(st_centroid(study.area$geometry)))

# Paraná river
map.riv <- readRDS("data/shp_riv.rds") %>% 
  filter(!grepl("Uru|Guale|Pla|Pal", fna)) %>% 
  st_crop(c(xmin=-60.80564,ymin=-34.22840,xmax=-57,ymax=-27.7))

# South America with study area
fig2a.sa <- ggplot() +
  geom_sf(data=border, fill="#f7f4f9", color="white", lwd=0) +  # South America map
  geom_sf(data=border, fill=NA, lwd=0, color="#d4b9da") +  # South America border
  geom_sf(data=study.area, fill="#e7e1ef", color="white", lwd=0, alpha=0.5) +  # Provinces
  geom_rect(aes(xmin=-64, xmax=-57, ymin=-36, ymax=-27),   # Rectangle around provinces
            fill=NA, col="black", lwd=0.2) +
  theme_void()
  
# Study area
stations <- data.frame(station=c("Sauce Viejo","Ceres","Reconquista","Rafaela", 
                                 "Rosario","Concordia","Gualeguaychú","Paraná"),
                       long=c(-60.807824,-61.946310,-59.659124,-61.490189,
                              -60.677072,-58.014574,-58.516761,-60.487925),
                       lat=c(-31.709946,-29.883555,-29.163602,-31.254377,
                             -32.953630,-31.385735,-33.008895,-31.777752))

par.r <- data.frame(riv=c("Paraná River"),
                    long=c(-59.65),
                    lat=c(-29.88))

fig2a.area <- ggplot() +
  geom_sf(data=study.area, fill="#f7f4f9", color="#d4b9da") +   # Provinces
  geom_sf(data=map.riv, col="#38afcd", fill="#38afcd") +  # Paraná River
  geom_point(data=stations, aes(x=long, y=lat),   # Stations
             size=2, col="#980043", fill="#980043") +
  geom_text(data=prov.names, aes(x=X, y=Y, label=NAME_1), size=6,  # Province names
            color="#d4b9da", fontface="bold", check_overlap=FALSE) +
  ggrepel::geom_text_repel(data=par.r, aes(x=long, y=lat, label=riv), 
                           nudge_x=1.8, nudge_y=0.5, size=5, fontface="bold") + # River name
  theme_void() +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0.5,0), "cm")) +
  coord_sf(xlim=c(-65,-57.8),
           ylim=c(-34.7, -27.99393)) +
  ggspatial::annotation_north_arrow(location="tl",
                                    which_north="true", 
                                    style=north_arrow_orienteering(), 
                                    height=unit(0.75, "cm"), 
                                    width=unit(0.75, "cm")) +
  ggspatial::annotation_scale(location="br", pad_x=unit(0, "cm"), line_width=0.5)

# Mosaic map
fig2a <- cowplot::ggdraw(fig2a.area) +
  cowplot::draw_plot(fig2a.sa, 
                     x=0.01, y=0.01,
                     width=0.3,
                     height=0.3)

# Figure 2b: time series of letpospirosis incidence ####
fig2b <- df.lepto %>% 
  dplyr::select(1:6) %>% 
  mutate(date=as.Date(date),
         inc=(cases/pop)*100000) %>% 
  ggplot() +
  geom_bar(aes(date, inc, fill=prov), stat="identity", position="dodge") +
  labs(fill="", x="",y="Cases per 100,000 inhabitants") +
  ggthemes::scale_fill_tableau("Color Blind") +
  scale_x_date(date_breaks="1 year", date_labels="%Y") +
  facet_wrap(~prov, nrow=2, scales="free")+
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
        strip.text=element_text(size=14, face="bold"),
        plot.margin=unit(c(1,1,0,0), "cm"))

# Combine figure 1a with figure 1b
fig2 <- cowplot::plot_grid(fig2a, NULL, fig2b, rel_widths=c(0.5,0.1,1), 
                           nrow=1, labels=c("a.", "b."))

ggsave(plot=fig2, filename="figures/figure2.pdf", width=18, height=6)

# Figure 3: time series observed versus fitted leptospirosis counts ####
# Extract fitted values for each model in Entre Ríos
fig3.er <- er.enso.fit$fits[[2]] %>% 
  mutate(mod="Random effects only") %>% 
  bind_rows(er.enso.fit$fits[[6]] %>% 
              mutate(mod="ENSO"),
            er.clim.fit$fits[[18]] %>% 
              mutate(mod="Local climate")) %>%
  `rownames<-`(NULL) %>%
  dplyr::select(mean, `0.025quant`, `0.975quant`, mod) %>%
  magrittr::set_colnames(c("mean", "lower", "upper", "mod")) %>% 
  bind_cols(date=rep(df.er$date, 3)) %>%
  mutate(across(1:3, ~.x*(df.er$pop/100000)),
         mod=factor(mod, levels=c("Random effects only","ENSO","Local climate")),
         date=as.Date(date)) %>%
  ggplot() +
  geom_line(data=df.er, aes(as.Date(date), cases), col="grey80", linewidth=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, mean, col=mod), linetype="dashed", linewidth=0.7) +
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
        strip.text=element_text(size=14, face="bold"),
        plot.margin=unit(c(1,1,0,0), "cm"))

# Extract fitted values for each model in Santa Fe
fig3.sf <- sf.enso.fit$fits[[2]] %>% 
  mutate(mod="Random effects only") %>% 
  bind_rows(sf.enso.fit$fits[[6]] %>% 
              mutate(mod="ENSO"),
            sf.clim.fit$fits[[18]] %>% 
              mutate(mod="Local climate")) %>%
  `rownames<-`(NULL) %>% 
  dplyr::select(mean, `0.025quant`, `0.975quant`, mod) %>%
  magrittr::set_colnames(c("mean", "lower", "upper", "mod")) %>% 
  bind_cols(date=rep(df.sf$date, 3)) %>%
  mutate(across(1:3, ~.x*(df.sf$pop/100000)),
         mod=factor(mod, levels=c("Random effects only","ENSO","Local climate")),
         date=as.Date(date)) %>%
  ggplot() +
  geom_line(data=df.er, aes(as.Date(date), cases), col="grey80", linewidth=0.7) +
  scale_color_manual(values=c("Observed"="grey80")) +
  geom_line(aes(date, mean, col=mod), linetype="dashed", linewidth=0.7) +
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
        strip.text=element_text(size=14, face="bold"),
        plot.margin=unit(c(1,1,0,0), "cm"))

fig3 <- cowplot::plot_grid(fig3.er, fig3.sf, nrow=2, labels=c("a.", "b."))

ggsave(plot=fig3, filename="figures/figure3.pdf", width=18, height=10)

# Figure 4: Plot ROC curves ####
# Extract trigger probabilities for each province
trig.er <- bind_rows(er.preds$trigger) %>% 
  slice(c(1,3,5)) %>% 
  mutate(model=c("Monthly random effects", "ENSO", "Local climate"))

trig.sf <- bind_rows(sf.preds$trigger) %>% 
  slice(c(1,3,4)) %>% 
  mutate(model=c("Monthly random effects", "ENSO", "Local climate"))

# Extract values from roc curves
roc.er <- data.frame(model=rep(c("Monthly random effects", "ENSO", "Local climate"), 
                               times=c(94,73,71)), # The number of rows changes with every new run
                     sens=c(er.preds$roc[[1]]$sensitivities,
                            er.preds$roc[[3]]$sensitivities,
                            er.preds$roc[[5]]$sensitivities),
                     spec=1-c(er.preds$roc[[1]]$specificities,
                              er.preds$roc[[3]]$specificities,
                              er.preds$roc[[5]]$specificities)) %>% 
  arrange(sens)

fig4.er <- roc.er %>% 
  ggplot(aes(spec, sens, group=model, col=model)) +
  geom_line() +
  ggthemes::scale_color_tableau("Color Blind") +
  geom_segment(aes(x=0, xend=1, y=0, yend=1), color="darkgrey", linetype="dashed") +
  geom_point(data=trig.er, aes(x=1-specificity, y=sensitivity, col=model),
             shape=1, size=5) +
  geom_text(data=trig.er, aes(x=(1-specificity)+c(0.05,0.05,-0.05), 
                              y=sensitivity+c(-0.05,-0.05,0.05), 
                              label=round(threshold, 2),
                              col=model), 
            size=4,
            fontface="bold",
            inherit.aes=FALSE,
            show.legend=FALSE) +
  labs(x="FAR", y="HR", col="") +
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

roc.sf <- data.frame(model=rep(c("Monthly random effects", "ENSO", "Local climate"), 
                               times=c(110,107,99)), # The number of rows changes with every new run
                     sens=c(sf.preds$roc[[1]]$sensitivities,
                            sf.preds$roc[[3]]$sensitivities,
                            sf.preds$roc[[4]]$sensitivities),
                     spec=1-c(sf.preds$roc[[1]]$specificities,
                              sf.preds$roc[[3]]$specificities,
                              sf.preds$roc[[4]]$specificities)) %>% 
  arrange(sens)

fig4.sf <- roc.sf %>% 
  ggplot(aes(spec, sens, group=model, col=model)) +
  geom_line() +
  ggthemes::scale_color_tableau("Color Blind") +
  geom_segment(aes(x=0, xend=1, y=0, yend=1), color="darkgrey", linetype="dashed") +
  geom_point(data=trig.sf, aes(x=1-specificity, y=sensitivity, col=model), shape=1, size=5) +
    geom_text(data=trig.sf, aes(x=(1-specificity)+c(0.05,0.04,-0.04), 
                                y=sensitivity+c(-0.05,-0.045,0.05), 
                                label=round(threshold, 2),
                                col=model), 
              size=4,
              fontface="bold",
              inherit.aes=FALSE,
              show.legend=FALSE) +
  labs(x="FAR", y="HR", col="") +
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

fig4.leg <- cowplot::get_legend(fig4.sf+theme(legend.position="bottom", 
                                    legend.direction="horizontal",
                                    legend.text=element_text(size=14, face="bold"),
                                    legend.key.size=unit(1, "cm")))

fig4.up <- cowplot::plot_grid(fig4.er, fig4.sf, nrow=1, labels=c("a.", "b."))
fig4 <- cowplot::plot_grid(fig4.up, fig4.leg, nrow=2, rel_heights=c(1,0.1))

ggsave(plot=fig4, filename="figures/figure4.pdf", width=12, height=5)

# Figure 5: Plot outbreak probabilities in March 2010 ####
# Extract outbreak probability for the period of interest (March 2010)
bind_rows(er.preds$outb.prob[[3]] %>% mutate(mod="ENSO model"),
          er.preds$outb.prob[[5]] %>% mutate(mod="Local model")) %>% 
  filter(date == "2010-03-01") %>% 
  dplyr::select(mod, prob.out)

# ENSO average outbreak prob in March 2010: 84% (changes with every new run)
# Local climate outbreak prob in March 2010: 89% (changes with every new run)

# Extract values from the posterior predictive distributions in each province
fig5.dt <- bind_rows(
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[3]], cols=c(1:1000), probs=0.975),
    threshold=er.preds$outb.prob[[3]]$thresh,
    mod="ENSO"),
  data.frame(
    median=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.5),
    lower=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.025),
    upper=matrixStats::rowQuantiles(er.preds$post.samples[[5]], cols=c(1:1000), probs=0.975),
    threshold=er.preds$outb.prob[[5]]$thresh,
    mod="Local climate")) %>%
  bind_cols(date=as.Date(rep(df.er$date, 2), format="%Y-%m-%d")) %>%
  mutate(mod=factor(mod, levels=c("ENSO","Local climate")))

plot.out <- function(filt, text, max.rect=NA){
  
  obs <- df.er %>% 
    mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
    filter(date %in% c(as.Date("2009-01-01", 
                               format="%Y-%m-%d"):as.Date("2013-06-15", 
                                                          format="%Y-%m-%d"))) %>% 
    select(date, cases)
  
  fig5.dt %>% 
    filter(mod==filt,
           date %in% c(as.Date("2009-01-01", 
                               format="%Y-%m-%d"):as.Date("2013-06-15", 
                                                          format="%Y-%m-%d"))) %>%
    ggplot() +
    geom_line(data=obs, aes(date, cases), col="grey80", linewidth=0.7) +
    geom_line(aes(date, threshold), linetype="dashed", linewidth=0.7, col="black", alpha=0.5) +
    geom_line(aes(date, median, col=mod), linetype="dashed", linewidth=0.7) +
    geom_ribbon(aes(x=date, ymin=lower, ymax=upper, fill=mod), alpha=0.2) +
    scale_x_date("", date_breaks="1 year", date_labels="%Y") +
    geom_rect(data=data.frame(xmin=as.Date("2010-02-15", format="%Y-%m-%d"),
                              xmax=as.Date("2010-04-15", format="%Y-%m-%d"),
                              ymin=-0.5,
                              ymax=max.rect),
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill=NA, col="grey20", linewidth=1) +
    annotate(geom="text",
             x=as.Date("2010-07-01", format="%Y-%m-%d"),
             y=max.rect-5,
             label=paste0("Prob(Outbreak):", text),
             hjust=0,
             size=8) +
    ylim(-0.5, 70) +
    ggthemes::scale_colour_tableau("Color Blind") +
    ggthemes::scale_fill_tableau("Color Blind") +
    labs(y="Number of cases", col="", fill="") +
    theme_bw() +
    theme(axis.text=element_text(size=16, face="bold"),
          axis.text.x=element_text(angle=45, hjust=1),
          axis.title=element_text(size=16, face="bold"),
          legend.position="none",
          panel.border=element_blank(),
          axis.line=element_line(color="black"),
          strip.background=element_rect(fill="white", color="white"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
} 

fig5.pt1 <- plot.out("ENSO", " 84%", 53)
fig5.pt2 <- plot.out("Local climate", " 89%", 68)

fig5 <- cowplot::plot_grid(fig5.pt1, fig5.pt2)

ggsave(plot=fig5, filename="figures/figure5.pdf", width=18, height=7)

# Table 1: goodness of fit statistics ####
# Extract goodness of fit statistics from model outputs
# Entre Ríos
er.enso.fit$gof %>% slice(1,2,6) %>% dplyr::select(form, dic, rsq.re, rsq.null)
er.clim.fit$gof %>% slice(18) %>% dplyr::select(form, dic, rsq.re, rsq.null)
# Santa Fe
sf.enso.fit$gof %>% slice(1,2,6) %>% dplyr::select(form, dic, rsq.re, rsq.null)
sf.clim.fit$gof %>% slice(18) %>% dplyr::select(form, dic, rsq.re, rsq.null)

# Table 2: AUC and confidence interval ####
# Extract AUC for outbreak detection
# Entre Ríos
er.preds$auc %>%
  bind_rows() %>% 
  filter(mod %in% c(1,3,5))

# Santa Fe
sf.preds$auc %>%
  bind_rows() %>% 
  filter(mod %in% c(1,3,4))

## ############################################################################
## END
## ############################################################################