
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr,hrbrthemes,lmtest, tidyverse,viridis, tidyr, stringr, emmeans, curl, estimatr, plm,ordinal, readxl, zoo, stringr, patchwork, cowplot, grid, ggeffects)




#### Read and preprocess the data ####
df <- read.csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/foi residential june 2025.csv") %>%
  dplyr::filter(ratings_numeric != "",
                !is.na(selffunders_perc_v2))%>%
  dplyr::mutate(ratings_factor = factor(ratings_numeric, 
                                        levels =  c("Inadequate", "Requires Improvement", "Good", "Outstanding")),
                ownership_factor = factor(ownership,
                                          levels = c("For-profit", "Local Authority", "Third Sector")))%>%
  dplyr::mutate(quintile = ifelse(idaopi_decile == 1, 1, 
                                  ifelse(idaopi_decile == 2, 1,
                                         ifelse(idaopi_decile == 3, 2,
                                                ifelse(idaopi_decile == 4, 2,
                                                       ifelse(idaopi_decile == 5, 3,
                                                              ifelse(idaopi_decile == 6, 3,
                                                                     ifelse(idaopi_decile == 7, 4,
                                                                            ifelse(idaopi_decile == 8, 4,
                                                                                   ifelse(idaopi_decile == 9, 5,
                                                                                          ifelse(idaopi_decile == 10, 5,NA
                                                                                          )))))))))),
                ownership_bin = factor(ifelse(ownership_factor=="For-profit", "For-profit care homes", "LA and third sector care homes" )),
                ratings_bin = factor(ifelse(ratings_factor=="Outstanding", 1, 
                                            ifelse(ratings_factor=="Good", 1, 0))))


indiv <- df %>% dplyr::select(ratings_bin, latest_obs, selffunders_perc_v2,ownership_bin,idaopi_decile,old,nursing,number_residents,dementia_perc,mentalhealth_perc,disability_perc,learning_perc,howmanypeoplearedirectlyemployed,active,year, numeric_id)%>%
  dplyr::mutate(self_funders_n = round(selffunders_perc_v2* number_residents),
                state_funders_n = round(number_residents-self_funders_n)) %>%
  dplyr::mutate(carehomeid  = factor(numeric_id))%>%
  dplyr::filter(!is.na(latest_obs))%>%
  pivot_longer(
    cols = c(self_funders_n, state_funders_n),
    names_to = "funding_type",
    values_to = "funders_n"
  ) %>%
  mutate(funding_type = ifelse(funding_type == "self_funders_n", "Self-funded", "State-funded")) %>%
  uncount(funders_n)#%>%
#dplyr::select(ratings_bin,funding_type,ownership_bin,idaopi_decile,carehomeid)

indiv$ratings_bin <- as.numeric(as.character(indiv$ratings_bin))
indiv$idaopi_decile <- as.numeric(as.character(indiv$idaopi_decile))
indiv$funding_type <- as.factor(indiv$funding_type)
indiv$ownership_bin <- as.factor(indiv$ownership_bin)
indiv$carehomeid <- as.factor(indiv$carehomeid)

#save data
write.csv(indiv, "Data_for_anders.csv")

####Model and probabilities####
#yes <- glm(ratings_bin~funding_type*ownership_bin*idaopi_decile+old, data=indiv, family="binomial" )

yes <- lm(ratings_bin~funding_type*ownership_bin*idaopi_decile+old+year, data=indiv)

yes2 <- as.data.frame(ggeffects::predict_response(yes, terms = c("funding_type[all]", "ownership_bin","idaopi_decile[all]"),
                                                  vcov_fun = "vcovCR", vcov_type = "CR2",
                                                  vcov_args = list(cluster = indiv$carehomeid)))


####Plot####
one <- ggplot(yes2, aes(x=as.numeric(facet), fill=x))+
  geom_smooth(aes(y=predicted, colour=x ), se = F, method="lm")+
  theme_bw()+
  labs(x="Deprivation decile", y="Probability of being Good or Outstanding", colour="Funding",fill="Funding",
       title = "Quality of care by individual funding status and care home ownership")+
  coord_cartesian(ylim = c(0.64,0.92))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10),
                     labels = c("Most\nDeprived","2","3","4","5","6","7","8","9","Least\nDeprived"))+
  facet_wrap(~group)+
  geom_ribbon(data = yes2, aes(ymin = conf.low, ymax = conf.high, y = NULL, fill = x), alpha = 0.1)

oneone <- ggplot()+
  geom_bar(data=indiv[indiv$ownership_bin=="For-profit care homes",], aes(x=as.numeric(idaopi_decile),  fill = funding_type), alpha=0.5, stat="count", position = position_dodge(width = 0.4)) +
  theme_nothing()+
  coord_cartesian(ylim = c(0, 30000))

twotwo <- ggplot()+
  geom_bar(data=indiv[indiv$ownership_bin=="LA and third sector care homes",], aes(x=as.numeric(idaopi_decile),  fill = funding_type), alpha=0.5, stat="count", position = position_dodge(width = 0.4)) +
  labs(y="Individuals (n)\n")+
  theme(
    axis.text.x  =          element_blank(),
    axis.title.x =         element_blank(),
    panel.grid.major  =   element_blank(),
    panel.grid.minor  =   element_blank(),
    axis.ticks.length   =  unit(0, "cm"),
    panel.spacing  =      unit(0, "lines"),
    plot.margin =        unit(c(0, 0, 0, 0), "lines"),
    legend.position =    "none",
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA)    
  )+
  scale_y_continuous(position = "right")+
  coord_cartesian(ylim = c(0, 30000))



plot2 <- ggdraw() +
  draw_plot(one, 0, 0, 1, 1) +
  draw_plot(oneone,0.025,0.0570,0.457,0.35)+
  draw_plot(twotwo,0.458,0.0570,0.5,0.35)





ggsave(plot=plot2, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Self-funders/Figures/indiv_model_final.jpeg", width=14, height=8, dpi=600)


#write.csv(indiv, "indiv_data.csv")







