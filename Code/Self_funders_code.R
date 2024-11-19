
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr,hrbrthemes, tidyverse,viridis, tidyr, stringr, emmeans, curl, estimatr, plm,ordinal, readxl, zoo, stringr, patchwork, cowplot, grid, ggeffects)



#####Output#####

# Read and preprocess the data
df <- read.csv("Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/foi residential june 2025.csv") %>%
  dplyr::filter(ratings_numeric != "",
                !is.na(latest_obs),
                !is.na(selffunders_perc_v2)
  )%>%
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
                ownership_bin = factor(ifelse(ownership_factor=="For-profit", "For-profit", "Other" )),
                ratings_bin = ifelse(ratings_factor=="Outstanding", 1, 
                                     ifelse(ratings_factor=="Good", 1, 0)))

df$ratings_bin <- factor(df$ratings_bin)

df <- df %>%
  dplyr::mutate(selffund_bin = factor(ifelse(selffunders_perc_v2>0.16,1,0)))















indiv <- df %>% dplyr::select(ratings_bin, selffunders_perc_v2,ownership_bin,idaopi_decile,old,nursing,number_residents,dementia_perc,mentalhealth_perc,disability_perc,learning_perc,howmanypeoplearedirectlyemployed,active,year)%>%
  dplyr::mutate(self_funders_n = round(selffunders_perc_v2* number_residents),
                state_funders_n = round(number_residents-self_funders_n)) %>%
  dplyr::mutate(carehomeid  = dplyr::row_number())%>%
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

# simpler_model <- lmerTest::lmer(ratings_bin ~ funding_type + ownership_bin + idaopi_decile + (1 | carehomeid), data=indiv)
# summary(simpler_model)


#olr2 <- lmerTest::lmer(as.numeric(ratings_bin)~ funding_type+(1+carehomeid|1), data=indiv)

#summary(olr2)


yes <- glm(ratings_bin~funding_type*ownership_bin*idaopi_decile+old, data=indiv, family="binomial" )

yes <- lm(ratings_bin~funding_type*ownership_bin*idaopi_decile+old, data=indiv)

yes2 <- as.data.frame(ggeffects::predict_response(yes, terms = c("funding_type[all]", "ownership_bin","idaopi_decile[all]"),
                                                  vcov_fun = "vcovCR", vcov_type = "CR2",
                                                  vcov_args = list(cluster = indiv$carehomeid)))



one <- ggplot(yes2, aes(x=as.numeric(facet), fill=x))+
  geom_smooth(aes(y=predicted, colour=x ), se = F, method="lm")+
  theme_bw()+
  labs(x="Deprivation decile", y="Probability of being Good or Outstanding", colour="Funding",fill="Funding",
       title = "Quality of care by individual funding status")+
  coord_cartesian(ylim = c(0.6,0.9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))+
  facet_wrap(~group)+
  geom_ribbon(data = yes2, aes(ymin = conf.low, ymax = conf.high, y = NULL, fill = x), alpha = 0.1)

oneone <- ggplot()+
  geom_bar(data=indiv[indiv$ownership_bin=="For-profit",], aes(x=as.numeric(idaopi_decile),  fill = funding_type), alpha=0.5, stat="count", position = position_dodge(width = 0.4)) +
  theme_nothing()

twotwo <- ggplot()+
  geom_bar(data=indiv[indiv$ownership_bin=="Other",], aes(x=as.numeric(idaopi_decile),  fill = funding_type), alpha=0.5, stat="count", position = position_dodge(width = 0.4)) +
  theme_nothing()

plot2 <- ggdraw() +
  draw_plot(one, 0, 0, 1, 1) +
  draw_plot(oneone,0.025,0.046,0.457,0.2)+
  draw_plot(twotwo,0.458,0.046,0.459,0.2)




ggsave(plot=plot2, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Self-funders/Figures/indiv_model_final.jpeg", width=14, height=8, dpi=600)


#write.csv(indiv, "indiv_data.csv")







