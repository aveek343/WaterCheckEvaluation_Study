# This script creates 5 plots provided (Number 2 to 6) in in the Appendix 1 (Figures)
# Author: Mahmud Aveek, David Rosenberg
# Date: 06-09-2024
# Runtime: 30 seconds
###############################################
# Appendix figure 2: Total water use during pre-Water Check period

# pre-water check water use indoor vs outdoor
ev_data_labeled %>%
  relocate(SiteID) %>% #View()
  mutate(week = isoweek(datetime)) %>%
  select(week, volume_gal, SiteID, label) %>%
  group_by(SiteID,week,label) %>%
  filter(SiteID %in% unique(sites_with_pre_post$SiteID)) %>% # filter data for sites that has pre and post
  summarize(weekly_vol = sum(volume_gal)) %>%
  left_join(WaterCheckData %>%  # import water check dates
              mutate(waterCheckWeek = isoweek(WaterCheckDate)) %>%
              select(SiteID, waterCheckWeek)) %>% 
  mutate(pre_post = case_when(week <= waterCheckWeek ~ 'pre',
                              week > waterCheckWeek ~ 'post')) -> plot_weekly_all_waterUse_data

plot_weekly_all_waterUse_data %>%
  left_join(Sites %>% select(SiteID,City)) %>% 
  ungroup() %>%
  select(pre_post,label, City, weekly_vol) %>%
  pivot_longer(-c(pre_post:City)) %>% 
  group_by(pre_post, City, label) %>%
  summarize(vol = sum(value)) %>%
  ungroup() %>%
  group_by(pre_post,City) %>%
  mutate(total_vol = sum(vol),
         pct =vol/total_vol ) %>%
  filter( pre_post == 'pre') -> pre_waterCheck_waterUse_irrigation_other

pre_waterCheck_waterUse_irrigation_other %>%
  ggplot()+
  geom_bar(aes(x = label, y = vol/1000, group = pre_post, fill = label), 
           stat = 'identity', position = 'dodge') +
  facet_wrap(~City) + 

  scale_fill_manual("Irrigation and Indoor Water use",
                    values = c("#eb3483", "#4334eb"), labels = c("Irrigation", "Indoor")) + 
  scale_x_discrete(labels=c("irrigation" = "Irrigation", "other" = "Indoor"))+
  labs(x= "Water use type", y = "Total water use during\npre-Water Check period (Kgal)") +
  theme_classic()+ 
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        legend.position = "none",
        strip.text.x = element_text(size = 15, colour = "black", angle = 0))
# pre-water check water use indoor vs outdoor
plot_weekly_all_waterUse_data %>%
  left_join(Sites %>% select(SiteID,City)) %>% 
  ungroup() %>%
  select(pre_post,label, City, weekly_vol) %>%
  pivot_longer(-c(pre_post:City)) %>% 
  group_by(pre_post, City, label) %>%
  summarize(vol = sum(value)) %>%
  ungroup() %>%
  group_by(pre_post,City) %>%
  mutate(total_vol = sum(vol),
         pct =vol/total_vol ) %>%
  filter( pre_post == 'pre') -> pre_waterCheck_waterUse_irrigation_other

pre_waterCheck_waterUse_irrigation_other %>%
  ggplot()+
  geom_bar(aes(x = label, y = vol/1000, group = pre_post, fill = label), 
           stat = 'identity', position = 'dodge') +
  facet_wrap(~City) + 
  
  # annotate("text", x = 1, y = 750, label = "a")+ 
  # annotate("text", x = 2, y = 25, label = "b")+ 
  # annotate("text", x = 1, y = 25, label = "c")+
  # annotate("text", x = 2, y = 25, label = "d")+
  
  scale_fill_manual("Irrigation and Indoor Water use",
                    values = c("#eb3483", "#4334eb"), labels = c("Irrigation", "Indoor")) + 
  scale_x_discrete(labels=c("irrigation" = "Irrigation", "other" = "Indoor"))+
  labs(x= "Water use type", y = "Total water use during\npre-Water Check period (Kgal)") +
  theme_classic()+ 
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        legend.position = "none",
        strip.text.x = element_text(size = 15, colour = "black", angle = 0)) -> preWaterCheck_indoor_Vs_outdoor_vol

plot(preWaterCheck_indoor_Vs_outdoor_vol)
###############################################
# Appendix figure 3: overall water use scenario during pre-water check period

# city-wise pre-water check water use behavior
df_daily %>% filter(SiteID %in% sites_with_pre_post_df$SiteID) %>% 
  left_join(Sites %>% select(SiteID, City)) %>%
  filter(pre_post == "pre") %>% 
  select(SiteID,City,n_eve, dvol,dmin,d_lastirr) %>% 
  pivot_longer(-c(SiteID, City), names_to = "variable") %>% 
  arrange(variable) %>% 
  mutate(new_variable = case_when(variable == "n_eve" ~ "Daily irrigation events (number)",
                                  variable == "d_lastirr" ~ "Days between irrigation (day)",
                                  variable == "dmin" ~ "Daily irrigation  time (min)",
                                  variable == "dvol" ~ "Daily irrigation volume (gallon)")) %>% # View()
  ggplot() +
  
  geom_boxplot(aes(x=as.factor(SiteID), value, color = new_variable, group = SiteID), 
               outlier.colour="black", 
               outlier.shape=16,
               outlier.size=2, notch=FALSE) + 
  
  facet_grid(new_variable~City, 
             scale= "free")+
  
  scale_color_manual("Variables", values = c("#eb3483", "cyan","#4334eb","#96CEF5"), 
                     label = c("Daily irrigation events (number)", "Days between irrigation (day)","Daily irrigation time (min)",
                               "Daily irrigation water volume (gal)"))+ 
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  
  labs ( x ="SiteID")+
  
  theme(legend.position = "none") -> preWaterCheckDailyWaterUseBehaviorValues

plot(preWaterCheckDailyWaterUseBehaviorValues)

###############################################
# Figure 5: KS-test for four water use variables between pre- and post-water check periods
# variables: dvol, dmin, d_lastirr, n_eve
individualDailyVolumeKSTest_plot <- ks_test_plots_siteWise(15, 'dvol')
plot(individualDailyVolumeKSTest_plot)
individualEventKSTest_plot <- ks_test_plots_siteWise(15, 'n_eve')
plot(individualEventKSTest_plot)
individualDaysLastIrrKSTest_plot <- ks_test_plots_siteWise(15,'d_lastirr')
plot(individualDaysLastIrrKSTest_plot)
individualDailyIrrDurationtKSTest_plot <- ks_test_plots_siteWise(15,'dmin')
plot(individualDailyIrrDurationtKSTest_plot)


