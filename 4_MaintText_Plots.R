# This script creates 5 plots provided in the main text of the paper 
# Author: Mahmud Aveek
# Date: 06-09-2024
# Runtime: 30 seconds
##################################################################
# Following codes are for viewing water events together (Figure 1)

# Use the following function to separate a indoor and a irrigation event
# The function takes in an eventid (check ev_data_labeled) and site_id. Use ev_data_labeled dataframe to select an event.
plot_ev_site_df <- function(eventid, siteid){
  # eventid = 1
  # siteid = 1
  site_data <- ev_data_labeled %>% filter(SiteID == siteid) #%>% filter(label == "irrigation")
  site_data
  i = which(site_data$EventID == eventid)
  end_time = site_data$datetime[i] + (site_data$duration_min[i] * 60)
  
  s_title = paste0('Date = ', as.Date(site_data$datetime[i]), '\n', 
                   'Label = ', site_data$label[eventid], '\n', 
                   'Volume (Gal) = ', round(site_data$volume_gal[i],0), '\n', 
                   'Duration (min) = ', round(site_data$duration_min[i],0), '\n', 
                   'Participant #: ', siteid, '\n'
                   #'City = ', FlumePersonalData$City[siteid]
  )
  
  data_evid_all %>%
    filter(SiteID == siteid) %>%
    select(id, datetime, VolumeGal) %>%
    filter(id == eventid) %>%
    #filter(VolumeGal < 7) %>%
    group_modify(~ add_row(.x, .before=0)) %>% # add a row with to fill up the 0 at the beginning
    fill(datetime, .direction = "downup") %>%
    mutate(datetime = if_else(is.na(VolumeGal), datetime - 5, datetime)) %>%
    mutate(VolumeGal = replace_na(VolumeGal, 0)) %>% 
    group_modify(~ add_row(.x, .after = max(nrow(.x)))) %>% 
    fill(datetime, .direction = "downup") %>%
    mutate(datetime = if_else(is.na(VolumeGal), datetime + 5, datetime)) %>%
    mutate(VolumeGal = replace_na(VolumeGal, 0)) %>%
    #add_column(label = site_data$label[id_ev]) %>%
    mutate(tdiff = as.numeric(difftime(datetime, lag(datetime), units = "secs"))) %>%
    mutate(tdiff = replace_na(tdiff, 0)) %>%
    mutate(n = cumsum(tdiff))%>%
    mutate(hour = as.numeric(format(datetime, "%H")),
           hour = ifelse(hour == 0 , 24, hour),
           diffH = hour - min(hour),
           general_hour = as.numeric(6 + diffH),
           minute = as.numeric(format(datetime, "%M")),
           second = as.numeric(format(datetime, "%S")),
           genericTime = as_datetime(strftime(paste0("2022-06-01"," ",general_hour, ":", minute, ":",
                                                     second), format = "%Y-%m-%d %H:%M:%S"))) %>%
    select(id, genericTime, VolumeGal) -> p
  
  return(p)
  
}


irrigation_df <- plot_ev_site_df(88,20) %>%
  mutate(variable = 'Irrigation')
other_indoor_df <- plot_ev_site_df(341,29) %>%
  mutate(variable = 'Indoor')


dataframe = rbind(irrigation_df,other_indoor_df)

dataframe  %>%
  ggplot(aes(genericTime, VolumeGal * (60/5), group = variable, color = variable)) + 
  geom_line(size=1) +
  scale_color_manual("", values = c("#4334eb", "#eb3483"), 
                     label = c("Indoor use", "Sprinkler Irrigation"))+
  labs(y = 'Flow Rate (GPM)', x = 'Time') +
  theme_classic()+
  theme(legend.position = "top",
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=15)) -> inOut_event_comparison_plot

plot(inOut_event_comparison_plot)

# Indoor vs Outdoor in Liter
# The function takes in an eventid (check ev_data_labeled) and site_id. Use ev_data_labeled to select an event.
plot_ev_site_df_lpm <- function(eventid, siteid){
  site_data <- ev_data_labeled %>% filter(SiteID == siteid)
  i <- which(site_data$EventID == eventid)
  end_time <- site_data$datetime[i] + (site_data$duration_min[i] * 60)
  
  s_title <- paste0(
    "Date = ", as.Date(site_data$datetime[i]), "\n",
    "Label = ", site_data$label[i], "\n",
    "Volume (L) = ", round(site_data$volume_gal[i] * 3.78541, 0), "\n",
    "Duration (min) = ", round(site_data$duration_min[i], 0), "\n",
    "Participant #: ", siteid, "\n"
  )
  
  data_evid_all %>%
    filter(SiteID == siteid) %>%
    select(id, datetime, VolumeGal) %>%
    filter(id == eventid) %>%
    group_modify(~ add_row(.x, .before = 0)) %>%
    fill(datetime, .direction = "downup") %>%
    mutate(datetime = if_else(is.na(VolumeGal), datetime - 5, datetime)) %>%
    mutate(VolumeGal = replace_na(VolumeGal, 0)) %>%
    group_modify(~ add_row(.x, .after = max(nrow(.x)))) %>%
    fill(datetime, .direction = "downup") %>%
    mutate(datetime = if_else(is.na(VolumeGal), datetime + 5, datetime)) %>%
    mutate(VolumeGal = replace_na(VolumeGal, 0)) %>%
    mutate(
      tdiff = replace_na(as.numeric(difftime(datetime, lag(datetime), units = "secs")), 0),
      n = cumsum(tdiff),
      hour = as.numeric(format(datetime, "%H")),
      hour = ifelse(hour == 0, 24, hour),
      diffH = hour - min(hour),
      general_hour = as.numeric(6 + diffH),
      minute = as.numeric(format(datetime, "%M")),
      second = as.numeric(format(datetime, "%S")),
      genericTime = as_datetime(strftime(
        paste0("2022-06-01"," ",general_hour, ":", minute, ":", second),
        format = "%Y-%m-%d %H:%M:%S"
      )),
      VolumeL = VolumeGal * 3.78541                 # convert to liters
    ) %>%
    select(id, genericTime, VolumeL) -> p
  
  return(p)
}

# Build the two example series
irrigation_df_lpm  <- plot_ev_site_df_lpm(88, 20) %>%  mutate(variable = "Irrigation")
other_indoor_df_lpm <- plot_ev_site_df_lpm(341, 29) %>% mutate(variable = "Indoor")

dataframe_lpm <- bind_rows(irrigation_df_lpm, other_indoor_df_lpm)

# Plot: liters per minute (LPM)
inOut_event_comparison_plot_lpm <- dataframe_lpm %>%
  ggplot(aes(genericTime, VolumeL * (60/5), group = variable, color = variable)) +
  geom_line(size = 1) +
  scale_color_manual("", values = c("#4334eb", "#eb3483"),
                     labels = c("Indoor use", "Sprinkler Irrigation")) +
  labs(y = "Flow rate (L/min)", x = "Time") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title   = element_text(size = 15)
  )

plot(inOut_event_comparison_plot_lpm)

##################################################################
# Following codes can create Figure 2 (Participant enrollment and praticipation in the different study phases)
# filtering only the households that participated the study
sites_with_pre_post_df <- data.frame(SiteID = unique(sites_with_pre_post$SiteID))

# Participant list
sites_with_pre_post_df %>%
  mutate(behavior_change_analysis = 1) -> sites_with_pre_post_df
# Water Check completed
WaterCheckData %>%
  mutate(water_check_completed = 1) %>%
  select(SiteID, water_check_completed)-> sites_with_waterCheck_completed
# Inverview completed
Interview_completed <- data.frame(SiteID = c(8,21,15,38,65,62,44,68,19,28), 
                                  interviewCompleted = rep(1,10))


# Combine all participant data ind different phases
FlumePropertyData %>%
  select(SiteID) %>%
  left_join(Sites) %>% select(SiteID, City) %>% 
  filter(City == "Logan" | City == "Hyde Park") %>%
  mutate(flume_installed = 1) %>%
  left_join(sites_with_waterCheck_completed) %>%
  left_join(sites_with_pre_post_df) %>%
  left_join(Interview_completed) %>%
  gather(Variable, Value, flume_installed:interviewCompleted) %>% na.omit() %>%  
  group_by(City, Variable) %>%
  summarize(value = n()) %>%
  mutate(bar_plot_priority= ifelse(Variable == "flume_installed", 1,
                                   ifelse(Variable == "water_check_completed", 2,
                                          ifelse(Variable == "behavior_change_analysis", 3,4))))%>% 
  arrange(bar_plot_priority)%>% 
  ggplot(aes(x = City, y = as.factor(value), group = as.factor(bar_plot_priority), fill = as.factor(Variable))) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual("Status", values = c("#eb3483", "#4334eb","#96CEF5","#96F1F5"), 
                    label = c("Flume installed","Water Check completed",
                              "Considered for\nbehavior change analysis","Interview completed"),
                    breaks=c('flume_installed','water_check_completed',
                             'behavior_change_analysis','interviewCompleted'))+
  labs(y= "number of properties", x = "city")+
  geom_text(aes(label=value),size = 10, position=position_dodge(width=0.9), vjust=-0.5)+ 
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 15),
        axis.title.x = element_text(color="black", size=15, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15)) -> participantBarPlot

plot(participantBarPlot)
##################################################################
# Following codes can create Figure 3 (Pre- and post-Water Check volume ECDFs (Solid lines) show decreasing water use post-water check with 99% confidence. There is a similar decrease in post-Water Check water budget (Dotted lines))

prePost_waterVol_budget_comparison_with_KSTest <- KS_test_weekly_function_advanced(variable = "weekly_volume_budgetGal", city = "All")
plot(prePost_waterVol_budget_comparison_with_KSTest)

prePost_waterVol_budget_comparison_with_KSTest_liter <- KS_test_weekly_function_advanced_liter(variable = "weekly_volume_budgetGal", city = "All")
plot(prePost_waterVol_budget_comparison_with_KSTest_liter)



##################################################################
# Following codes can create Figure 4 (ECDFs of pre- and post-Water Check water use volume for high (red) and low (blue) users before (dotted) and after (solid) a water check
# weekly volume liter
library(scales)

weekly_volumetric_df_with_pre_wc_budget_comparison %>%
  filter(name == "weekly_volume") %>%
  mutate(new_variable = paste0(pre_post, "_", user_cat)) %>%
  ggplot(aes(x = value * 3.78541,         # gallons -> liters
             group = new_variable,
             color = user_cat,
             linetype = pre_post)) +
  stat_ecdf(size = 1) +
  scale_linetype_manual("Water use period",
                        values = c("dotted", "solid"),
                        breaks  = c("pre", "post")) +
  scale_color_manual("Water user group",
                     labels = c("Low user", "High user"),
                     values = c("blue", "#E51349")) +
  scale_x_continuous(
    name   = "Water volume (liters/week)",
    breaks = seq(0, 100000, 10000),
    labels = label_number(big.mark = ",", accuracy = 1),
    limits = c(0, 100000),
    expand = expansion(add = c(800, 0)),
    guide  = guide_axis(angle = 45)       # tilts tick labels
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "top")  -> group_wise_PrePost_waterUseComparison_liter
plot(group_wise_PrePost_waterUseComparison_liter)

# weekly volume 
weekly_volumetric_df_with_pre_wc_budget_comparison %>%
  filter(name == 'weekly_volume') %>%
  mutate(new_variable = paste0(pre_post, '_', user_cat)) %>%
  ggplot(aes(value,
             group = new_variable, 
             color = user_cat,linetype= pre_post 
  )) +
  stat_ecdf(size=1) +
  
  scale_linetype_manual("Water use period",
                        values=c("dotted", "solid"), 
                        breaks=c('pre', 'post')) +
  scale_color_manual("Water user group",
                     labels= c('Low user', 'High user'),
                     values = c("blue","#E51349"))+
  theme_classic(base_size = 12) +
  theme(legend.position ="top") +
  xlab("Water volume (gallons/week)") -> group_wise_PrePost_waterUseComparison
plot(group_wise_PrePost_waterUseComparison)

##################################################################
# Figure 5: Changes in behavioral variables (days between irrigation, volume, and number of daily events)

# Sites that changed volume

volume_change_sites <- map_df(unique(sites_with_pre_post_df$SiteID),ks_test_df_function, Variable = 'dvol') %>%
  mutate(variable = 'dvol')

volume_change_sites %>% 
  filter(ksTest_type_short== "less", DStat_Bigger == 'True' & p_significant =="True") -> reduced_volume

volume_change_sites %>% 
  filter(ksTest_type_short== "greater", DStat_Bigger == 'True' & p_significant =="True") -> increased_volume

volume_change_sites %>% 
  filter(ksTest_type_short== "two.sided", DStat_Bigger == 'True' & p_significant =="True") -> changed_volume #so all significant changes (regardless increased or decreased will be saved here)


# Sites that changed duration

duration_change_sites <- map_df(unique(sites_with_pre_post_df$SiteID),ks_test_df_function, Variable = 'dmin') %>%
  mutate(variable = 'dmin')

duration_change_sites %>% 
  filter(ksTest_type_short== "less", DStat_Bigger == 'True' & p_significant =="True") -> reduced_duration

duration_change_sites %>% 
  filter(ksTest_type_short== "greater", DStat_Bigger == 'True' & p_significant =="True") -> increased_duration

duration_change_sites %>% 
  filter(ksTest_type_short== "two.sided", DStat_Bigger == 'True' & p_significant =="True") -> changed_duration #so all significant changes (regardless increased or decreased will be saved here)



# Sites that changed event

event_change_sites <- map_df(unique(sites_with_pre_post_df$SiteID),ks_test_df_function, Variable = 'n_eve') %>%
  mutate(variable = 'n_eve')

event_change_sites %>% 
  filter(ksTest_type_short== "less", DStat_Bigger == 'True' & p_significant =="True") -> reduced_event

event_change_sites %>% 
  filter(ksTest_type_short== "greater", DStat_Bigger == 'True' & p_significant =="True") -> increased_event


# Sites that changed days between irrigation

lastIRR_change_sites <- map_df(unique(sites_with_pre_post_df$SiteID),ks_test_df_function, Variable = 'd_lastirr') %>%
  mutate(variable = 'd_lastirr')

lastIRR_change_sites %>% 
  filter(ksTest_type_short== "less", DStat_Bigger == 'True' & p_significant =="True") -> reduced_lastIRR

lastIRR_change_sites %>% 
  filter(ksTest_type_short== "greater", DStat_Bigger == 'True' & p_significant =="True") -> increased_lastIRR



# use a Likert plot to show the changes
rbind(volume_change_sites,duration_change_sites, event_change_sites,lastIRR_change_sites) %>%
  filter(ksTest_type_short == 'less' | ksTest_type_short == 'greater', 
         DStat_Bigger == 'True' & p_significant =="True") %>%
  select(SiteID,variable, ksTest_type_short) %>% 
  mutate(plotLocation = case_when(variable == 'dvol' & ksTest_type_short == 'less' ~ -1,
                                  variable == 'dvol' & ksTest_type_short == 'greater' ~ 1,
                                  variable == 'dmin' & ksTest_type_short == 'less' ~ -1,
                                  variable == 'dmin' & ksTest_type_short == 'greater' ~ 1,
                                  variable == 'n_eve' & ksTest_type_short == 'less' ~ -1,
                                  variable == 'n_eve' & ksTest_type_short == 'greater' ~ 1,
                                  variable == 'd_lastirr' & ksTest_type_short == 'less' ~ -1,
                                  variable == 'd_lastirr' & ksTest_type_short == 'greater' ~ 1,))-> likert_df 



# plot likert plot
likert_df %>%
  ggplot(aes(x=plotLocation, y= as.factor(SiteID), fill = variable)) +
  geom_col(position = "stack")+ xlab("Change in behavior") + ylab("Site ID")+ 
  scale_fill_manual("Behavior\nvariables",values= c("#34b1eb","#ead5e3","#42f5e3","#3734eb"),
                    labels = c("Days between irrigation","Duration","Volume","Number of irrigation events")) + 
  geom_vline(xintercept = 0, 
             color = "black", 
             size = 3) +
  annotate("text", x=-2, y= -.9, label= "Behavior variable \nvalue decreased",
           col="blue", size=5)+
  annotate("text", x=2, y= - 0.9, label= "Behavior variable \nvalue increased",
           col="red", size=5)+
  xlim(-3, 3) + expand_limits(x = 0, y = -2) +
  #scale_y_discrete(limits = as.factor(unique(likert_df$SiteID))) +
  theme_classic()+
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1),
        axis.title.x = element_text(color="black", size=14, face="bold", hjust=0.6),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #geom_hline(yintercept = 12, linetype = "dashed", color = 'red', size = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 3, yend = 0), colour = "red", linetype = "dashed",
               arrow = arrow(length = unit(0.5,"cm")))+
  geom_segment(aes(x = 0, y = 0, xend = -3, yend = 0), colour = "blue", linetype = "dashed",
               arrow = arrow(length = unit(0.5,"cm")))+
  theme(legend.position ="top",
        axis.title.x = element_text(hjust= 0.5)) -> likertPlot

plot(likertPlot)
