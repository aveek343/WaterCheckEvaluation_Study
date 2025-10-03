# This script creates the KS-test functions.
# Anlyzing pre vs post weekly water use of all the HH
# must run 2_DataProcessing.R before this one
# import df_test_weekly from 
# Author: Mahmud Aveek
# Date: 06-09-2024
# Runtime: 30 seconds
########################################################################
# calculate the budget/water requirement
# Analysis at the weekly level 
# Add the budget
kc = 0.8 # generalization - crop coefficient # based on Endtar-Wada et al. (2008) which says 0.7-0.9 for cool season turf
DU = 1  
# Re = 0.8 # can be an arbitrary value of 0.80 as effective rainfall portion; this coefficient is based on Lewis et al. (2016), USEPA (2011)
# or,
# effective rainfall (Re) can vary with the amount of Rainfall 
# if Rainfall <= 0.25 inch per week, then Re = 15%
# if Rainfall > 0.25 but <= 0.5 inch per week, then Re = 100%
# if Rainfall > 0.50, then Re = 67% (these values are partially based on Fontanier et al., 2021) 


# calculate the budget using the parameter
daily_wd %>% # daily weather data
  mutate(week = isoweek(date)) %>%
  filter(year(date) == '2022') %>%
  group_by(week) %>%
  summarise(weekly_pcp_in = sum(precip), weekly_tmp_F = mean(airt_avg), weekly_eto_in = sum(eto)) %>%
  mutate(Re = ifelse(weekly_pcp_in <= 0.25, .15,
                     ifelse(weekly_pcp_in > 0.25 & weekly_pcp_in <=0.5, 1, 0.67))) %>%
  mutate(budget = (kc*weekly_eto_in - Re*weekly_pcp_in)/DU) %>%
  mutate(budget = if_else(budget < 0, 0 , budget)) -> weekly_wd_new_par # remove negative values

# Irrigable areas
# colnames(WaterCheckData)
WaterCheckData %>%
  select(SiteID, TurfAreaft2, OtherIrrAreaft2) %>%
  mutate(IrrigableArea_ft2 = TurfAreaft2 + OtherIrrAreaft2) %>%
  select(SiteID, IrrigableArea_ft2) -> IrrAreas_All

# Comparing the budget with Weekly water application value
data_test %>%
  #filter(SiteID == 62) %>%
  select(SiteID, date, dvol, n_eve, dmin, WaterCheckDate, pre_post) %>%
  mutate(week = isoweek(date), wc_week = isoweek(WaterCheckDate)) %>%
  group_by(week) %>%
  filter(row_number() != 1 & row_number() != n()) %>% # remove the first and last day as these are incomplete
  filter(week != wc_week) %>%
  # or probably we should consider the week when water check had happened as pre.. ask DR
  ungroup() %>%
  select(SiteID, week, dvol, n_eve, dmin, pre_post) %>%
  group_by(SiteID, week, pre_post) %>%
  summarise(weekly_volume = sum(dvol), total_irrigation_minutes = sum(dmin), frequency = sum(n_eve)) %>%
  left_join(weekly_wd_new_par) %>% # add weather data and budget
  left_join(IrrAreas_All) %>% # add irrigable area
  # mutate(IrrigationVolume_in = (weekly_volume *  0.133681 / IrrigableArea_ft2) * 12) %>%
  # mutate(IrrigationVolume_in = (weekly_volume * 1000 * 0.133681 / IrrigableArea_ft2) / 12) %>% # volume from 10^3 gal to ft3. ft3/ft2. ft to inches
  mutate(budgetGal = 0.624 * budget * IrrigableArea_ft2) %>%
  mutate(VolumeOverBudget = weekly_volume - budgetGal) %>% # volume of water applied above the budget
  mutate(VolumeOverBudget = if_else(VolumeOverBudget < 0, 0, VolumeOverBudget), # remove negative values
         VolumeVsBudget = weekly_volume - budgetGal,
         over_budget = ifelse(VolumeVsBudget > 0,VolumeVsBudget,0),
         under_budget = ifelse(VolumeVsBudget < 0, VolumeVsBudget, 0)) -> df_test_weekly_volumetric

# Separate households from the two user_cat groups
df_test_weekly_volumetric %>%
  filter(pre_post == "pre") %>%
  ungroup() %>%
  select(SiteID, weekly_volume, budgetGal) %>%
  group_by(SiteID) %>%
  summarize(total_volume_pre = sum(weekly_volume),
            total_budget_pre = sum(budgetGal)) %>% 
  mutate(user_cat = ifelse(total_volume_pre > total_budget_pre, "Used more than budget",
                           "Used less than budget")) -> total_budget_vs_use_cat_df

df_test_weekly_volumetric  %>%
  ungroup() %>%
  select(SiteID, week, pre_post, weekly_volume, budgetGal)%>%
  mutate(residual = weekly_volume - budgetGal ) %>%
  left_join(Sites %>% select(SiteID, City)) %>% 
  relocate(City, .before = weekly_volume) %>% 
  pivot_longer(cols = weekly_volume:residual) %>%
  mutate(x_label = case_when(name == "weekly_volume" ~ "water application (gal/week)",
                             name == "budgetGal" ~ "weekly budget (gal/week)",
                             name == "residual" ~ "residual (gal/week) = water application (gal/week) - budget (gal/week)"),
         title = case_when(name == "weekly_volume" ~ "K-S Test for all weekly pre and post observation", 
                           name == "budgetGal" ~ "K-S Test for all weekly pre and post budget",
                           name == "residual" ~ "K-S Test for all pre weekly residual and post weekly residual")) %>% 
  left_join(total_budget_vs_use_cat_df %>% 
              select (SiteID, user_cat)) -> weekly_volumetric_df_with_pre_wc_budget_comparison

#############################################
# Creating a dataframe for the ks-test function which has volumetric comparison between budget and actual water application for ks testing

df_test_weekly_volumetric %>%
  select(SiteID,pre_post,weekly_volume, budgetGal ) %>%
  mutate(residual = weekly_volume - budgetGal ) %>%
  left_join(Sites) %>% ungroup() %>%
  select(pre_post,weekly_volume, budgetGal,residual, City) %>% 
  relocate(City, .before = weekly_volume)%>%
  pivot_longer(cols = weekly_volume:residual) %>%
  mutate(x_label = case_when(name == "weekly_volume" ~ "water application (gal/week)",
                             name == "budgetGal" ~ "weekly budget (gal/week)",
                             name == "residual" ~ "residual (gal/week) = water application (gal/week) - budget (gal/week)"),
         title = case_when(name == "weekly_volume" ~ "K-S Test for all weekly pre and post observation", 
                           name == "budgetGal" ~ "K-S Test for all weekly pre and post budget",
                           name == "residual" ~ "K-S Test for all pre weekly residual and post weekly residual")) -> all_pre_post_weekly

#   Following function performs ks test of only volume data: 
##      1. all water use data without grouping users. under this set of tests, 
##         same variable from pre and post is compared, i.e., pre water use vs post water use, and pre residual (water use - budget) vs post residual (water use - budget)
##      2. Users are divided into 2 groups: users who cumulatively used more than the total budget in 
##         entire duration of the pre-intervention period (high users), and vice-versa (low users). Then
##         the usual pre and post water use and residual comparisons are done using KS test. 


KS_test_weekly_function <- function(variable, city) { # variable = weekly_volume, budgetGal, or residual
  # city = Hyde Park, Logan
  #comparison_period = comparisonPeriod
  variable_name = variable
  city_name = city
  
  if(city_name == "All"){
    all_pre_post_weekly %>%
      filter(name == variable_name) -> weekly_df
  }else{
    all_pre_post_weekly %>%
      filter(name == variable_name) %>%
      filter(City == city_name)-> weekly_df
  }
  
  # creating pre-post vectors
  pre_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre", "value"]))
  post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  
  
  # Critical value for D-statistic
  Dcrit <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly))/((length(pre_weekly) * length(post_weekly))))
  
  # cdf creation
  pre_ecdf <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  #ks test
  ksTest_weekly_l <- ks.test(pre_weekly,post_weekly, 
                             alternative = c("less"))
  ksTest_weekly_g <- ks.test(pre_weekly,post_weekly, 
                             alternative = c("greater"))
  
  ksTest_Df <- tibble(ksTest_type = c(ksTest_weekly_l$alternative,ksTest_weekly_g$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ksTest_weekly_l$p.value,ksTest_weekly_g$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ksTest_weekly_l$statistic,ksTest_weekly_g$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  true_false <- paste0(ksTest_Df[1,6] , ksTest_Df[1,7])
  # df of ks-test for labeling purposes
  
  if (true_false == "FalseTrue" | true_false == "FalseFalse" | true_false == "TrueFalse") {
    ksTest_Df %>%
      filter(ksTest_type_short == 'less') -> ksTest_Df_label
    
  } else {
    ksTest_Df %>%
      filter(DStat_Bigger == "True" & p_significant == "True") -> ksTest_Df_label
  }
  
  # # df of ks-test for labeling purposes
  # ksTest_Df_label <- ksTest_Df %>%
  #   # filter(ksTest_type != 'two-sided') %>%
  #   filter(DStat_Bigger == "True" & p_significant == "True" )
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_weekly), length(post_weekly)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_weekly), length(pre_weekly)))[1]
  
  if(length(pre_weekly) == first_length){
    first_data <- pre_weekly
  } else {
    first_data <- post_weekly
  }
  
  if(length(post_weekly) == second_length){
    second_data <- post_weekly
  } else {
    second_data <- pre_weekly
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]  
  
  
  # # creating result df
  # weekly_ks_test_result <- tibble(pvalue = ksTest_weekly_l$p.value,
  #                                 D = as.numeric(ksTest_weekly_l$statistic),
  #                                 pre_n = length(pre_weekly),
  #                                 post_n = length(post_weekly),
  #                                 alternative_hypothesis = ifelse(ksTest_weekly_l$alternative == "the CDF of x lies below that of y","less",
  #                                                                 ifelse(ksTest_weekly_l$alternative == "the CDF of x lies above that of y","greater","two.sided")),
  #                                 variable = variable_name)
  
  # data for shaded region
  
  preQuntile <- unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5, 0.625,0.75,0.875)))
  postQuntile <- unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  
  
  # plotting the ks test
  
  p_all_week_cdf<-ggplot(weekly_df, aes(x = value,
                                        group = pre_post, 
                                        color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual(values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(weekly_df$x_label)) +
    ylab("ECDF") +
    #geom_line(size=1) +
    ggtitle(paste0("City: ", city_name,"\n",
                   unique(weekly_df$title), 
                   "\nD-stat:",
                   round(ksTest_Df_label$DStat,2),", D-Crit:", round(ksTest_Df_label$Dcrit,2),
                   "\np-value:", formatC(ksTest_Df_label$p_value),
                   "\nAlt. hypothesis:",ksTest_Df_label$ksTest_type_short)) +
    theme_classic()+
    theme(legend.title=element_blank(),
          legend.position = "top")+
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), 
                     xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue", size=2) +
    geom_segment(aes(x = preQuntile[1], y = .25, 
                     xend = postQuntile[1], yend = .25),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[2], y = 0.375, 
                     xend = postQuntile[2], yend = 0.375),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[3], y = 0.5, 
                     xend = postQuntile[3], yend = 0.5),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[4], y = 0.625, 
                     xend = postQuntile[4], yend = 0.625),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[5], y = 0.75, 
                     xend = postQuntile[5], yend = 0.75),
                 linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[6], y = 0.875, 
                     xend = postQuntile[6], yend = 0.875),
                 linetype = "dotted", color = "brown")+
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=4) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=4) +
    geom_text(aes(x=largest_diff_location, 
                  y = mean(pre_ecdf(largest_diff_location),post_ecdf(largest_diff_location)),
                  label = paste0("D-stat: ",
                                 round(ksTest_Df_label$DStat,2))),
              hjust = -.5,
              # yjust = -1,
              size = 5,
              color = 'blue')
  
  print(paste0("pre-largest diff percentile=", pre_ecdf(largest_diff_location), "and the value=",largest_diff_location))
  print(paste0("post-largest diff percentile=", post_ecdf(largest_diff_location)))
  
  table_ecdf <- tibble(percentile_values = c(0.25,0.375,0.5, 0.625,0.75,0.875, 0.90),
                       pre_ecdf = unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))),
                       post_ecdf = unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))))
  print(table_ecdf)
  
  #plot(p_all_week_cdf)
  return(p_all_week_cdf)
}

# variable = weekly_volume, or budgetGal, or residual
# use one of the 3 options for cities: All, or Hyde Park, or Logan

# For pooled data
# KS_test_weekly_function(variable = "weekly_volume", city = "All")
# KS_test_weekly_function(variable = "budgetGal", city = "All")
# KS_test_weekly_function(variable = "residual", city = "All")
# 
# # For Hyde Park
# KS_test_weekly_function(variable = "weekly_volume", city = "Hyde Park")
# KS_test_weekly_function(variable = "budgetGal", city = "Hyde Park")
# KS_test_weekly_function(variable = "residual", city = "Hyde Park")
# 
# # For Logan
# KS_test_weekly_function(variable = "weekly_volume", city = "Logan")
# KS_test_weekly_function(variable = "budgetGal", city = "Logan")
# KS_test_weekly_function(variable = "residual", city = "Logan")

######################################
# daily pooled data for ks testing
data_test %>%
  left_join(Sites %>% select(SiteID,City)) %>% ungroup() %>%
  select(City, pre_post, dvol, dmin, n_eve,d_lastirr) %>%
  pivot_longer(-c(City, pre_post)) -> data_test2


ks_test_plots <- function(City, Variable ){
  
  
  
  
  variable_name = as.character(Variable)
  city_name = as.character(City)
  
  # variable_name = 'dmin'
  # city_name = 'Logan'
  
  
  
  if(city_name == "All"){
    data_test2 %>%
      filter(name == variable_name) -> daily_df
  } else{
    data_test2 %>%
      filter(name == variable_name) %>%
      filter(City == city_name)-> daily_df
  }
  
  
  daily_df  %>%
    mutate(label = case_when(variable_name == "dvol" ~ "Daily irrigation Volume (gal)",
                             variable_name == 'dmin' ~ "Daily irrigation duration (min)",
                             variable_name == "d_lastirr" ~ "Days between irrigation (days)",
                             variable_name == "n_eve" ~ "Daily irrigation events (number)"))-> ks_test_data
  
  
  
  
  
  
  # create pre and post-wc variable vectors
  pre_data <- ks_test_data$value[ks_test_data$pre_post == "pre"]
  post_data <- ks_test_data$value[ks_test_data$pre_post == "post"]
  
  # ks test
  
  # for less
  ks_test_result_less <- ks.test(pre_data,post_data,alternative = "less")
  # for greater
  ks_test_result_greater <- ks.test(pre_data,post_data,alternative = "greater")
  # for two sided
  ks_test_result_ts <- ks.test(pre_data,post_data,alternative = "two.sided")
  
  # get the critical value using following formula which was extracted from this website: https://sparky.rice.edu/astr360/kstest.pdf
  # Dcrit = 1.36* sqrt((lenght of pre+ length of post)/(length of pre* length of post)); 
  # where 1.36 is a coefficient for alpha = 0.05 (95% confidence)
  
  Dcrit <- 1.36 * sqrt((length(pre_data) + length(post_data))/((length(pre_data) * length(post_data))))
  
  
  # Create a dataframe with all these values to check what happened (increased, decreased)
  ksTest_Df <- tibble(ksTest_type = c(ks_test_result_less$alternative,ks_test_result_greater$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ks_test_result_less$p.value,ks_test_result_greater$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ks_test_result_less$statistic,ks_test_result_greater$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  
  ksTest_Df %>%
    select(ksTest_type_short, DStat_Bigger,p_significant) %>%
    pivot_longer(cols = DStat_Bigger:p_significant) ->randomCheck
  
  
  true_false <- paste0(ksTest_Df[1,6] , ksTest_Df[1,7])
  
  
  # df of ks-test for labeling purposes
  if (true_false == "FalseTrue" | true_false == "FalseFalse" | true_false == "TrueFalse") {
    ksTest_Df %>%
      filter(ksTest_type_short == 'less') -> ksTest_Df_label
    
  } else {
    ksTest_Df %>%
      filter(DStat_Bigger == "True" & p_significant == "True") -> ksTest_Df_label
  }
  
  
  
  
  # ksTest_Df_label <- ksTest_Df %>%
  #   # filter(ksTest_type != 'two-sided') %>%
  #   filter(DStat_Bigger == "True" & p_significant == "True" )
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_data), length(post_data)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_data), length(pre_data)))[1]
  
  if(length(pre_data) == first_length){
    first_data <- pre_data
  } else {
    first_data <- post_data
  }
  
  if(length(post_data) == second_length){
    second_data <- post_data
  } else {
    second_data <- pre_data
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]
  
  # check
  pre_ecdf <- ecdf(pre_data)
  
  post_ecdf <- ecdf(post_data)
  
  abs(pre_ecdf(largest_diff_location) - post_ecdf(largest_diff_location))
  
  
  # data for shaded region
  
  preQuntile <- unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5, 0.625,0.75,0.875)))
  postQuntile <- unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  
  
  # plot
  ks_test_data %>% 
    select(pre_post, value) %>%
    ggplot(aes(x = value,
               group = pre_post, 
               color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual("Period",values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(ks_test_data$label)) +
    ylab("ECDF") +
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), 
                     xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue", size=2) +
    geom_segment(aes(x = preQuntile[1], y = .25, 
                     xend = postQuntile[1], yend = .25),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[2], y = 0.375, 
                     xend = postQuntile[2], yend = 0.375),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[3], y = 0.5, 
                     xend = postQuntile[3], yend = 0.5),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[4], y = 0.625, 
                     xend = postQuntile[4], yend = 0.625),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[5], y = 0.75, 
                     xend = postQuntile[5], yend = 0.75),
                 linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[6], y = 0.875, 
                     xend = postQuntile[6], yend = 0.875),
                 linetype = "dotted", color = "brown") +
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue") +
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=2) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=2)+
    ggtitle(paste0("City=",city_name,", ",unique(ks_test_data$label), 
                   "\nD-crit=", round(Dcrit,2), ", D-stat=", round(ksTest_Df_label$DStat,2),
                   "\np-value=", formatC(ksTest_Df_label$p_value))) -> plot_data
  
  table_ecdf <- tibble(percentile_values = c(0.25,0.375,0.5, 0.625,0.75,0.875, 0.90),
                       pre_ecdf = unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))),
                       post_ecdf = unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))))
  print(table_ecdf)
  
  return(plot_data)
  
}
# use one of the 4 variables: dmin, dvol, d_lastirr, n_eve
# use one of the 3 options for cities: All, or Hyde Park, or Logan

# For pooled data
# ks_test_plots("All","dmin")
# ks_test_plots("All","dvol")
# ks_test_plots("All","d_lastirr")
# ks_test_plots("All","n_eve")
# 
# # For pooled data
# ks_test_plots("Hyde Park","dmin")
# ks_test_plots("Hyde Park","dvol")
# ks_test_plots("Hyde Park","d_lastirr")
# ks_test_plots("Hyde Park","n_eve")
# 
# # For pooled data
# ks_test_plots("Logan","dmin")
# ks_test_plots("Logan","dvol")
# ks_test_plots("Logan","d_lastirr")
# ks_test_plots("Logan","n_eve")

##################################################
# B. Following function can check the ks-test for volume, budget, and resudual for volume, 
# and also show how pooled or city-wise water volume is compared to ranked budget

KS_test_weekly_function_advanced <- function(variable, city) { # variable = weekly_volume, budgetGal, residual, or "weekly_volume_budgetGal"
  # city = "All"
  # comparison_period = comparisonPeriod
  variable_name = variable
  city_name = city
  
  if(city_name == "All" & variable_name != "weekly_volume_budgetGal"){
    all_pre_post_weekly %>%
      filter(name == variable_name) -> weekly_df
  } else if (city_name == "All" & variable_name == "weekly_volume_budgetGal") {
    all_pre_post_weekly %>%
      filter(name == "weekly_volume" | name == "budgetGal") -> weekly_df
  } else if (city_name != "All" & variable_name != "weekly_volume_budgetGal") {
    all_pre_post_weekly %>%
      filter(name == variable_name) %>%
      filter(City == city_name)-> weekly_df
  } else {
    all_pre_post_weekly %>%
      filter(name == "weekly_volume" | name == "budgetGal") %>%
      filter(City == city_name)
  }-> weekly_df
  
  # creating pre-post vectors
  
  if(variable_name != "weekly_volume_budgetGal"){
    pre_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre" , "value"]))
    post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  } else {
    pre_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre" & weekly_df$name == 'weekly_volume', "value"]))
    post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post" & weekly_df$name == 'weekly_volume', "value"]))
  }
  
  
  # Critical value for D-statistic
  Dcrit <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly))/((length(pre_weekly) * length(post_weekly))))
  
  # cdf creation
  pre_ecdf <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  #ks test
  ksTest_weekly_l <- ks.test(pre_weekly,post_weekly, 
                             alternative = c("less"))
  ksTest_weekly_g <- ks.test(pre_weekly,post_weekly, 
                             alternative = c("greater"))
  
  ksTest_Df <- tibble(ksTest_type = c(ksTest_weekly_l$alternative,ksTest_weekly_g$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ksTest_weekly_l$p.value,ksTest_weekly_g$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ksTest_weekly_l$statistic,ksTest_weekly_g$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  
  # df of ks-test for labeling purposes
  ksTest_Df_label <- ksTest_Df %>%
    # filter(ksTest_type != 'two-sided') %>%
    filter(DStat_Bigger == "True" & p_significant == "True" )
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_weekly), length(post_weekly)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_weekly), length(pre_weekly)))[1]
  
  if(length(pre_weekly) == first_length){
    first_data <- pre_weekly
  } else {
    first_data <- post_weekly
  }
  
  if(length(post_weekly) == second_length){
    second_data <- post_weekly
  } else {
    second_data <- pre_weekly
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]  
  
  
  # # creating result df
  # weekly_ks_test_result <- tibble(pvalue = ksTest_weekly_l$p.value,
  #                                 D = as.numeric(ksTest_weekly_l$statistic),
  #                                 pre_n = length(pre_weekly),
  #                                 post_n = length(post_weekly),
  #                                 alternative_hypothesis = ifelse(ksTest_weekly_l$alternative == "the CDF of x lies below that of y","less",
  #                                                                 ifelse(ksTest_weekly_l$alternative == "the CDF of x lies above that of y","greater","two.sided")),
  #                                 variable = variable_name)
  
  # data for shaded region
  
  preQuntile <- unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5, 0.625,0.75,0.875)))
  postQuntile <- unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  
  # now calculation for the linear interpolation to create pre-water check data so that there are 372 weeks of pre-water check data
  
  
  
  # if(variable_name == "weekly_volume_budgetGal") {
  #   pre_budget <- weekly_df %>% filter(pre_post == 'pre' & name == 'budgetGal') %>% select(value)
  #   post_budget <- weekly_df %>% filter(pre_post == 'post' & name == 'budgetGal') %>% select(value)
  #   lm_pre_budgetGal <- approx(pre_budget, n = length(post_budget))
  #   lm_pre_weekly <- approx(pre_weekly, n = length(post_weekly))
  # } else {
  #   lm_pre_weekly <- approx(pre_weekly, n = length(post_weekly))
  #   }
  # 
  # 
  # water_use_df <- tibble(lm_pre_use = lm_pre_use$y,
  #                        post_use = post_use,
  #                        difference = lm_pre_use - post_use)


  # plotting the ks test
  if(variable_name != "weekly_volume_budgetGal") { 
    ggplot(weekly_df, aes(x = value,
                          group = pre_post, 
                          color = pre_post))+
      stat_ecdf(size=1) + scale_color_manual(values = c("#3275a8","pink"))+
      theme_bw(base_size = 12) +
      theme(legend.position ="top") +
      xlab(unique(weekly_df$x_label)) +
      ylab("ECDF") +
      #geom_line(size=1) +
      ggtitle(paste0("City: ", city_name,"\n",
                     unique(weekly_df$title), 
                     "\nD-stat:",
                     round(ksTest_Df_label$DStat,2),", D-Crit:", round(ksTest_Df_label$Dcrit,2),
                     "\np-value:", formatC(ksTest_Df_label$p_value),
                     "\nAlt. hypothesis:",ksTest_Df_label$ksTest_type_short)) +
      theme_classic()+
      theme(legend.title=element_blank(),
            legend.position = "top")+
      geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), 
                       xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                   linetype = "dashed", color = "blue", size=2) +
      geom_segment(aes(x = preQuntile[1], y = .25, 
                       xend = postQuntile[1], yend = .25),
                   linetype = "dotted", color = "brown")+
      geom_segment(aes(x = preQuntile[2], y = 0.375, 
                       xend = postQuntile[2], yend = 0.375),
                   linetype = "dotted", color = "brown")+
      geom_segment(aes(x = preQuntile[3], y = 0.5, 
                       xend = postQuntile[3], yend = 0.5),
                   linetype = "dotted", color = "brown")+
      geom_segment(aes(x = preQuntile[4], y = 0.625, 
                       xend = postQuntile[4], yend = 0.625),
                   linetype = "dotted", color = "brown")+
      geom_segment(aes(x = preQuntile[5], y = 0.75, 
                       xend = postQuntile[5], yend = 0.75),
                   linetype = "dotted", color = "brown") +
      geom_segment(aes(x = preQuntile[6], y = 0.875, 
                       xend = postQuntile[6], yend = 0.875),
                   linetype = "dotted", color = "brown")+
      geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=4) +
      geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=4) +
      geom_text(aes(x=largest_diff_location, 
                    y = mean(pre_ecdf(largest_diff_location),post_ecdf(largest_diff_location)),
                    label = paste0("D-stat: ",
                                   round(ksTest_Df_label$DStat,2))),
                hjust = -.5,
                # yjust = -1,
                size = 5,
                color = 'blue') -> p_all_week_cdf
    
    print(paste0("pre-largest diff percentile=", pre_ecdf(largest_diff_location), "and the value=",largest_diff_location))
    print(paste0("post-largest diff percentile=", post_ecdf(largest_diff_location)))
  } else {
    weekly_df %>% 
      mutate(variable_type = paste0(pre_post, '_', name)) %>%
      select(variable_type, value) %>% 
      mutate(lineType = ifelse(variable_type == gsub("budget","",variable_type, fixed = TRUE),"water used","budget")) %>%
      ggplot(aes(x = value))+
      stat_ecdf(size =1, aes(group = variable_type, color = variable_type,linetype=lineType)) +
      
      scale_linetype_manual("Variable",values=c("dashed", "solid"))+
      
      scale_color_manual("Variable", values = c("#96C3F5","#3275a8","#E3A1B2","#E51349"),
                         labels = c("Post-WC Budget", "Post-WC Water Used",
                                    "Pre-WC Budget", "pre-WC Water Used")) +
      
      theme_bw(base_size = 12) +
      theme(legend.position ="top") +
      xlab("Water Volume (gallons/week)") +
      ylab("ECDF")  +
      #geom_line(size=1) +
      ggtitle(paste0("City: ", city_name,"\n",
                     "Comparison between actual pre- and post-Water Check\nWater use and Water budget (requirement)",
                     "\nD-stat:",
                     round(ksTest_Df_label$DStat,2),", D-Crit:", round(ksTest_Df_label$Dcrit,2),
                     "\np-value:", formatC(ksTest_Df_label$p_value),
                     "\nAlt. hypothesis:",ksTest_Df_label$ksTest_type_short))  +
      theme_classic()+
      theme(legend.title=element_blank(),
            legend.position = "top")+
      geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location),
                       xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                   linetype = "dashed", color = "blue", size=2)  +
      # geom_segment(aes(x = preQuntile[1], y = .25,
      #                  xend = postQuntile[1], yend = .25),
      #              linetype = "dotted", color = "brown")+
      # geom_segment(aes(x = preQuntile[2], y = 0.375,
      #                  xend = postQuntile[2], yend = 0.375),
      #              linetype = "dotted", color = "brown")+
      # geom_segment(aes(x = preQuntile[3], y = 0.5,
      #                  xend = postQuntile[3], yend = 0.5),
      #              linetype = "dotted", color = "brown")+
      # geom_segment(aes(x = preQuntile[4], y = 0.625,
      #                  xend = postQuntile[4], yend = 0.625),
    #              linetype = "dotted", color = "brown")+
    # geom_segment(aes(x = preQuntile[5], y = 0.75,
    #                  xend = postQuntile[5], yend = 0.75),
    #              linetype = "dotted", color = "brown") +
    # geom_segment(aes(x = preQuntile[6], y = 0.875,
    #                  xend = postQuntile[6], yend = 0.875),
    #              linetype = "dotted", color = "brown")+
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=4) +
      geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=4) +
      geom_text(aes(x=largest_diff_location, 
                    y = mean(pre_ecdf(largest_diff_location),post_ecdf(largest_diff_location)),
                    label = paste0("D-stat: ",
                                   round(ksTest_Df_label$DStat,2))),
                hjust = -.5,
                # yjust = -1,
                size = 5,
                color = 'blue') +
      guides(linetype = "none") -> p_all_week_cdf
    
    print(paste0("pre-largest diff percentile=", pre_ecdf(largest_diff_location), "and the value=",largest_diff_location))
    print(paste0("post-largest diff percentile=", post_ecdf(largest_diff_location))) 
  }
  
  # plot(p_all_week_cdf)
  return(p_all_week_cdf)
}

# variable = either weekly_volume, budgetGal, and residual, weekly_volume_budgetGal
## use weekly_volume,budgetGal,and residual, for just volume comparison, budget in gallons, and residual respectively
## use weekly_volume_budgetGal to compare the pre- and post- volume and budget in the same figure

# city = All, Hyde Park, Logan

# test function
# KS_test_weekly_function_advanced(variable = "weekly_volume_budgetGal", city = "All")
# KS_test_weekly_function_advanced(variable = "weekly_volume", city = "All")
# KS_test_weekly_function_advanced(variable = "budgetGal", city = "All")
# KS_test_weekly_function_advanced(variable = "residual", city = "All")
# 
# KS_test_weekly_function_advanced(variable = "weekly_volume_budgetGal", city = "Hyde Park")
# KS_test_weekly_function_advanced(variable = "weekly_volume", city = "Hyde Park")
# KS_test_weekly_function_advanced(variable = "budgetGal", city = "Hyde Park")
# KS_test_weekly_function_advanced(variable = "residual", city = "Hyde Park")
# 
# KS_test_weekly_function_advanced(variable = "weekly_volume_budgetGal", city = "Logan")
# KS_test_weekly_function_advanced(variable = "weekly_volume", city = "Logan")
# KS_test_weekly_function_advanced(variable = "budgetGal", city = "Logan")
# KS_test_weekly_function_advanced(variable = "residual", city = "Logan")

# to convert from gallon to liter
KS_test_weekly_function_advanced_liter <- function(variable, city) {
  # variable = weekly_volume, budgetGal, residual, or "weekly_volume_budgetGal"
  conv <- 3.78541  # gallons -> liters
  
  variable_name <- variable
  city_name     <- city
  
  # ---------------- Filter data ----------------
  if (city_name == "All" & variable_name != "weekly_volume_budgetGal") {
    weekly_df <- dplyr::filter(all_pre_post_weekly, name == variable_name)
  } else if (city_name == "All" & variable_name == "weekly_volume_budgetGal") {
    weekly_df <- dplyr::filter(all_pre_post_weekly, name %in% c("weekly_volume","budgetGal"))
  } else if (city_name != "All" & variable_name != "weekly_volume_budgetGal") {
    weekly_df <- all_pre_post_weekly %>% dplyr::filter(name == variable_name, City == city_name)
  } else {
    weekly_df <- all_pre_post_weekly %>% dplyr::filter(name %in% c("weekly_volume","budgetGal"), City == city_name)
  }
  
  # liters for plotting (keep gallons in 'value' for stats)
  weekly_df <- weekly_df %>% dplyr::mutate(value_l = value * conv)
  
  # ---------------- Pre/Post vectors (gallons) ----------------
  if (variable_name != "weekly_volume_budgetGal") {
    pre_weekly  <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre",  "value"]))
    post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  } else {
    pre_weekly  <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre"  & weekly_df$name == "weekly_volume",  "value"]))
    post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post" & weekly_df$name == "weekly_volume", "value"]))
  }
  
  # ---------------- KS stats (gallons) ----------------
  Dcrit     <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly)) / (length(pre_weekly) * length(post_weekly)))
  pre_ecdf  <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  ksTest_weekly_l <- ks.test(pre_weekly, post_weekly, alternative = "less")
  ksTest_weekly_g <- ks.test(pre_weekly, post_weekly, alternative = "greater")
  
  ksTest_Df <- tibble::tibble(
    ksTest_type       = c(ksTest_weekly_l$alternative, ksTest_weekly_g$alternative),
    ksTest_type_short = c("less","greater"),
    p_value           = c(ksTest_weekly_l$p.value, ksTest_weekly_g$p.value),
    Dcrit             = Dcrit,
    DStat             = c(ksTest_weekly_l$statistic, ksTest_weekly_g$statistic),
    DStat_Bigger      = ifelse(DStat > Dcrit, "True", "False"),
    p_significant     = ifelse(p_value < 0.05, "True", "False")
  )
  ksTest_Df_label <- dplyr::filter(ksTest_Df, DStat_Bigger == "True", p_significant == "True")
  
  # ---------------- Largest vertical gap ----------------
  first_length  <- ifelse(ksTest_Df_label$ksTest_type_short[1] == "less", length(pre_weekly),  length(post_weekly))
  second_length <- ifelse(ksTest_Df_label$ksTest_type_short[1] == "less", length(post_weekly), length(pre_weekly))
  
  first_data  <- if (length(pre_weekly)  == first_length)  pre_weekly  else post_weekly
  second_data <- if (length(post_weekly) == second_length) post_weekly else pre_weekly
  
  combined_data         <- c(first_data, second_data)
  combined_data_rank    <- order(combined_data)
  location_assign       <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  largest_diff_location_gal <- combined_data[combined_data_rank[biggest_value_location]]
  largest_diff_location_l   <- largest_diff_location_gal * conv
  largest_diff_location_l   <- largest_diff_location_gal * conv
  
  # quantile guide x-positions (liters)
  preQuntile_l  <- unname(quantile(pre_ecdf,  na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))  * conv
  postQuntile_l <- unname(quantile(post_ecdf, na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875))) * conv
  
  # ---------------- Axis range (liters) ----------------
  max_x_l    <- max(weekly_df$value_l, largest_diff_location_l, preQuntile_l, postQuntile_l, na.rm = TRUE)
  breaks_end <- ceiling(max_x_l / 25000) * 25000
  # we'll set limits via coord_cartesian() to preserve 'expand' padding
  x_right <- breaks_end + 2000
  
  # ---------------- Plotting ----------------
  if (variable_name != "weekly_volume_budgetGal") {
    p_all_week_cdf <-
      ggplot2::ggplot(weekly_df, ggplot2::aes(x = value_l, group = pre_post, color = pre_post)) +
      ggplot2::stat_ecdf(size = 1) +
      ggplot2::scale_color_manual(
        values = c("post" = "#3275a8", "pre" = "pink"),
        breaks = c("post","pre"),
        labels = c("post-WC", "pre-WC")
      ) +
      ggplot2::theme_classic() +
      # ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
      ggplot2::xlab(gsub("gallons/week","liters/week", unique(weekly_df$x_label), ignore.case = TRUE)) +
      ggplot2::ylab("ECDF") +
      ggplot2::ggtitle(paste0(
        "City: ", city_name, "\n", unique(weekly_df$title),
        "\nD-stat:", round(ksTest_Df_label$DStat, 2), ", D-Crit:", round(ksTest_Df_label$Dcrit, 2),
        "\np-value:", formatC(ksTest_Df_label$p_value),
        "\nAlt. hypothesis:", ksTest_Df_label$ksTest_type_short
      )) +
      
      ggplot2::geom_segment(
        ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal),
                     xend = largest_diff_location_l, yend = pre_ecdf(largest_diff_location_gal)),
        linetype = "dashed", color = "blue", size = 2
      ) +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[1], y = .25,   xend = postQuntile_l[1], yend = .25),   linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[2], y = 0.375, xend = postQuntile_l[2], yend = 0.375), linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[3], y = 0.5,   xend = postQuntile_l[3], yend = 0.5),   linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[4], y = 0.625, xend = postQuntile_l[4], yend = 0.625), linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[5], y = 0.75,  xend = postQuntile_l[5], yend = 0.75),  linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[6], y = 0.875, xend = postQuntile_l[6], yend = 0.875), linetype = "dotted", color = "brown") +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal)), color = "blue", size = 4) +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = pre_ecdf(largest_diff_location_gal)),  color = "blue", size = 4) +
      ggplot2::geom_text(
        ggplot2::aes(x = largest_diff_location_l,
                     y = mean(pre_ecdf(largest_diff_location_gal), post_ecdf(largest_diff_location_gal)),
                     label = paste0("D-stat: ", round(ksTest_Df_label$DStat, 2))),
        hjust = -.5, size = 5, color = "blue"
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, breaks_end, by = 25000),
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0.01, 0), add = c(1000, 2000)) # small left pad, keep right headroom
      ) +
      ggplot2::coord_cartesian(xlim = c(0, x_right), clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5))
    
  } else {
    p_all_week_cdf <-
      weekly_df %>%
      dplyr::mutate(variable_type = paste0(pre_post, "_", name)) %>%
      dplyr::select(variable_type, value_l) %>%
      dplyr::mutate(lineType = ifelse(grepl("budget", variable_type, fixed = TRUE), "budget", "water used")) %>%
      ggplot2::ggplot(ggplot2::aes(x = value_l)) +
      ggplot2::stat_ecdf(size = 1, ggplot2::aes(group = variable_type, color = variable_type, linetype = lineType)) +
      ggplot2::scale_linetype_manual("Variable", values = c("dashed", "solid")) +
      ggplot2::scale_color_manual(
        "Variable",
        values = c("#96C3F5","#3275a8","#E3A1B2","#E51349"),
        labels = c("post-WC Budget", "post-WC Water Used",
                   "pre-WC Budget",  "pre-WC Water Used")
      ) +
      
      #ggplot2::theme_bw() +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
      ggplot2::xlab("Water Volume (liters/week)") +
      ggplot2::ylab("ECDF") +
      ggplot2::ggtitle(paste0(
        "City: ", city_name, "\n",
        "Comparison between actual pre- and post-Water Check\nWater use and Water budget (requirement)",
        "\nD-stat:", round(ksTest_Df_label$DStat, 2), ", D-Crit:", round(ksTest_Df_label$Dcrit, 2),
        "\np-value:", formatC(ksTest_Df_label$p_value),
        "\nAlt. hypothesis:", ksTest_Df_label$ksTest_type_short
      )) +
      
      ggplot2::geom_segment(
        ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal),
                     xend = largest_diff_location_l, yend = pre_ecdf(largest_diff_location_gal)),
        linetype = "dashed", color = "blue", size = 2
      ) +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal)), color = "blue", size = 4) +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = pre_ecdf(largest_diff_location_gal)),  color = "blue", size = 4) +
      ggplot2::geom_text(
        ggplot2::aes(x = largest_diff_location_l,
                     y = mean(pre_ecdf(largest_diff_location_gal), post_ecdf(largest_diff_location_gal)),
                     label = paste0("D-stat: ", round(ksTest_Df_label$DStat, 2))),
        hjust = -.5, size = 5, color = "blue"
      ) +
      ggplot2::guides(linetype = "none") +
      ggplot2::scale_x_continuous(
        breaks = seq(0, breaks_end, by = 25000),
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0.01, 0), add = c(1000, 2000))
      ) +
      ggplot2::coord_cartesian(xlim = c(0, x_right), clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5))
  }
  # table_ecdf <- tibble(percentile_values = c(0.25,0.375,0.5, 0.625,0.75,0.875, 0.90),
  #                      pre_ecdf = unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))),
  #                      post_ecdf = unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))))
  # 
  # print(table_ecdf)
  return(p_all_week_cdf)
}





########
# create a KS test function to check how high and low user groups changed their water use
KS_test_function_for_pre_wc_budgetVswaterApp_comparison <- function(PreWC_userCategory,variable, city) { 
  # PreWC_userCategory = Used less than budget, Used more than budget
  # variable = weekly_volume, budgetGal, or residual
  # city = Hyde Park, Logan
  
  user_category = PreWC_userCategory
  variable_name = variable
  city_name = city
  
  if(city_name == "All"){
    weekly_volumetric_df_with_pre_wc_budget_comparison %>%
      filter(name == variable_name) %>%
      filter(user_cat == user_category)-> weekly_df} 
  else{
    weekly_volumetric_df_with_pre_wc_budget_comparison %>%
      filter(name == variable_name) %>%
      filter(City == city_name)%>%
      filter(user_cat == user_category)-> weekly_df
  }
  
  # creating pre-post vectors
  pre_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre", "value"]))
  post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  
  # calculate the critical value
  Dcrit <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly))/((length(pre_weekly) * length(post_weekly))))
  
  # cdf creation
  pre_ecdf <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  # # cdf creation
  # cdf_pre <- ecdf(pre_weekly)
  # cdf_post <- ecdf(post_weekly)
  # 
  #ks test
  if(variable_name == "residual" & user_category == "Used less than budget"){
    
    ksTest_weekly_l <- ks.test(pre_weekly,post_weekly, 
                               alternative = c("greater"))
  } else {
    
    
    ksTest_weekly_l <- ks.test(pre_weekly,post_weekly, 
                               alternative = c("less"))
  }
  
  
  first_length <- ifelse(ksTest_weekly_l$alternative == "the CDF of x lies below that of y",
                         length(pre_weekly), length(post_weekly))
  
  second_length <- ifelse(ksTest_weekly_l$alternative == "the CDF of x lies below that of y",
                          length(post_weekly), length(pre_weekly))
  
  if(length(pre_weekly) == first_length){
    first_data <- pre_weekly
  } else {
    first_data <- post_weekly
  }
  
  if(length(post_weekly) == second_length){
    second_data <- post_weekly
  } else {
    second_data <- pre_weekly
  }
  
  # find largest difference between ecdfs
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]  
  
  # creating result df
  weekly_ks_test_result <- tibble(pvalue = ksTest_weekly_l$p.value,
                                  D = as.numeric(ksTest_weekly_l$statistic),
                                  pre_n = length(pre_weekly),
                                  post_n = length(post_weekly),
                                  alternative_hypothesis = ifelse(ksTest_weekly_l$alternative == "the CDF of x lies below that of y","less",
                                                                  ifelse(ksTest_weekly_l$alternative == "the CDF of x lies above that of y","greater","two.sided")),
                                  variable = variable_name)
  
  # plotting the ks test
  
  p_all_week_cdf<-ggplot(weekly_df, aes(x = value,
                                        group = pre_post, 
                                        color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual(values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(weekly_df$x_label)) +
    ylab("ECDF") +
    #geom_line(size=1) +
    ggtitle(paste0(unique(weekly_df$title), "\n",
                   "City: ", city_name,"\n",
                   "Pre water check user group: ", unique(weekly_df$user_cat),
                   "\nD-stat:", round(ksTest_weekly_l$statistic,2),", D-Crit:", round(Dcrit,2),
                   "\np-value:", formatC(ksTest_weekly_l$p.value),
                   "\nAlt. hypothesis:",weekly_ks_test_result$alternative_hypothesis)) +
    theme(legend.title=element_blank())+
    theme_classic()+
    theme(legend.title=element_blank(),
          legend.position = "top")+
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), 
                     xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue", size=2) +
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=4) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=4) +
    geom_text(aes(x=largest_diff_location, 
                  y = mean(pre_ecdf(largest_diff_location),post_ecdf(largest_diff_location)),
                  label = paste0("D-stat: ",
                                 round(ksTest_weekly_l$statistic,2))),
              hjust = -.5,
              # yjust = -1,
              size = 5,
              color = 'blue')
  
  #plot(p_all_week_cdf)
  return(p_all_week_cdf)
}
# PreWC_userCategory = Used less than budget, Used more than budget
# variable = weekly_volume or residual
# city = All, Hyde Park, Logan

KS_test_function_for_pre_wc_budgetVswaterApp_comparison(PreWC_userCategory = "Used less than budget",
                                                        variable = "residual", city = "All")
KS_test_function_for_pre_wc_budgetVswaterApp_comparison(PreWC_userCategory = "Used more than budget",
                                                        variable = "residual", city = "All")

# Site-wise pre- post-intervention data comparison using KS test for 4 behavior variables

ks_test_plots_siteWise <- function(Siteid, Variable){
  
  siteid = Siteid
  
  variable = as.character(Variable)
  
  data_test %>%
    select(SiteID, pre_post, dvol, dmin, n_eve,d_lastirr) %>%
    pivot_longer(-c(SiteID, pre_post)) %>%
    filter(SiteID == siteid) %>%
    filter(name == variable) %>%
    mutate(label = case_when(variable == "dvol" ~ "Daily irrigation Volume (gal)",
                             variable == 'dmin' ~ "Daily irrigation duration (min)",
                             variable == "d_lastirr" ~ "Days between irrigation (days)",
                             variable == "n_eve" ~ "Daily irrigation events (number)"))-> ks_test_data
  
  # create pre and post-wc variable vectors
  pre_data <- ks_test_data$value[ks_test_data$pre_post == "pre"]
  post_data <- ks_test_data$value[ks_test_data$pre_post == "post"]
  
  # ks test
  
  # for less
  ks_test_result_less <- ks.test(pre_data,post_data,alternative = "less")
  # for greater
  ks_test_result_greater <- ks.test(pre_data,post_data,alternative = "greater")
  # for two sided
  ks_test_result_ts <- ks.test(pre_data,post_data,alternative = "two.sided")
  
  # get the critical value using following formula which was extracted from this website: https://sparky.rice.edu/astr360/kstest.pdf
  # Dcrit = 1.36* sqrt((lenght of pre+ length of post)/(length of pre* length of post)); 
  # where 1.36 is a coefficient for alpha = 0.05 (95% confidence)
  
  Dcrit <- 1.36 * sqrt((length(pre_data) + length(post_data))/((length(pre_data) * length(post_data))))
  
  
  # Create a dataframe with all these values to check what happened (increased, decreased)
  ksTest_Df <- tibble(SiteID = siteid,
                      ksTest_type = c(ks_test_result_less$alternative,ks_test_result_greater$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ks_test_result_less$p.value,ks_test_result_greater$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ks_test_result_less$statistic,ks_test_result_greater$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  ksTest_Df_label <- ksTest_Df %>%
    # filter(ksTest_type != 'two-sided') %>%
    filter(DStat_Bigger == "True" & p_significant == "True" )
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_data), length(post_data)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_data), length(pre_data)))[1]
  
  if(length(pre_data) == first_length){
    first_data <- pre_data
  } else {
    first_data <- post_data
  }
  
  if(length(post_data) == second_length){
    second_data <- post_data
  } else {
    second_data <- pre_data
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]
  
  # check
  pre_ecdf <- ecdf(pre_data)
  
  post_ecdf <- ecdf(post_data)
  
  abs(pre_ecdf(largest_diff_location) - post_ecdf(largest_diff_location))
  
  # plot
  ks_test_data %>% 
    select(pre_post, value) %>%
    ggplot(aes(x = value,
               group = pre_post, 
               color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual("Period",values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(ks_test_data$label)) +
    ylab("ECDF") +
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue") +
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=2) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=2)+
    ggtitle(paste0("SiteID=", siteid, ", D-crit=", round(Dcrit,2), ", D-stat=", round(ksTest_Df_label$DStat,2),
                   "\np-value=", formatC(ksTest_Df_label$p_value))) -> plot_data
  
  return(plot_data)
  
}

# Site-wise pre- post-intervention data comparison using KS test for 4 behavior variables, but dvol in liter
ks_test_plots_siteWise_liter <- function(Siteid, Variable){
  
  siteid = Siteid
  variable = as.character(Variable)
  
  ks_test_data <- data_test %>%
    select(SiteID, pre_post, dvol, dmin, n_eve, d_lastirr) %>%
    pivot_longer(-c(SiteID, pre_post)) %>%
    filter(SiteID == siteid) %>%
    filter(name == variable) %>%
    mutate(
      # convert gallons to liters if variable is dvol
      value = if_else(name == "dvol", value * 3.78541, value),
      label = case_when(
        variable == "dvol"      ~ "Daily irrigation volume (liter)",
        variable == "dmin"      ~ "Daily irrigation duration (min)",
        variable == "d_lastirr" ~ "Days between irrigation (days)",
        variable == "n_eve"     ~ "Daily irrigation events (number)"
      )
    )
  
  # create pre and post-wc variable vectors
  pre_data <- ks_test_data$value[ks_test_data$pre_post == "pre"]
  post_data <- ks_test_data$value[ks_test_data$pre_post == "post"]
  
  # ks test
  
  # for less
  ks_test_result_less <- ks.test(pre_data,post_data,alternative = "less")
  # for greater
  ks_test_result_greater <- ks.test(pre_data,post_data,alternative = "greater")
  # for two sided
  ks_test_result_ts <- ks.test(pre_data,post_data,alternative = "two.sided")
  
  # get the critical value using following formula which was extracted from this website: https://sparky.rice.edu/astr360/kstest.pdf
  # Dcrit = 1.36* sqrt((lenght of pre+ length of post)/(length of pre* length of post)); 
  # where 1.36 is a coefficient for alpha = 0.05 (95% confidence)
  
  Dcrit <- 1.36 * sqrt((length(pre_data) + length(post_data))/((length(pre_data) * length(post_data))))
  
  
  # Create a dataframe with all these values to check what happened (increased, decreased)
  ksTest_Df <- tibble(SiteID = siteid,
                      ksTest_type = c(ks_test_result_less$alternative,ks_test_result_greater$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ks_test_result_less$p.value,ks_test_result_greater$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ks_test_result_less$statistic,ks_test_result_greater$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  ksTest_Df_label <- ksTest_Df %>%
    # filter(ksTest_type != 'two-sided') %>%
    filter(DStat_Bigger == "True" & p_significant == "True" )
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_data), length(post_data)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_data), length(pre_data)))[1]
  
  if(length(pre_data) == first_length){
    first_data <- pre_data
  } else {
    first_data <- post_data
  }
  
  if(length(post_data) == second_length){
    second_data <- post_data
  } else {
    second_data <- pre_data
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]
  
  # check
  pre_ecdf <- ecdf(pre_data)
  
  post_ecdf <- ecdf(post_data)
  
  abs(pre_ecdf(largest_diff_location) - post_ecdf(largest_diff_location))
  
  # plot
  ks_test_data %>% 
    select(pre_post, value) %>%
    ggplot(aes(x = value,
               group = pre_post, 
               color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual("Period",values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(ks_test_data$label)) +
    ylab("ECDF") +
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue") +
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=2) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=2)+
    ggtitle(paste0("SiteID=", siteid, ", D-crit=", round(Dcrit,2), ", D-stat=", round(ksTest_Df_label$DStat,2),
                   "\np-value=", formatC(ksTest_Df_label$p_value))) -> plot_data
  
  return(plot_data)
  
}


# variables: dvol, dmin, d_lastirr, n_eve

# ks_test_plots_siteWise(15,"n_eve")

# # Check the changes

ks_test_df_function <- function(Siteid, Variable){
  
  siteid = Siteid
  variable = as.character(Variable)
  
  data_test %>%
    select(SiteID, pre_post, dvol, dmin, n_eve,d_lastirr) %>%
    pivot_longer(-c(SiteID, pre_post)) %>%
    filter(SiteID == siteid) %>%
    filter(name == variable) %>%
    mutate(label = case_when(variable == "dvol" ~ "Daily irrigation Volume (gal)",
                             variable == "dmin" ~ "Daily irrigation duration (min)",
                             variable == "d_lastirr" ~ "Days between irrigation (days)",
                             variable == "n_eve" ~ "Daily irrigation events (number)"))-> ks_test_data
  
  # create pre and post-wc variable vectors
  pre_data <- ks_test_data$value[ks_test_data$pre_post == "pre"]
  post_data <- ks_test_data$value[ks_test_data$pre_post == "post"]
  
  # ks test
  
  # for less
  ks_test_result_less <- ks.test(pre_data,post_data,alternative = "less")
  # for greater
  ks_test_result_greater <- ks.test(pre_data,post_data,alternative = "greater")
  # for two sided
  ks_test_result_ts <- ks.test(pre_data,post_data,alternative = "two.sided")
  
  # get the critical value using following formula which was extracted from this website: https://sparky.rice.edu/astr360/kstest.pdf
  # Dcrit = 1.36* sqrt((lenght of pre+ length of post)/(length of pre* length of post)); where 1.36 is a coefficient for alpha = 0.05 (95% confidence)
  
  Dcrit <- 1.36 * sqrt((length(pre_data) + length(post_data))/((length(pre_data) * length(post_data))))
  
  
  # Create a dataframe with all these values to check what happened (increased, decreased, or no change)
  return(ksTest_Df <- tibble(SiteID = siteid,
                             ksTest_type = c(ks_test_result_less$alternative,ks_test_result_greater$alternative,ks_test_result_ts$alternative),
                             ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                             ksTest_type == "the CDF of x lies above that of y" ~ "greater",
                                                             ksTest_type == "two-sided" ~ "two.sided")),
                             p_value = c(ks_test_result_less$p.value,ks_test_result_greater$p.value,ks_test_result_ts$p.value),
                             Dcrit = Dcrit,
                             DStat = c(ks_test_result_less$statistic,ks_test_result_greater$statistic,ks_test_result_ts$statistic),
                             DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                             p_significant = ifelse(p_value < 0.05, "True", "False")))
  
  
}

# test
# ks_test_df_function(2,"dmin")
###########################################################################
# For comparing other behavioral variable at weekly (duration, and number of event  (Table 5))
#####################################################################
# KS test for all behavior variables (duration, event) using pooled data
df_test_weekly_volumetric %>%
  left_join(Sites) %>% ungroup() %>%
  select(pre_post,total_irrigation_minutes, frequency,  City) %>%
  relocate(City, .before = pre_post)%>%
  pivot_longer(cols = total_irrigation_minutes:frequency) %>%
  mutate(x_label = case_when(name == "total_irrigation_minutes" ~ "Weekly duration (minutes/week)",
                             name == "frequency" ~ "Weekly Events (Events/week)"),
         title = case_when(name == "total_irrigation_minutes" ~ "K-S Test for all weekly pre and post irrigation duration", 
                           name == "frequency" ~ "K-S Test for all weekly pre and post pre and post irrigation event (number)")
  )  -> duration_eventFreq_weeklyData

# write the function
KS_test_weekly_function_duration_eventNo <- function(variable, city) { 
  # variable = "frequency"
  # city = "All"
  #comparison_period = comparisonPeriod
  variable_name = variable
  city_name = city
  
  if(city_name == "All"){
    duration_eventFreq_weeklyData %>%
      filter(name == variable_name) -> weekly_df
  } else {
    duration_eventFreq_weeklyData %>%
      filter(name == variable_name) %>%
      filter(City == city_name)-> weekly_df
  }
  # creating pre-post vectors
  pre_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre", "value"]))
  post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  
  
  # Critical value for D-statistic
  Dcrit <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly))/((length(pre_weekly) * length(post_weekly))))
  
  # cdf creation
  pre_ecdf <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  #ks test
  ksTest_weekly_l <- ks.test(pre_weekly,post_weekly, 
                             alternative = c("less"))
  ksTest_weekly_g <- ks.test(pre_weekly,post_weekly, 
                             alternative = c("greater"))
  ksTest_weekly_ts <- ks.test(pre_weekly,post_weekly, 
                              alternative = c("two.sided"))
  
  ksTest_Df <- tibble(ksTest_type = c(ksTest_weekly_l$alternative,ksTest_weekly_g$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ksTest_weekly_l$p.value,ksTest_weekly_g$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ksTest_weekly_l$statistic,ksTest_weekly_g$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  ksTest_Df %>%
    select(ksTest_type_short, DStat_Bigger,p_significant) %>%
    pivot_longer(cols = DStat_Bigger:p_significant) ->randomCheck
  
  
  true_false <- paste0(ksTest_Df[1,6] , ksTest_Df[1,7])
  # df of ks-test for labeling purposes
  
  if (true_false == "FalseTrue" | true_false == "FalseFalse" | true_false == "TrueFalse") {
    ksTest_Df %>%
      filter(ksTest_type_short == 'less') -> ksTest_Df_label
    
  } else {
    ksTest_Df %>%
      filter(DStat_Bigger == "True" & p_significant == "True") -> ksTest_Df_label
  }
  
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_weekly), length(post_weekly)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_weekly), length(pre_weekly)))[1]
  
  if(length(pre_weekly) == first_length){
    first_data <- pre_weekly
  } else {
    first_data <- post_weekly
  }
  
  if(length(post_weekly) == second_length){
    second_data <- post_weekly
  } else {
    second_data <- pre_weekly
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]  
  
  
  # # creating result df
  # weekly_ks_test_result <- tibble(pvalue = ksTest_weekly_l$p.value,
  #                                 D = as.numeric(ksTest_weekly_l$statistic),
  #                                 pre_n = length(pre_weekly),
  #                                 post_n = length(post_weekly),
  #                                 alternative_hypothesis = ifelse(ksTest_weekly_l$alternative == "the CDF of x lies below that of y","less",
  #                                                                 ifelse(ksTest_weekly_l$alternative == "the CDF of x lies above that of y","greater","two.sided")),
  #                                 variable = variable_name)
  
  # data for shaded region
  
  preQuntile <- unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5, 0.625,0.75,0.875)))
  postQuntile <- unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  
  
  # plotting the ks test
  
  p_all_week_cdf<-  ggplot(weekly_df, aes(x = value,
                                          group = pre_post, 
                                          color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual(values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(weekly_df$x_label)) +
    ylab("ECDF") +
    #geom_line(size=1) +
    ggtitle(paste0("City: ", city_name,"\n",
                   unique(weekly_df$title), 
                   "\nD-stat:",
                   round(ksTest_Df_label$DStat,2),", D-Crit:", round(ksTest_Df_label$Dcrit,2),
                   "\np-value:", formatC(ksTest_Df_label$p_value),
                   "\nAlt. hypothesis:",ksTest_Df_label$ksTest_type_short)) +
    theme_classic()+
    theme(legend.title=element_blank(),
          legend.position = "top")+
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), 
                     xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue", size=2) +
    geom_segment(aes(x = preQuntile[1], y = .25, 
                     xend = postQuntile[1], yend = .25),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[2], y = 0.375, 
                     xend = postQuntile[2], yend = 0.375),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[3], y = 0.5, 
                     xend = postQuntile[3], yend = 0.5),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[4], y = 0.625, 
                     xend = postQuntile[4], yend = 0.625),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[5], y = 0.75, 
                     xend = postQuntile[5], yend = 0.75),
                 linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[6], y = 0.875, 
                     xend = postQuntile[6], yend = 0.875),
                 linetype = "dotted", color = "brown")+
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=4) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=4) +
    geom_text(aes(x=largest_diff_location, 
                  y = mean(pre_ecdf(largest_diff_location),post_ecdf(largest_diff_location)),
                  label = paste0("D-stat: ",
                                 round(ksTest_Df_label$DStat,2))),
              hjust = -.5,
              # yjust = -1,
              size = 5,
              color = 'blue')
  
  print(paste0("pre-largest diff percentile=", pre_ecdf(largest_diff_location), "and the value=",largest_diff_location))
  print(paste0("post-largest diff percentile=", post_ecdf(largest_diff_location)))
  
  table_ecdf <- tibble(percentile_values = c(0.25,0.375,0.5, 0.625,0.75,0.875, 0.90),
                       pre_ecdf = unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))),
                       post_ecdf = unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))))
  
  print(table_ecdf)
  
  # plot(p_all_week_cdf)
  return(p_all_week_cdf)
}


# variable = total_irrigation_minutes, frequency
# city = Hyde Park, Logan
#comparison_period = comparisonPeriod


# KS_test_weekly_function_duration_eventNo("frequency", "All")
# KS_test_weekly_function_duration_eventNo("total_irrigation_minutes", "All")

#####################################################
# daily pooled data for ks testing (Table 5)
data_test %>%
  left_join(Sites %>% select(SiteID,City)) %>% ungroup() %>%
  select(City, pre_post, dvol, dmin, n_eve,d_lastirr) %>%
  pivot_longer(-c(City, pre_post)) -> data_test2


ks_test_plots <- function(City, Variable ){
  
  
  
  
  variable_name = as.character(Variable)
  city_name = as.character(City)
  
  # variable_name = 'dmin'
  # city_name = 'Logan'
  
  
  
  if(city_name == "All"){
    data_test2 %>%
      filter(name == variable_name) -> daily_df
  } else{
    data_test2 %>%
      filter(name == variable_name) %>%
      filter(City == city_name)-> daily_df
  }
  
  
  daily_df  %>%
    mutate(label = case_when(variable_name == "dvol" ~ "Daily irrigation Volume (gal)",
                             variable_name == 'dmin' ~ "Daily irrigation duration (min)",
                             variable_name == "d_lastirr" ~ "Days between irrigation (days)",
                             variable_name == "n_eve" ~ "Daily irrigation events (number)"))-> ks_test_data
  
  
  
  
  
  
  # create pre and post-wc variable vectors
  pre_data <- ks_test_data$value[ks_test_data$pre_post == "pre"]
  post_data <- ks_test_data$value[ks_test_data$pre_post == "post"]
  
  # ks test
  
  # for less
  ks_test_result_less <- ks.test(pre_data,post_data,alternative = "less")
  # for greater
  ks_test_result_greater <- ks.test(pre_data,post_data,alternative = "greater")
  # for two sided
  ks_test_result_ts <- ks.test(pre_data,post_data,alternative = "two.sided")
  
  # get the critical value using following formula which was extracted from this website: https://sparky.rice.edu/astr360/kstest.pdf
  # Dcrit = 1.36* sqrt((lenght of pre+ length of post)/(length of pre* length of post)); 
  # where 1.36 is a coefficient for alpha = 0.05 (95% confidence)
  
  Dcrit <- 1.36 * sqrt((length(pre_data) + length(post_data))/((length(pre_data) * length(post_data))))
  
  
  # Create a dataframe with all these values to check what happened (increased, decreased)
  ksTest_Df <- tibble(ksTest_type = c(ks_test_result_less$alternative,ks_test_result_greater$alternative),
                      ksTest_type_short = c(case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                                                      ksTest_type == "the CDF of x lies above that of y" ~ "greater")),
                      p_value = c(ks_test_result_less$p.value,ks_test_result_greater$p.value),
                      Dcrit = Dcrit,
                      DStat = c(ks_test_result_less$statistic,ks_test_result_greater$statistic),
                      DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
                      p_significant = ifelse(p_value < 0.05, "True", "False"))
  
  
  ksTest_Df %>%
    select(ksTest_type_short, DStat_Bigger,p_significant) %>%
    pivot_longer(cols = DStat_Bigger:p_significant) ->randomCheck
  
  
  true_false <- paste0(ksTest_Df[1,6] , ksTest_Df[1,7])
  
  
  # df of ks-test for labeling purposes
  if (true_false == "FalseTrue" | true_false == "FalseFalse" | true_false == "TrueFalse") {
    ksTest_Df %>%
      filter(ksTest_type_short == 'less') -> ksTest_Df_label
    
  } else {
    ksTest_Df %>%
      filter(DStat_Bigger == "True" & p_significant == "True") -> ksTest_Df_label
  }
  
  
  
  
  # ksTest_Df_label <- ksTest_Df %>%
  #   # filter(ksTest_type != 'two-sided') %>%
  #   filter(DStat_Bigger == "True" & p_significant == "True" )
  
  # now find the location of greatest vertical distance
  # we need to find if the household increased or decreased the variable value
  
  first_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                   length(pre_data), length(post_data)))[1]
  
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == 'less',
                                    length(post_data), length(pre_data)))[1]
  
  if(length(pre_data) == first_length){
    first_data <- pre_data
  } else {
    first_data <- post_data
  }
  
  if(length(post_data) == second_length){
    second_data <- post_data
  } else {
    second_data <- pre_data
  }
  
  # pre_length <- length(pre_data)
  # post_length <- length(post_data)
  
  combined_data <- c(first_data,second_data)
  
  combined_data_rank <- order(combined_data)
  
  location_assign <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  
  largest_diff_location <- combined_data[combined_data_rank[biggest_value_location]]
  
  # check
  pre_ecdf <- ecdf(pre_data)
  
  post_ecdf <- ecdf(post_data)
  
  abs(pre_ecdf(largest_diff_location) - post_ecdf(largest_diff_location))
  
  
  # data for shaded region
  
  preQuntile <- unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5, 0.625,0.75,0.875)))
  postQuntile <- unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  
  
  # plot
  ks_test_data %>% 
    select(pre_post, value) %>%
    ggplot(aes(x = value,
               group = pre_post, 
               color = pre_post))+
    stat_ecdf(size=1) + scale_color_manual("Period",values = c("#3275a8","pink"))+
    theme_bw(base_size = 12) +
    theme(legend.position ="top") +
    xlab(unique(ks_test_data$label)) +
    ylab("ECDF") +
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), 
                     xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue", size=2) +
    geom_segment(aes(x = preQuntile[1], y = .25, 
                     xend = postQuntile[1], yend = .25),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[2], y = 0.375, 
                     xend = postQuntile[2], yend = 0.375),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[3], y = 0.5, 
                     xend = postQuntile[3], yend = 0.5),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[4], y = 0.625, 
                     xend = postQuntile[4], yend = 0.625),
                 linetype = "dotted", color = "brown")+
    geom_segment(aes(x = preQuntile[5], y = 0.75, 
                     xend = postQuntile[5], yend = 0.75),
                 linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[6], y = 0.875, 
                     xend = postQuntile[6], yend = 0.875),
                 linetype = "dotted", color = "brown") +
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location), xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue") +
    geom_point(aes(x = largest_diff_location , y= post_ecdf(largest_diff_location)), color="blue", size=2) +
    geom_point(aes(x = largest_diff_location , y= pre_ecdf(largest_diff_location)), color="blue", size=2)+
    ggtitle(paste0("City=",city_name,", ",unique(ks_test_data$label), 
                   "\nD-crit=", round(Dcrit,2), ", D-stat=", round(ksTest_Df_label$DStat,2),
                   "\np-value=", formatC(ksTest_Df_label$p_value))) -> plot_data
  
  table_ecdf <- tibble(percentile_values = c(0.25,0.375,0.5, 0.625,0.75,0.875, 0.90),
                       pre_ecdf = unname(quantile(pre_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))),
                       post_ecdf = unname(quantile(post_ecdf,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90))))
  print(table_ecdf)
  
  return(plot_data)
  
}
#3 variables: dmin, dvol, d_lastirr, n_eve

ks_test_plots("All","d_lastirr")
########################################################
# All additional functions created for the manuscript
KS_test_weekly_function_advanced_liter <- function(variable, city) {
  # variable = weekly_volume, budgetGal, residual, or "weekly_volume_budgetGal"
  conv <- 3.78541  # gallons -> liters
  
  variable_name <- variable
  city_name     <- city
  
  # ---------------- Filter data ----------------
  if (city_name == "All" & variable_name != "weekly_volume_budgetGal") {
    weekly_df <- dplyr::filter(all_pre_post_weekly, name == variable_name)
  } else if (city_name == "All" & variable_name == "weekly_volume_budgetGal") {
    weekly_df <- dplyr::filter(all_pre_post_weekly, name %in% c("weekly_volume","budgetGal"))
  } else if (city_name != "All" & variable_name != "weekly_volume_budgetGal") {
    weekly_df <- all_pre_post_weekly %>% dplyr::filter(name == variable_name, City == city_name)
  } else {
    weekly_df <- all_pre_post_weekly %>% dplyr::filter(name %in% c("weekly_volume","budgetGal"), City == city_name)
  }
  
  # liters for plotting (keep gallons in 'value' for stats)
  weekly_df <- weekly_df %>% dplyr::mutate(value_l = value * conv)
  
  # ---------------- Pre/Post vectors (gallons) ----------------
  if (variable_name != "weekly_volume_budgetGal") {
    pre_weekly  <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre",  "value"]))
    post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  } else {
    pre_weekly  <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre"  & weekly_df$name == "weekly_volume",  "value"]))
    post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post" & weekly_df$name == "weekly_volume", "value"]))
  }
  
  # ---------------- KS stats (gallons) ----------------
  Dcrit     <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly)) / (length(pre_weekly) * length(post_weekly)))
  pre_ecdf  <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  ksTest_weekly_l <- ks.test(pre_weekly, post_weekly, alternative = "less")
  ksTest_weekly_g <- ks.test(pre_weekly, post_weekly, alternative = "greater")
  
  ksTest_Df <- tibble::tibble(
    ksTest_type       = c(ksTest_weekly_l$alternative, ksTest_weekly_g$alternative),
    ksTest_type_short = c("less","greater"),
    p_value           = c(ksTest_weekly_l$p.value, ksTest_weekly_g$p.value),
    Dcrit             = Dcrit,
    DStat             = c(ksTest_weekly_l$statistic, ksTest_weekly_g$statistic),
    DStat_Bigger      = ifelse(DStat > Dcrit, "True", "False"),
    p_significant     = ifelse(p_value < 0.05, "True", "False")
  )
  ksTest_Df_label <- dplyr::filter(ksTest_Df, DStat_Bigger == "True", p_significant == "True")
  
  # ---------------- Largest vertical gap ----------------
  first_length  <- ifelse(ksTest_Df_label$ksTest_type_short[1] == "less", length(pre_weekly),  length(post_weekly))
  second_length <- ifelse(ksTest_Df_label$ksTest_type_short[1] == "less", length(post_weekly), length(pre_weekly))
  
  first_data  <- if (length(pre_weekly)  == first_length)  pre_weekly  else post_weekly
  second_data <- if (length(post_weekly) == second_length) post_weekly else pre_weekly
  
  combined_data         <- c(first_data, second_data)
  combined_data_rank    <- order(combined_data)
  location_assign       <- cumsum(ifelse(combined_data_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  largest_diff_location_gal <- combined_data[combined_data_rank[biggest_value_location]]
  largest_diff_location_l   <- largest_diff_location_gal * conv
  largest_diff_location_l   <- largest_diff_location_gal * conv
  
  # quantile guide x-positions (liters)
  preQuntile_l  <- unname(quantile(pre_ecdf,  na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))  * conv
  postQuntile_l <- unname(quantile(post_ecdf, na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875))) * conv
  # ------- NEW: Means -------
  pre_mean  <- mean(pre_weekly* conv,  na.rm = TRUE)
  post_mean <- mean(post_weekly* conv, na.rm = TRUE)
  # ---------------- Axis range (liters) ----------------
  max_x_l    <- max(weekly_df$value_l, largest_diff_location_l, preQuntile_l, postQuntile_l, na.rm = TRUE)
  breaks_end <- ceiling(max_x_l / 25000) * 25000
  # we'll set limits via coord_cartesian() to preserve 'expand' padding
  x_right <- breaks_end + 2000
  
  # ---------------- Plotting ----------------
  if (variable_name != "weekly_volume_budgetGal") {
    p_all_week_cdf <-
      ggplot2::ggplot(weekly_df, ggplot2::aes(x = value_l, group = pre_post, color = pre_post)) +
      ggplot2::stat_ecdf(size = 1) +
      ggplot2::scale_color_manual(
        values = c("post" = "#3275a8", "pre" = "pink"),
        breaks = c("post","pre"),
        labels = c("post-WC", "pre-WC")
      ) +
      ggplot2::theme_classic() +
      # ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
      ggplot2::xlab(gsub("gallons/week","liters/week", unique(weekly_df$x_label), ignore.case = TRUE)) +
      ggplot2::ylab("ECDF") +
      ggplot2::ggtitle(paste0(
        "City: ", city_name, "\n", unique(weekly_df$title),
        "\nD-stat:", round(ksTest_Df_label$DStat, 2), ", D-Crit:", round(ksTest_Df_label$Dcrit, 2),
        "\np-value:", formatC(ksTest_Df_label$p_value),
        "\nAlt. hypothesis:", ksTest_Df_label$ksTest_type_short
      )) +
      
      ggplot2::geom_segment(
        ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal),
                     xend = largest_diff_location_l, yend = pre_ecdf(largest_diff_location_gal)),
        linetype = "dashed", color = "blue", size = 2
      ) +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[1], y = .25,   xend = postQuntile_l[1], yend = .25),   linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[2], y = 0.375, xend = postQuntile_l[2], yend = 0.375), linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[3], y = 0.5,   xend = postQuntile_l[3], yend = 0.5),   linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[4], y = 0.625, xend = postQuntile_l[4], yend = 0.625), linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[5], y = 0.75,  xend = postQuntile_l[5], yend = 0.75),  linetype = "dotted", color = "brown") +
      ggplot2::geom_segment(ggplot2::aes(x = preQuntile_l[6], y = 0.875, xend = postQuntile_l[6], yend = 0.875), linetype = "dotted", color = "brown") +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal)), color = "blue", size = 4) +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = pre_ecdf(largest_diff_location_gal)),  color = "blue", size = 4) +
      ggplot2::geom_text(
        ggplot2::aes(x = largest_diff_location_l,
                     y = mean(pre_ecdf(largest_diff_location_gal), post_ecdf(largest_diff_location_gal)),
                     label = paste0("D-stat: ", round(ksTest_Df_label$DStat, 2))),
        hjust = -.5, size = 5, color = "blue"
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, breaks_end, by = 25000),
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0.01, 0), add = c(1000, 2000)) # small left pad, keep right headroom
      ) +
      ggplot2::coord_cartesian(xlim = c(0, x_right), clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5))
    
  } else {
    p_all_week_cdf <-
      weekly_df %>%
      dplyr::mutate(variable_type = paste0(pre_post, "_", name)) %>%
      dplyr::select(variable_type, value_l) %>%
      dplyr::mutate(lineType = ifelse(grepl("budget", variable_type, fixed = TRUE), "budget", "water used")) %>%
      ggplot2::ggplot(ggplot2::aes(x = value_l)) +
      ggplot2::stat_ecdf(size = 1, ggplot2::aes(group = variable_type, color = variable_type, linetype = lineType)) +
      ggplot2::scale_linetype_manual("Variable", values = c("dashed", "solid")) +
      ggplot2::scale_color_manual(
        "Variable",
        values = c("#96C3F5","#3275a8","#E3A1B2","#E51349"),
        labels = c("post-WC Budget", "post-WC Water Used",
                   "pre-WC Budget",  "pre-WC Water Used")
      ) +
      
      #ggplot2::theme_bw() +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
      ggplot2::xlab("Water Volume (liters/week)") +
      ggplot2::ylab("ECDF") +
      ggplot2::ggtitle(paste0(
        "City: ", city_name, "\n",
        "Comparison between actual pre- and post-Water Check\nWater use and Water budget (requirement)",
        "\nD-stat:", round(ksTest_Df_label$DStat, 2), ", D-Crit:", round(ksTest_Df_label$Dcrit, 2),
        "\np-value:", formatC(ksTest_Df_label$p_value),
        "\nAlt. hypothesis:", ksTest_Df_label$ksTest_type_short
      )) +
      
      ggplot2::geom_segment(
        ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal),
                     xend = largest_diff_location_l, yend = pre_ecdf(largest_diff_location_gal)),
        linetype = "dashed", color = "blue", size = 2
      ) +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = post_ecdf(largest_diff_location_gal)), color = "blue", size = 4) +
      ggplot2::geom_point(ggplot2::aes(x = largest_diff_location_l, y = pre_ecdf(largest_diff_location_gal)),  color = "blue", size = 4) +
      ggplot2::geom_text(
        ggplot2::aes(x = largest_diff_location_l,
                     y = mean(pre_ecdf(largest_diff_location_gal), post_ecdf(largest_diff_location_gal)),
                     label = paste0("D-stat: ", round(ksTest_Df_label$DStat, 2))),
        hjust = -.5, size = 5, color = "blue"
      ) +
      ggplot2::guides(linetype = "none") +
      ggplot2::scale_x_continuous(
        breaks = seq(0, breaks_end, by = 25000),
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0.01, 0), add = c(1000, 2000))
      ) +
      ggplot2::coord_cartesian(xlim = c(0, x_right), clip = "off") +
      ggplot2::theme(plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5))
  }
  # Optional: diagnostics in console
  message(sprintf("pre-largest diff percentile=%.3f, value=%.3f",
                  pre_ecdf(largest_diff_location_gal), largest_diff_location_l))
  message(sprintf("post-largest diff percentile=%.3f", post_ecdf(largest_diff_location_gal)))
  # mean table
  mean_table <- tibble(variable = variable_name,
                       preMean = pre_mean,
                       postMean = post_mean)
  # percentile table
  table_ecdf <- tibble(percentile_values = c(0.25,0.375,0.5, 0.625,0.75,0.875, 0.90),
                       pre_ecdf = unname(quantile(pre_ecdf ,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90)))* conv,
                       post_ecdf = unname(quantile(post_ecdf ,na.rm = T,probs = c(0.25,0.375,0.5,0.625,0.75,0.875, 0.90)))* conv)
  
  print(mean_table)
  print(table_ecdf)
  return(p_all_week_cdf)
}

KS_test_weekly_function_advanced_liter(variable = "weekly_volume", city = "All") # when using weekly_volume it is not changing the x-axis title


KS_test_weekly_function_duration_eventNo_update <- function(variable, city) { 
  variable_name <- variable
  city_name <- city
  
  if (city_name == "All") {
    weekly_df <- duration_eventFreq_weeklyData %>%
      filter(name == variable_name)
  } else {
    weekly_df <- duration_eventFreq_weeklyData %>%
      filter(name == variable_name, City == city_name)
  }
  
  # pre/post vectors
  pre_weekly  <- as.vector(unlist(weekly_df[weekly_df$pre_post == "pre",  "value"]))
  post_weekly <- as.vector(unlist(weekly_df[weekly_df$pre_post == "post", "value"]))
  
  # Critical value for D-statistic
  Dcrit <- 1.36 * sqrt((length(pre_weekly) + length(post_weekly)) /
                         (length(pre_weekly) * length(post_weekly)))
  
  # ECDFs
  pre_ecdf  <- ecdf(pre_weekly)
  post_ecdf <- ecdf(post_weekly)
  
  # KS tests
  ksTest_weekly_l  <- ks.test(pre_weekly, post_weekly, alternative = "less")
  ksTest_weekly_g  <- ks.test(pre_weekly, post_weekly, alternative = "greater")
  ksTest_weekly_ts <- ks.test(pre_weekly, post_weekly, alternative = "two.sided")
  
  ksTest_Df <- tibble(
    ksTest_type       = c(ksTest_weekly_l$alternative, ksTest_weekly_g$alternative),
    ksTest_type_short = c(
      case_when(ksTest_type == "the CDF of x lies below that of y" ~ "less",
                ksTest_type == "the CDF of x lies above that of y" ~ "greater")
    ),
    p_value      = c(ksTest_weekly_l$p.value, ksTest_weekly_g$p.value),
    Dcrit        = Dcrit,
    DStat        = c(ksTest_weekly_l$statistic, ksTest_weekly_g$statistic),
    DStat_Bigger = ifelse(DStat > Dcrit, "True", "False"),
    p_significant = ifelse(p_value < 0.05, "True", "False")
  )
  
  true_false <- paste0(ksTest_Df[1,6], ksTest_Df[1,7])
  if (true_false %in% c("FalseTrue", "FalseFalse", "TrueFalse")) {
    ksTest_Df_label <- ksTest_Df %>% filter(ksTest_type_short == "less")
  } else {
    ksTest_Df_label <- ksTest_Df %>% filter(DStat_Bigger == "True", p_significant == "True")
  }
  
  # Location of max vertical distance
  first_length  <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == "less",
                                    length(pre_weekly), length(post_weekly)))[1]
  second_length <- as.vector(ifelse(ksTest_Df_label$ksTest_type_short == "less",
                                    length(post_weekly), length(pre_weekly)))[1]
  
  first_data  <- if (length(pre_weekly)  == first_length)  pre_weekly  else post_weekly
  second_data <- if (length(post_weekly) == second_length) post_weekly else pre_weekly
  
  combined_data <- c(first_data, second_data)
  combined_rank <- order(combined_data)
  location_assign <- cumsum(ifelse(combined_rank <= first_length, second_length, -first_length))
  biggest_value_location <- which.max(abs(location_assign))
  largest_diff_location  <- combined_data[combined_rank[biggest_value_location]]
  
  # Quantiles (already shown)
  preQuntile  <- unname(quantile(pre_ecdf,  na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  postQuntile <- unname(quantile(post_ecdf, na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875)))
  
  # ------- NEW: Means -------
  pre_mean  <- mean(pre_weekly,  na.rm = TRUE)
  post_mean <- mean(post_weekly, na.rm = TRUE)
  # --------------------------
  
  # Plot
  p_all_week_cdf <- ggplot(weekly_df, aes(x = value, group = pre_post, color = pre_post)) +
    stat_ecdf(size = 1) +
    scale_color_manual(values = c("#3275a8", "pink")) +  # post=blue, pre=pink (alphabetical mapping)
    theme_bw(base_size = 12) +
    theme(legend.position = "top") +
    xlab(unique(weekly_df$x_label)) +
    ylab("ECDF") +
    ggtitle(paste0("City: ", city_name, "\n",
                   unique(weekly_df$title),
                   "\nD-stat:", round(ksTest_Df_label$DStat, 2),
                   ", D-Crit:", round(ksTest_Df_label$Dcrit, 2),
                   "\np-value:", formatC(ksTest_Df_label$p_value),
                   "\nAlt. hypothesis:", ksTest_Df_label$ksTest_type_short)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = "top") +
    # KS gap
    geom_segment(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location),
                     xend = largest_diff_location, yend = pre_ecdf(largest_diff_location)),
                 linetype = "dashed", color = "blue", size = 2) +
    # Quantile offsets (existing)
    geom_segment(aes(x = preQuntile[1],  y = 0.25,  xend = postQuntile[1],  yend = 0.25),  linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[2],  y = 0.375, xend = postQuntile[2], yend = 0.375), linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[3],  y = 0.5,   xend = postQuntile[3], yend = 0.5),   linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[4],  y = 0.625, xend = postQuntile[4], yend = 0.625), linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[5],  y = 0.75,  xend = postQuntile[5], yend = 0.75),  linetype = "dotted", color = "brown") +
    geom_segment(aes(x = preQuntile[6],  y = 0.875, xend = postQuntile[6], yend = 0.875), linetype = "dotted", color = "brown") +
    geom_point(aes(x = largest_diff_location, y = post_ecdf(largest_diff_location)), color = "blue", size = 4) +
    geom_point(aes(x = largest_diff_location, y = pre_ecdf(largest_diff_location)),  color = "blue", size = 4) +
    geom_text(aes(x = largest_diff_location, 
                  y = mean(pre_ecdf(largest_diff_location), post_ecdf(largest_diff_location)),
                  label = paste0("D-stat: ", round(ksTest_Df_label$DStat, 2))),
              hjust = -0.5, size = 5, color = "blue") +
    # ------- NEW: show means -------
  geom_vline(xintercept = pre_mean,  color = "pink",    linetype = "longdash", size = 0.8) +
    geom_vline(xintercept = post_mean, color = "#3275a8", linetype = "longdash", size = 0.8) +
    annotate("label", x = pre_mean,  y = 0.98,
             label = paste0("Pre mean = ", round(pre_mean, 1)),
             color = "pink", size = 3, vjust = 1, label.size = NA) +
    annotate("label", x = post_mean, y = 0.94,
             label = paste0("Post mean = ", round(post_mean, 1)),
             color = "#3275a8", size = 3, vjust = 1, label.size = NA)
  # -------------------------------
  
  # Optional: diagnostics in console
  message(sprintf("pre-largest diff percentile=%.3f, value=%.3f",
                  pre_ecdf(largest_diff_location), largest_diff_location))
  message(sprintf("post-largest diff percentile=%.3f", post_ecdf(largest_diff_location)))
  
  mean_table <- tibble(variable = variable_name,
                       preMean = pre_mean,
                       postMean = post_mean)
  
  table_ecdf <- tibble(
    percentile_values = c(0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.90),
    pre_ecdf  = unname(quantile(pre_ecdf,  na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875,0.90))),
    post_ecdf = unname(quantile(post_ecdf, na.rm = TRUE, probs = c(0.25,0.375,0.5,0.625,0.75,0.875,0.90)))
  )
  print(paste0("Dstat=",ksTest_Df_label$DStat))
  print(paste0("Dcrit=",Dcrit))
  print(table_ecdf)
  print(mean_table)
  return(p_all_week_cdf)
}
KS_test_weekly_function_duration_eventNo("frequency", "All")
KS_test_weekly_function_duration_eventNo_update("frequency", "All")


KS_test_weekly_function_duration_eventNo("total_irrigation_minutes", "All")
KS_test_weekly_function_duration_eventNo_update("total_irrigation_minutes", "All")