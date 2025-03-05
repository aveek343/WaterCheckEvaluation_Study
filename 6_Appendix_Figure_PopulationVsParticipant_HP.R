# Author: Mahmud Aveek, David Rosenberg
# Date: 06-09-2024
# Runtime: 30 seconds

# This is an R document. This document compares summer water use in 2022 for the entire
# population of Hyde Park single family residents (n = 1726) to the opt-in sample of households for the Utah
# State University Extension Flume/Water Check Study (n = 40). The purpose is to identify whether there
# are statistical differences between the study sample of households and the city population.
# The comparison is presented multiple ways with different data visualizations and one Kolgomorv-Smirnov
# statistical test (KS-Test).
# The plots are:
#   1. Cumulative distributions of participants and population
#   2. Quantile-Quantile plot

# files required are: md_withSiteID_anonymous_summer.csv # summer water use data for the Hyde Park Population

# rm(list=ls())


cPackages <- c("versions", "dygraphs", "tidyquant", "xts", "tidyquant","lubridate", "stringr", "RColorBrewer" )
# Install packages not yet installed
installed_packages <- cPackages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(cPackages[!installed_packages])
}
# Packages loading
invisible(lapply(cPackages, library, character.only = TRUE))


# read the csv file with anonymized summer water use data
hydeParkWaterUse <- read.csv('md_withSiteID_anonymous_summer_HP.csv')

# add summer usage

hydeParkPop_summerUse <- hydeParkWaterUse %>% ungroup() %>%
  group_by(SiteID) %>%
  summarize(SummerUsage = sum(Usage)) %>%
  mutate(Group = 'Population')

# now separate the population from participant
# SiteID with 2 or 3 digit are participants

hydeParkPop_summerUse_part <- hydeParkPop_summerUse %>%
  filter(SiteID < 1000) %>%
  mutate(Group = 'Participant')

# now bind two dataset for plotting purposes

df <- rbind(hydeParkPop_summerUse_part, hydeParkPop_summerUse)

#Return the unique groups
cGroups <- unique(df$Group)

#Count the number of households in each group
dfGroupSize <- df %>% group_by(Group) %>% summarize(n = n())
dfGroupSize$NumAsText <- paste0(dfGroupSize$Group, " (n = ", as.character(dfGroupSize$n), ")")

#Add the NumAsText as new variable in Combined data frame
dfCombined <- left_join(x = df, dfGroupSize, by = c("Group" = "Group"))



## 1. Cummulative distribution
ggplot(dfCombined, aes(x=SummerUsage, group = NumAsText, color = NumAsText)) +
  stat_ecdf(size = 2) +
  #xlim(0, 20) +
  scale_y_continuous(breaks = seq(0,1,by=0.25),labels=seq(0,100,by=25)) +
  theme_bw() +
  #coord_fixed() +
  labs(x="Summer Water Usage\n(1,000 gallons)", y="Ranked Households\n(Percent of group size)") + 
  theme(text = element_text(size=24), legend.title=element_blank(), legend.text=element_text(size=24),
        legend.position = c(0.8, 0.8)) -> CumPlot

CumPlot

## 4. QQ-plot
ggplot(dfCombined) +
  stat_qq(aes(sample=SummerUsage, group = NumAsText, color = NumAsText, shape = NumAsText), size =2) +
  #stat_qq(aes(sample = (dfPart$SummerUsage)), color = "red", shape = 16, size = 2) +
  #stat_qq(aes(sample = (dfPop$SummerUsage)), color = "blue", shape = 15, size = 2) +
  #geom_histogram(binwidth=2) +
  scale_color_manual(name = "Hyde Park", values = c("red","blue"), breaks = dfGroupSize$NumAsText) +
  scale_shape_manual(name = "Hyde Park", values = c(16,15), breaks = dfGroupSize$NumAsText) +
  #scale_y_continuous(breaks = seq(0,50,by=10),labels=seq(0,50,by=10)) +
  #scale_x_date(limits= c(as.Date("1968-01-01"), as.Date("2030-01-01"))) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  scale_x_continuous(breaks=seq(-3,3,by=1), labels= c("3\nBelow", "2\nBelow", "1\nBelow", "0", "1\nAbove", "2\nAbove", "3\nAbove")) +
  #xlim(-3,2.2) +
  # ylim(0, 50) +
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  #guides(shape = "none") + 
  labs(x = "Deviation from central value", y="Summer Water Use\n(1,000 gallons)", color = "Hyde Park") +
  theme(text = element_text(size=24), legend.text=element_text(size=24),
        legend.position = c(0.2, 0.8)) -> qqPlot

qqPlot

ks.test(x = hydeParkPop_summerUse_part$SummerUsage, 
        y = hydeParkPop_summerUse$SummerUsage, alternative = "two.sided" )

Dcrit <- 1.36 * sqrt((length(hydeParkPop_summerUse_part$SiteID) + length(hydeParkPop_summerUse$SiteID))/((length(hydeParkPop_summerUse_part$SiteID) * length(hydeParkPop_summerUse$SiteID))))

############
# Plot
plot(qqPlot)
plot(CumPlot)