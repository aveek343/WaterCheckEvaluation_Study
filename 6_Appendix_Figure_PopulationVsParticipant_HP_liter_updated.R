# Author: Mahmud Aveek, David Rosenberg
# Date: 10-02-2025
# Runtime: 30 seconds

# This is an R document. This document compares summer water use in 2022 for the entire
# population of Hyde Park single family residents (n = 1726) to the opt-in sample of households for the Utah
# State University Extension Flume/Water Check Study (n = 40). The purpose is to identify whether there
# are statistical differences between the study sample of households and the city population.
# The comparison is presented multiple ways with different data visualizations and one Kolmogorov–Smirnov
# statistical test (KS-Test).
# The plots are:
#   1. Cumulative distributions of participants and population
#   2. Quantile-Quantile plot

cPackages <- c("versions","dygraphs","tidyquant","xts","tidyquant","lubridate",
               "stringr","RColorBrewer","dplyr","ggplot2")
installed_packages <- cPackages %in% rownames(installed.packages())
if (any(!installed_packages)) install.packages(cPackages[!installed_packages])
invisible(lapply(cPackages, library, character.only = TRUE))

# Read & convert to liters
hydeParkWaterUse_liter <- read.csv("md_withSiteID_anonymous_summer_HP.csv") %>%
  mutate(
    Usage_L = Usage * 3.78541,                 # gallons -> liters
    date = as.Date(date),                      # convert to proper Date
    month = lubridate::month(date),            # extract month number
    year = lubridate::year(date)               # extract year
  ) %>%
  filter(year == 2022, month >= 6, month <= 9) 

# Summer usage per SiteID (liters)
hydeParkPop_summerUse_liter <- hydeParkWaterUse_liter %>%
  group_by(SiteID) %>%
  summarize(SummerUsage = sum(Usage_L), .groups = "drop") %>%
  mutate(Group = "Population")

# Participants (SiteID with 2–3 digits)
hydeParkPop_summerUse_part_liter <- hydeParkPop_summerUse_liter %>%
  filter(SiteID < 1000) %>%
  mutate(Group = "Participant")

# Combined df for plotting
df_liter <- rbind(hydeParkPop_summerUse_part_liter, hydeParkPop_summerUse_liter)

# Group sizes
dfGroupSize_liter <- df_liter %>%
  group_by(Group) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(NumAsText = paste0(Group, " (n = ", n, ")"))

# Add display group text
dfCombined_liter <- left_join(df_liter, dfGroupSize_liter, by = "Group")

## 1) Cumulative distribution (liters)
CumPlot_liter <- ggplot(dfCombined_liter,
                        aes(x = SummerUsage, group = NumAsText, color = NumAsText)) +
  stat_ecdf(size = 2) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = seq(0, 100, by = 25)) +
  theme_classic() +
  labs(x = "Summer water usage \n(1000 liters)", y = "Ranked households\n(percent of group size)") +
  theme(text = element_text(size = 24),
        legend.title = element_blank(),
        legend.text  = element_text(size = 24),
        legend.position = c(0.8, 0.8))

CumPlot_liter

## 2) QQ-plot (liters)
qqPlot_liter <- ggplot(dfCombined_liter) +
  stat_qq(aes(sample = SummerUsage, group = NumAsText, color = NumAsText, shape = NumAsText),
          size = 2) +
  scale_color_manual(name = "Hyde Park", values = c("red","blue"),
                     breaks = dfGroupSize_liter$NumAsText) +
  scale_shape_manual(name = "Hyde Park", values = c(16,15),
                     breaks = dfGroupSize_liter$NumAsText) +
  scale_x_continuous(breaks = seq(-3, 3, by = 1),
                     labels = c("3\nBelow","2\nBelow","1\nBelow","0","1\nAbove","2\nAbove","3\nAbove")) +
  theme_classic() +
  labs(x = "Deviation from central value", y = "Summer water use (1000 liters)", color = "Hyde Park") +
  theme(text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.position = c(0.2, 0.8))

qqPlot_liter

## KS test (liters)
ks.test(x = hydeParkPop_summerUse_part_liter$SummerUsage,
        y = hydeParkPop_summerUse_liter$SummerUsage,
        alternative = "two.sided")

Dcrit_liter <- 1.36 * sqrt(
  (length(hydeParkPop_summerUse_part_liter$SiteID) + length(hydeParkPop_summerUse_liter$SiteID)) /
    (length(hydeParkPop_summerUse_part_liter$SiteID) * length(hydeParkPop_summerUse_liter$SiteID))
)

# Plot
plot(qqPlot_liter)
plot(CumPlot_liter)

# Build empirical CDFs
ecdf_part <- ecdf(hydeParkPop_summerUse_part_liter$SummerUsage)
ecdf_pop  <- ecdf(hydeParkPop_summerUse_liter$SummerUsage)

# Use the combined sorted values as the evaluation grid
all_vals <- sort(unique(c(hydeParkPop_summerUse_part_liter$SummerUsage,
                          hydeParkPop_summerUse_liter$SummerUsage)))

diffs <- abs(ecdf_part(all_vals) - ecdf_pop(all_vals))
imax  <- which.max(diffs)

max_diff      <- diffs[imax]              # same as KS D
val_at_max    <- all_vals[imax]           # usage value where it occurs
partile_part  <- ecdf_part(val_at_max)    # participant percentile there
partile_pop   <- ecdf_pop(val_at_max)     # population percentile there

# Additions: print full KS summary using existing variables -----
cat(sprintf("\n--- KS Summary (Hyde Park, liters) ---\n"))
cat(sprintf("D observed = %.4f (from ECDF), D critical (alpha=0.05) = %.4f\n", max_diff, Dcrit_liter))
cat(sprintf("Max gap at %.0f liters: Participant CDF = %.1f%%, Population CDF = %.1f%%\n",
            val_at_max, 100*partile_part, 100*partile_pop))

# Additions: annotate ECDF at the max-gap (no new vars; inline data.frame)
## 1) Cumulative distribution (liters) with D, Dcrit, p-value, alt, and percentile labels
ks_tmp <- ks.test(
  x = hydeParkPop_summerUse_part_liter$SummerUsage,
  y = hydeParkPop_summerUse_liter$SummerUsage,
  alternative = "two.sided",
  exact = FALSE
)

CumPlot_liter <- ggplot(dfCombined_liter,
                        aes(x = SummerUsage, group = NumAsText, color = NumAsText)) +
  stat_ecdf(size = 1.8) +
  # vertical D segment at max gap
  geom_segment(
    data = data.frame(x = val_at_max,
                      y_min = min(partile_part, partile_pop),
                      y_max = max(partile_part, partile_pop)),
    aes(x = x, xend = x, y = y_min, yend = y_max),
    inherit.aes = FALSE, linewidth = 1,linetype = "dotted"
  ) +
  # dots at the two CDF percentiles
  geom_point(
    data = data.frame(
      x = c(val_at_max, val_at_max),
      y = c(partile_pop, partile_part),
      NumAsText = dfGroupSize_liter$NumAsText
    ),
    aes(x = x, y = y, color = NumAsText),
    inherit.aes = FALSE, size = 3
  ) +
  # percentile labels next to dots
  geom_text(
    data = data.frame(
      x = c(val_at_max, val_at_max),
      y = c(partile_pop, partile_part),
      lab = c(sprintf("Population: %.1f%%", 100 * partile_pop),
              sprintf("Participant: %.1f%%", 100 * partile_part))
    ),
    aes(x = x, y = y, label = lab),
    inherit.aes = FALSE,
    nudge_x = diff(range(dfCombined_liter$SummerUsage, na.rm = TRUE)) * 0.03,
    vjust = -0.051,
    hjust = -0.051,
    size = 5
  ) +
  # "D-stat" label near the segment (like your mock)
  annotate(
    "text",
    x = val_at_max * 1.05,
    y = min(partile_part, partile_pop) + (max(partile_part, partile_pop) - min(partile_part, partile_pop)) * 0.35,
    label = sprintf("D-stat: %.2f", max_diff),
    size = 6,
    colour = "blue4",
    hjust = -0.2
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = seq(0, 100, by = 25)) +
  theme_classic() +
  labs(
    title = paste0(
      "City: Hyde Park\n",
      "K-S Test for sample vs population (summer use)\n",
      sprintf("D-stat: %.2f, D-Crit: %.2f", max_diff, Dcrit_liter)
    ),
    subtitle = paste0(
      "p-value: ", formatC(ks_tmp$p.value, format = "e", digits = 2),
      ", Alt. hypothesis: two-sided"
    ),
    x = "Summer water usage (liters)",
    y = "ECDF"
  ) +
  theme(text = element_text(size = 22),
        plot.title = element_text(size = 20, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 18, hjust = 0),
        legend.title = element_blank(),
        legend.text  = element_text(size = 22),
        legend.position = c(0.8, 0.2))

print(CumPlot_liter)


# The following results are in x1000 Liter
cat(sprintf("Max diff = %.3f at %.0f liters\n", max_diff, val_at_max))
cat(sprintf("Participant percentile = %.1f%%, Population percentile = %.1f%%\n",
            100*partile_part, 100*partile_pop))

