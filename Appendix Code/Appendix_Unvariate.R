# Appendix Univariate Stats and Plots
# Carla Leone

## Load packages and data ----
library(tidyverse)
library(readxl)

merged <- read_excel("data/merged.xlsx", 
                     sheet = "merged3")
str(merged)

## Models and Diagnostic Plots ----

hist(sqrt(full_diversity$diversity)) #best transformation
lm_div<- aov(sqrt(diversity)~habitat, data= full_diversity)
plot(lm_div) # outliers in residuals vs leverage

# kruskal wallis because assumptions are not met
kw_full_diversity<- kruskal.test(diversity~habitat, data=full_diversity)
kw_full_diversity

## Plot with GGPLOT ---- 
# Make subsets of the data required
full_diversity <- merged %>%
  group_by(site,habitat) %>%
  summarise(diversity = mean(full_simpson)) %>%
  ungroup()

# summarize data with means and standard error
summary_full_diversity<- full_diversity%>%
  group_by(habitat) %>%
  summarise(mean_value = mean(diversity),
            sd = sd(diversity),
            se = sd/sqrt(n()) ) %>%
  ungroup()
print(summary_full_diversity)

# GGPLOT
(full_diversity_plot<- ggplot(summary_full_diversity, aes(x = habitat, y = mean_value)) +
    geom_bar(stat = "identity", position= 'dodge', fill = Colours, alpha = 0.6) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2, color = "black", position = position_dodge(width = 0)) +
    geom_point(data = full_diversity, aes(y =diversity), color = "red", size = 3, position = position_dodge(width = 0)) + # add raw data
    #geom_text(aes(label = sprintf("%.2f", mean_value)), vjust = -0.5, color = "black", size = 3, position = position_dodge(width = 0.5)) +
    labs(y = "Mean Diversity (Simpson's Diversity Index (D))", x = "Habitat Complexity Category") +
    theme_classic() +
    scale_fill_manual(values = Colours, name = "Habitat Category")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(size = 1.5),  # increase axis line thickness
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))  # increase axis title size) 
)
