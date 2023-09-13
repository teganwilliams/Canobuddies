# Script for analysing understory plant diversity for Field Science project
# September 2023

library(readxl)
library(writexl)
library(abdiv)
library(vegan)
library(ggplot2)
library(tidyverse)
library(lsr)
library(car)

# PROJECT: How do light availability, soil pH, and soil texture influence understory plant composition?
  # THIS SRIPT focuses on the calculation of diversity indices and assesses the distribution of our data

# 1. Plotting data distribution ----------------------------------------------

  # Import the dataset including the species names and their occurrence in each plot

# Visualising the distribution of species occurrence across all plots

# Count number of times species occurred in plots (1 or 0 per plot):
species_sum <- colSums(trees[,2:25] != 0)   
species_sum <- data.frame(species_sum) %>% 
  rownames_to_column("species") %>% 
  rename(occurrence = species_sum) %>% 
  arrange(occurrence,decreasing=TRUE)

# Create vector with abbreviated species names for axis tick labels
barplot_labels <- c("B. spicant","C. bicuspidata","Liverwort 3","Liverwort 4",
                    "D. flexuosa","G. repens","Moss 13","Moss 14","R. triquetrus",
                    "Moss 12","B. chrysocoma","C. vulgaris","V. vitis idaea",
                    "Liverwort 1","Hypnum sp.", "S. palustre","P. remmuone",
                    "R. loreus","T. tamariscinum","V. myrtillis","M. caerulea",
                    "L. bidentata","P. schreberi","P. undulatum")

(species_barplot <- ggplot(data=species_sum, aes(x=reorder(species, occurrence), y=occurrence), fill=species) +
    geom_bar(stat="identity") +
    scale_fill_gradient(low="blue",high="skyblue") +
    coord_flip() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y=element_blank()
    ) +
    labs(y="Occurrence") +
    scale_x_discrete(labels=(barplot_labels)) 
)

# 2. Diversity indices ------------------------------------------------------

# Hill numbers, such as the exponential Shannon's diversity index and the inverse Simpson's index, make diversity indices comparable across different studies
  # with different levels of replication and different community sizes

# Exponential Shannon's diversity index
trees$shannon <- diversity(trees[2:25], index="shannon")  
  # standard Shannon's index
trees$exp_shannon <- exp(trees$shannon) 
  # exponential Shannon's index

# Simpson's inverse of D index:
trees$inv_simpson <- diversity(trees[2:25], index="invsimpson")

# I will continue to include Simpson's index to show the difference in level of detection between the indices

# 3. Boxplots on diversity between forest types ------------------------------

# Create new column with forest types
trees$wood_type <- c(rep("Mixed", 15), rep("Pine",15))

# Boxplot for Shannon's diversity
(diversity_shannon_boxplot <- ggplot(trees, aes(wood_type, exp_shannon)) +
    # geom_jitter(position=position_jitter(0), show.legend = FALSE) +
    stat_boxplot(show.legend=FALSE, fill=c("#cd96e6", "#4ecb50")) +
    ylab("Exponent of Shannon-Wiener's index (H')") +
    xlab("Forest type") +
    theme_bw() +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
)

ggsave("diversity_shannon_boxplot.png", width=9, height=6, dpi=300)

# Boxplot for Simpson's diversity
(diversity_simpson_boxplot <- ggplot(trees, aes(wood_type, inv_simpson, fill=wood_type)) +
    stat_boxplot(show.legend=FALSE, fill=c("#4ecb50","#cd96e6")) +
    # geom_jitter(position=position_jitter(0), show.legend = FALSE) +
    ylab("Simpson’s reciprocal index (D)") +
    xlab("Forest type") +
    theme_bw() +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
)

ggsave("diversity_simpson_boxplot.png", width=9, height=6, dpi=300)

# 4. Assessing the power of our study -----------------------------------------------

# 4a) T-test for diversity indices vs. forest type 
# Comparing the mean diversity between forest types -> is there a significant difference in diversity?

t_test_shannon <- t.test(exp_shannon~wood_type, data=trees)
  # p-value: 0.046
  # mean diversity in pine: 4.64
  # mean diversity in mixed: 3.69
  # rare species drive diversity difference between forest types, makes forests more equal
t_test_simpson <- t.test(inv_simpson~wood_type, data=trees)
  # p-value: 0.11
  # mean diversity in pine: 3.98
  # mean diversity in mixed: 3.32
  # Simpson's index discounts rare species at higher rate

# ANOVA:
# good for means for more than two groups
# t-test better for only two groups

# 4b) Effect size
# T-test: difference between means of groups (forest types)
# e.g., difference between means is 0.5 with significant p-value
# But is that a small or large difference? -> effect size will tell!
# Effect size: is that difference between means of groups large or small?
# Difference in diversity index means means is based on number of samples in each group
# Difference in diversity index means is divided by standard deviation -> makes it comparable 

# Cohen's d: measures size of difference in means compared to their standard deviation between the 2 forest types
cohensD(trees$exp_shannon[1:15], trees$exp_shannon[16:30])
  # Cohen's d = 0.76
  # less than 0.3: small
  # between 0.3 and 0.5: medium
  # more than 0.8: large 
  # 0.76 is basically a large effect size

cohensD(trees$inv_simpson[1:15], trees$inv_simpson[16:30])
  # 0.6
  # rather large given that no significant difference in t test
  # DO NOT TRUST


# 4c) Power analysis
# The power analysis assess how much power our study has based on the number of samples we did
power.t.test(n=15, sig.level = 0.05, alternative = "two.sided", sd = 1, delta = 0.76, type = "one.sample")
  # power = 0.78
  # 78% probability of detecting a true effect
  # type 2 error: don't find something when there is actually is something
  # low chance of rejecting a true effect 

# Is the variance between the groups equal?
leveneTest(exp_shannon~wood_type, data=trees)
  # p-value = 0.6 -> equal variance

# Is the diversity index data normally distributed?
# Visually:
hist(trees$exp_shannon)
hist(trees$inv_simpson)

# Statistically:
shapiro.test(trees$exp_shannon)
shapiro.test(trees$inv_simpson)
  # => our data is normally distributed


