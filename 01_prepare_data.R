library(tidyverse)
library(patchwork)
library(corrr) # For  rplot
# Create a data folder 
dir.create("data")

#Paste and paste0 take different string and create 1 string from the fragmets of other
# paste - sep is space
# paste0, no separator

#Download the files using a for loop
for(i in c ("counts_raw.csv", "counts_transformed.csv", 
            "sample_info.csv", "test_result.csv")){
  download.file(
    url = paste0("http://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=TRUE"),
  destfile = paste0("data/",i)
  )
}


raw_cts <- read.csv("data/counts_raw.csv")
trans_cts <- read.csv("data/counts_transformed.csv")
sample_info <- read.csv("data/sample_info.csv")
test_result <- read.csv("data/test_result.csv")

head(raw_cts)
str(raw_cts)
str(trans_cts)
str(sample_info)
str(test_result)
unique(sample_info$replicate)
unique(sample_info$minute)


# Gene expression counts in yeast at different time points with osmotic pressure

plot(raw_cts$wt_0_r1)

??pivot_longer
# Currently raw_cts is in wide format
# For every gene now we will have 36 values, the value from the cols variable will 
# be saved against each gene. Take the values from the mentioned 'cols' and save 
# the column name to the "sample" and the values of those columns would be in the 
# "count" for each gene. If we had other columns , those columns would stay the same
# only those in names_to and values_to 

raw_cts_longer <- raw_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3,
               names_to = "sample", # which column to put the names to
               values_to = "count") %>% # which column to put the values to
  filter(!is.na(count))

# Join the long table with the sample information
raw_cts_longer_full <- full_join(raw_cts_longer, sample_info, by = ("sample"))# If 
# no 'by' given, dplyr will take the common column
unique(raw_cts_longer_full$minute)

raw_cts_longer_full %>% 
  ggplot(aes(replicate,
             count,
             color = replicate, # for filling the plot color 
             fill = replicate)) + # for filling the legend 
  geom_boxplot()

raw_cts_longer_full %>% 
  ggplot(aes(replicate,
             count,
             color = replicate, # for filling the plot color 
             fill = replicate)) + # for filling the legend 
  geom_boxplot() +
  facet_grid(rows = vars(strain),
             cols = vars(minute))

# Normalised counts 
raw_cts_longer_full %>% 
  ggplot(aes(replicate,
             log10(count+ 1) ,
             #color = replicate, # for filling the plot color 
             fill = replicate)) + # for filling the legend 
  geom_boxplot() +
  facet_grid(rows = vars(strain),
             cols = vars(minute))


raw_cts_longer_full %>% 
  ggplot(aes(log10(count+ 1 ),
             #color = replicate, # for filling the plot color 
             colour = replicate)) + # for filling the legend 
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain),
             cols = vars(minute))

summary(raw_counts_longer_full$count)
################################################################################
# Repeat the pivot_longer and inner_join on the transformed counts 
trans_cts_longer <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3,
               names_to = "sample", # which column to put the names to
               values_to = "count") %>% # which column to put the values to
  filter(!is.na(count))

trans_cts_longer_full <- full_join(trans_cts_longer, sample_info, by = ("sample"))

trans_cts_longer_full %>% 
  ggplot(aes(replicate,
             count,
             #colour = replicate,
             fill = replicate)) +
  geom_boxplot() + 
  facet_grid( rows = vars(strain),
              cols = vars(minute))

trans_cts_longer_full %>% 
  ggplot(aes(replicate,
             log10(count + 1),
             colour = replicate)) +
  geom_boxplot() + 
  facet_grid( rows = vars(strain),
              cols = vars(minute))

# Compare the raw counts between 2 points
raw_cts %>% 
  ggplot(aes(wt_0_r1, 
             wt_15_r1)) +
  geom_point() +
  geom_abline(colour = "green")

# Compare the transformed counts between 2 points  
trans_cts %>% 
  ggplot(aes(wt_0_r1, 
             wt_15_r1)) +
  geom_point() +
  geom_abline(colour = "red") # The points on the line do not change, whereas below 
# 0 is higher, and if above the line time 15 has higher expression, also the pressure increased 
# the expression as seen from the points above the line compared to those below

# Patchwork do side by side 
(raw_cts %>% 
  ggplot(aes(wt_0_r1, 
             wt_15_r1)) +
  geom_point()
)  +

(trans_cts %>% 
  ggplot(aes(wt_0_r1, 
             wt_15_r1)) +
  geom_point())

# Compare the raw counts between 2 points again on the log transformed 
# 1. plots are same, but 1 is on actual number scale, 2 is on log2 scale, points same
raw_cts %>% 
  ggplot(aes(wt_0_r1 + 1, 
             wt_15_r1 + 1)) +
  geom_point() +
  scale_x_continuous(trans = "log2") + # convert to log2 for continuous variables
  scale_y_continuous(trans = "log2") +
  geom_abline(colour = "brown")

raw_cts %>% 
  ggplot(aes(log2(wt_0_r1 + 1), 
             log2(wt_15_r1 + 1))) +
  geom_point() +
  #scale_x_continuous(trans = "log2") + # convert to log2 for continuous variables
  #scale_y_continuous(trans = "log2") +
  geom_abline(colour = "green")

b <- raw_cts %>% 
  ggplot(aes(wt_0_r1 + 1, 
             wt_15_r1 + 1)) +
  geom_point() +
  scale_x_continuous(trans = "log2") + # convert to log2 for continuous variables
  scale_y_continuous(trans = "log2") +
  geom_abline(colour = "brown")

c <- raw_cts %>% 
  ggplot(aes(log2(wt_0_r1 + 1), 
             log2(wt_15_r1 + 1))) +
  geom_point() +
  #scale_x_continuous(trans = "log2") + # convert to log2 for continuous variables
  #scale_y_continuous(trans = "log2") +
  geom_abline(colour = "green")
b + c 

############################################################
raw_counts_longer_full_grouped <-raw_counts_longer_full %>% 
  group_by(gene) %>%
  summarize( mean_count = mean(count),
             var_count = var(count))

# What does this plot mean? as the expression goes up the more variance. This is a 
# skewed data, lowly expressed genes have lower variability/flexibility and the highly
# expressed genes, expression changes drastically. In biology, the more a gene is expressed 
# the higher variability is expected 
p1 <- raw_counts_longer_full_grouped %>% 
  ggplot(aes(mean_count,
             var_count)) + # color will create legend 
  geom_point() +
  geom_abline( color = "red") +
  scale_x_continuous(trans = "log2") + 
  scale_y_continuous(trans = "log2") +
  labs(title="Raw read counts")

trans_cts_longer_full_grouped <-trans_cts_longer_full %>% 
  group_by(gene) %>%
  summarize( mean_count = mean(count),
             var_count = var(count))

# Higher expression doesn't mean higher variance here -corrected for Poisson
p2 <- trans_cts_longer_full_grouped %>% 
  ggplot(aes(mean_count,
             var_count,
             color = "blue")) + # color will create legend 
  geom_point() 

p1+p2
# Transform the scaled counts again 
trans_cts_longer_full_grouped %>% 
  ggplot(aes(mean_count,
             var_count,
             color = "blue")) + # color will create legend 
  geom_point() +
  scale_x_continuous(trans = "log2") + 
  scale_y_continuous(trans = "log2") 


# Calculate the correlation between the samples 
trans_cts_cor <- trans_cts %>% 
  select(-gene) %>% #remove gene column
  cor(method = "spearman")# Spearman correlation to see similarity between different conditions
trans_cts_cor

rplot(trans_cts_cor, shape =1) + 
  theme(axis.text = element_text(angle = 45),
        hjust =0.5,
        vjust =0.5 )
# wt in high time points are shwing high similar with the mutatnt in the smae time points, because 
#  we made pressire to both for a high time , so at one stage they started die / or the muta had 
#  enough time to deal with the stress 

as.dendrogram(hclust(as.dist(1-trans_cts_cor))) %>% 
  plot()






