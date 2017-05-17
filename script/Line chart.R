#---
# May 16, 2017
# Final Project -- Data Viz
#---

library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Read data
nels88 <- read_csv("data/nels88.csv")

# Define what variables are going to show on X-axis
Ori <- "Ori_perc" # 1987
BY <- "BY_perc_c" # 1988
F1 <- "F1_perc_c" # 1990
F2 <- "F2_perc_c" # 1992
F3 <- "F3_com_perc" # 1994
F4 <- "F4_com_perc" # 2000

# Define the categorical variable to be shown by the color channel
C <- "BY_momedu"

# Prepare the graph tibble
gTibble <- nels88[, c("ID", C, Ori, BY, F1, F2, F3, F4)] %>% 
  sample_frac(size = 0.01, replace = FALSE) %>% 
  gather(-ID, -get(C), key = "Year", value = "Percentile")

gTibble$Year <- factor(gTibble$Year,
                       levels = c("Ori_perc", "BY_perc_c", "F1_perc_c", "F2_perc_c", "F3_com_perc", "F4_com_perc"))

# Line graph -- One line for each individual
ggplot(gTibble) +
  geom_line(aes(x = Year, y = Percentile, group = ID, color = get(C)))

# Line graph -- Draw one "average" line for each group