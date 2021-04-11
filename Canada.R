#datafest 2021
#AHHHHHHHHHHHHHHHHHHHH

library(tidyverse)
library(ggplot2)
library(plyr)
library(maps)
library(ggfortify)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)

Canada <- read_csv("CA/ca.csv")
View(Canada)

Canada_Young <- filter(Canada, DEM_AGE < 35)
View(Canada_Young)

#grouping dates into months
Canada <- Canada %>%
  mutate(month = format(DATE, "%m")) %>%
  group_by(month)

#Canada <- na.omit(Canada)

View(Canada)

Canada_plot <- ggplot(Canada)

#getting ages
ggplot(Canada_Young, aes(x = DEM_AGE)) + geom_histogram(binwidth = 1)
ggplot(Canada, aes(x = DEM_AGE)) + geom_histogram(binwidth = 1)

test <- cut(Canada$OXYM_USE, 2, labels = c("No","Yes"))
test
count(test)

#count of regions
count_of_regions <- ggplot(Canada, aes(x = cut(Canada$OXYM_USE, 2, labels = c("No","Yes")))) + geom_bar(fill = "pink" , color = "black") + coord_flip() + facet_wrap(~DEM_LOCATION) +
  labs(title = "OXYM Use in Regions of Canada", x = "Count of Respones", y = "Survey Response") + 
  theme_light()
count_of_regions

#bar plots of drug use
cod_use <- ggplot(Canada, aes(x = cut(Canada$COD_USE, 2, labels = c("No","Yes")))) + geom_bar(fill = "red2", color = "black")+ 
  theme_light() + ylim(c(0,8000)) +
  labs(x = "Codeine Use Ever", y = "Number of Responses")
cod_nmu <- ggplot(Canada, aes(x = cut(Canada$COD_NMU, 2, labels = c("No","Yes")))) + geom_bar(fill= "gold", color = "black") + 
  theme_light() + ylim(c(0,8000)) +
  labs(x = "Codeine Use Non-Prescription", y = "Number of Responses")
cotc_nmu <- ggplot(Canada, aes(x = cut(Canada$COTC_NMU, 2, labels = c("No","Yes")))) + geom_bar(fill = "dodgerblue", color = "black") + 
  theme_light() + ylim(c(0,8000)) +
  labs(x = "Codeine Use Illegally", y = "Number of Responses")

title <- ggdraw() + draw_label("Number of Respones to Codeine Questions", fontface = "bold")
graphs <- plot_grid(cod_use, cod_nmu, cotc_nmu, labels = "AUTO", nrow = 1) 
codeine_use <- plot_grid(title, graphs, ncol = 1, rel_heights = c(0.1, 1))
codeine_use

#hex tile
ggplot(Canada, aes(x = DEM_AGE, y = QTIME, color = month)) + geom_jitter(na.rm = TRUE) + ylim(c(0, 50000)) +
  labs(x = "Demographic Age (years)", y = "Time taken for survey (seconds)", title = "Demograhic Age versus Time Taken") 
ggplot(Canada, aes(x = DEM_AGE, y = QTIME)) + geom_jitter(na.rm = TRUE) + ylim(c(0, 50000)) + facet_wrap(~month) +
  labs(x = "Demographic Age (years)", y = "Time taken for survey (seconds)", title = "Demograhic Age versus Time Taken") 
ggplot(Canada, aes(x = DEM_AGE, y = QTIME, color = month)) + geom_jitter(na.rm = TRUE) + ylim(c(0, 5000)) +
  labs(x = "Demographic Age (years)", y = "Time taken for survey (seconds)", title = "Demograhic Age versus Time Taken") 
ggplot(Canada, aes(x = DEM_AGE, y = QTIME)) + geom_jitter(na.rm = TRUE) + ylim(c(0, 5000)) + facet_wrap(~month) +
  labs(x = "Demographic Age (years)", y = "Time taken for survey (seconds)", title = "Demograhic Age versus Time Taken") 
