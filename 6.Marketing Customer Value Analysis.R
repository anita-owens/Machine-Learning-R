############# Customer Analytics  ######

#Dataset from IBM Watson Community

#####load libraries ######

library(multcomp)
library(tidyverse)
library(cowplot)

###### Data Import #############

df<-read.csv("https://raw.githubusercontent.com/pthiagu2/DataMining/master/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")

View(df)

#Encode engaged customers as 0s and 1s
df$engaged <- as.integer(df$Response) - 1
