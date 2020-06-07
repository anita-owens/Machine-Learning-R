############# Marketing A/B Test  ######

#Dataset from IBM Watson Community

#####load libraries ######

library(multcomp)
library(tidyverse)
library(cowplot)

###### Data Import #############

df<-read.csv("https://raw.githubusercontent.com/pthiagu2/DataMining/master/WA_Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv")

head(df)

#check for missing data using VIM package
aggr(df, prop = F, numbers = T) # no red - no missing values


#summary sales statistics
grouped.df <- df %>% 
  group_by(Promotion) %>% 
  summarize(
    count = n(),
    totalSales = sum(SalesInThousands),
    meanSales = mean(SalesInThousands),
    sd = sd(SalesInThousands))

#see stats
grouped.df 
#We can see that group 3 created the most sales
#followed by groups 1 & 2

# Box plots
# ++++++++++++++++++++
# Visualize means of sales
library("ggpubr")
ggboxplot(df, x = "Promotion", y = "SalesInThousands", 
          color = "Promotion", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Sales", xlab = "Promotion")

#We see that promotion one has the most average sales
#followed by promotion 3 just like in our summary statistics table


############Data Visualization #############

#Let's check and explore the stores
viz1 <- ggplot(df, aes(x=MarketSize))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()

#market size by promotion data frame
market_df <- df %>% 
  group_by(Promotion) %>% 
  count(MarketSize) %>% 
  mutate(percent = n/sum(n))
market_df #check results

viz2 <- ggplot(data=market_df, aes(x=MarketSize, y=percent, fill = factor(Promotion))) +
  geom_bar(stat = "identity", width=0.7)

viz3 <- ggplot(data = df, aes(x = factor(Promotion), y = LocationID, fill = Promotion))+geom_boxplot()

viz4 <- ggplot(data = df, aes(x = factor(Promotion), y = AgeOfStore, fill = Promotion))+geom_boxplot()

viz5 <- ggplot(data=df, aes(x=factor(AgeOfStore)))+
  geom_bar(stat="count", width=0.7, fill="green")+
  theme_minimal()


#there is a bit of a delay before plots appear
plot_grid(viz1, viz2, viz3, viz4, viz5, labels = "AUTO")

######### Data transformation ###########

#check the promotion variable
str(df$Promotion) #an integer object, we need to change this

#factor the promotion variable before we model it
df$Promotion <- as.factor(df$Promotion)

#check results
str(df$Promotion)


###### Data Question ###############

#Does store sales differ by promotion?
#Compute the mean sales using aggregate function
aggregate(SalesInThousands ~ Promotion, df, mean)
#promotion 1 has the highest level of sales, but it is statistically significant?



######## Significance Testing - ANOVA #############

#promotion 1 has the highest mean of sales, but is it statistically significant?
#We perform a one-way ANOVA
df.anova <- aov(SalesInThousands ~  Promotion, data = df)
summary(df.anova)

#We plot the ANOVA model to visualize confidence
#intervals for mean sales satisfaction by promotion

# 2. Normality plot - check if distribution is normal
plot(df.anova, 2)

# Check normality assumption by analyzing the model residuals
# Create a QQ plot of residuals
qqplot(residuals(df.anova))

#We see that the sales differs by Promotion,
# the model is statiscally significant
#but we don't know
#which pair groups are significant


#How can we change this??

###### Method 1 Multiple comparisons using multcomp package #############

#Use glht() to perform multiple pairwise-comparisons for
# a one-way ANOVA: (with confidence interval and p-values)
summary(glht(df.anova, linfct = mcp(Promotion = "Tukey")))
#group 2 is significant against group 1
#group 3 is significant against group 2

#TukeyHSD(aov(df.anova), "Promotion") #does same as glht function but includes the
      #confidence intervals

# plot difference in mean levels of promotion
plot(TukeyHSD(df.anova))

#Tukey comparison of means - much better and has confidence intervals
a1 <- aov(formula = df$SalesInThousands ~ df$Promotion)
plot(a1) # plots to check normality


#Post hoc testing
posthoc <- TukeyHSD(x=a1, conf.level = 0.95)
posthoc


#plot means
library(gplots)

plotmeans(SalesInThousands ~ Promotion, data = df,
  frame = FALSE, connect = TRUE, mean.labels = TRUE,
  digits = 2, col=" dark red",
  barwidth=2, pch = " ",
          main = "Groups 1 & 2 and 3 & 2 are significant")


###### Method 2 Change ANOVA equation to remove intercept term #############

df.anova2 <- aov(SalesInThousands ~  -1 + Promotion, data = df)
glht(df.anova2)

#With intercept removed, glht gives us the mean value for each segment
# plot difference in mean levels of promotion
plot(glht(df.anova2), xlab="Average sales by promotion (95% CI)")
#The dot shows the mean for each segment and bars reflect the confidence interval.

#With all 3 plotted with confidence intervals, Promo 2 is significantly worse
#than Promo 1 and 3, but we cannot say that Promo 1 and 3 are significant as their
#confidence intervals overlap.

######### Summary: What should you tell the marketing & sales team? ############

#Let's run again with just promotion 1 & 3 to
#see if we can get a significant result

#It appeared in group 1 there were some stores that were slightly
#younger than those in Group 3
#it may not have made a difference but we should try to control for this.
