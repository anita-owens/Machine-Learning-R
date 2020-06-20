############# Customer Analytics  ######

#Dataset from IBM Watson Community

#####load libraries ######

library(multcomp)
library(tidyverse)
library(cowplot)

###### Data Import #############

df<-read.csv("https://raw.githubusercontent.com/pthiagu2/DataMining/master/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv")

View(df)

#Check response variable
table(df$Response)

#Encode engaged customers as 0s and 1s
df$engaged <- as.integer(df$Response) - 1

########### Check Data #################

#summarize variables in dataframe
summary(df)

#let's look at the post type
table(df$Type)

#let's look at the category
table(df$Category)

#let's make a cross-table of type and category
table(df$Category, df$Type)

#check column names - some of them are quite long
names(df)


#we need to rename some of the columns to make the names shorter
df <- rename(df,
                        CLV = Customer.Lifetime.Value,
                        eff.date = Effective.To.Date,
                        empl.status = EmploymentStatus,
                        location = Location.Code,
                        mar.status = Marital.Status,
                        month.premium = Monthly.Premium.Auto
            )
 
#check results
names(df)

########## Any Missing Data #############

## Show cases with missing values
df[!complete.cases(df),] #no missing

#using aggr function from VIM packages --missing values are in red
aggr(df, prop = F, numbers = T)
#nothing missing

###### Explore Data: Data visualization ##############



##### Univariate Analysis: Barplots for important categorical variables (x) ######

# Don't map a variable to y
bar1 <- ggplot(df, aes(x=factor(State)))+
  geom_bar(stat="count", width=0.7, fill="sienna1")+
  theme_minimal()

bar2 <- ggplot(df, aes(x=factor(Coverage)))+
  geom_bar(stat="count", width=0.7, fill="springgreen")+
  theme_minimal()

bar3 <- ggplot(df, aes(x=factor(Education)))+
  geom_bar(stat="count", width=0.7, fill="seagreen2")+
  theme_minimal()

bar4 <- ggplot(df, aes(x=factor(empl.status)))+
  geom_bar(stat="count", width=0.7, fill="red4")+
  theme_minimal()

bar5 <- ggplot(df, aes(x=factor(Gender)))+
  geom_bar(stat="count", width=0.7, fill="plum")+
  theme_minimal()

bar6 <- ggplot(df, aes(x=factor(location)))+
  geom_bar(stat="count", width=0.7, fill="peachpuff")+
  theme_minimal()

plot_grid(bar1, bar2, bar3, bar4, bar5,
  bar6, labels = "AUTO")







########## Check for normality ##########

#from car package
powerTransform(engaged ~ ., data = df)


#QQ Plots for numeric variables
qqPlot(df$CLV) # not normal

qqPlot(df$Income)

qqPlot(df$Total.Claim.Amount)

######## Correlation Matrix ###########


#Threshold for correlation

#Cohen's rule of thumb (if data are normally distributed)
# r = 0.1 weak
# r = 0.3 medium
# r = 0.5 strong

# Build Correlation Matrix structure:
df %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
    corrplot(type = "upper", insig = "blank", diag = FALSE, addCoef.col = "grey")

