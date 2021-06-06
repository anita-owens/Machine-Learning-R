############# Customer Analytics  ######

#Dataset from IBM Watson Community

# Context Using Watson Analytics, you can predict behavior to retain your
# customers. You can analyze all relevant customer data and develop focused
# customer retention programs.
#
# Inspiration Understand customer demographics and buying behavior. Use
# predictive analytics to analyze the most profitable customers and how they
# interact. Take targeted actions to increase profitable customer response,
# retention, and growth.
#
# Source
# https://www.ibm.com/communities/analytics/watson-analytics-blog/marketing-customer-value-analysis/

#####load libraries ######

library(multcomp)
library(tidyverse)
library(cowplot)
library(VIM)
library(ggcorrplot)
library(car)

###### Data Import #############

df<-read.csv("https://raw.githubusercontent.com/pthiagu2/DataMining/master/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", stringsAsFactors = TRUE)
glimpse(df)

#Check response variable
table(df$Response)

#Encode engaged customers as 0s and 1s
df$engaged <- as.integer(df$Response) - 1

table(df$engaged) #check results

########### Check Data #################

#summarize variables in data frame
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


###### Univariate Analysis: Histogram plots for important independent numeric variables##


p1 <-  ggplot(data = df, aes(x = age))+
      geom_histogram(fill = "lightblue", binwidth = 5, colour = "black") +
      geom_vline(aes(xintercept = median(age)), linetype = "dashed")


p2 <- ggplot(data = df, aes(x = balance))+
      geom_histogram(fill = "blue", colour = "black") +
      geom_vline(aes(xintercept = median(balance)), linetype = "dashed")

 # campaign: number of contacts performed during this campaign and for this
 # client (numeric, includes last contact)
p3 <- ggplot(data = df, aes(x = campaign))+
      geom_histogram(fill = "red", binwidth = 3, colour = "black") +
      geom_vline(aes(xintercept = median(campaign)), linetype = "dashed")


# day_of_week: last contact day of the week (categorical:
# 'mon','tue','wed','thu','fri')
p4 <- ggplot(data = df, aes(x = day))+
      geom_histogram(fill = "green", binwidth = 5, colour = "black") +
      geom_vline(aes(xintercept = median(day)), linetype = "dashed")


########## Check for normality ##########

#from car package
powerTransform(engaged ~ ., data = df)

#QQ Plots for numeric variables
#qqPlot(df$CLV) # not normal

#qqPlot(df$Income)

#qqPlot(df$Total.Claim.Amount)

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

# more informative scatterplot matrix
library(psych)
pairs.panels(df[c("CLV", "month.premium",
  "Income",
  "Number.of.Policies",
  "Months.Since.Last.Claim",
  "Months.Since.Policy.Inception",
  "engaged")])

#### Model 1: Linear Regression to establish high CLV customers-------------------


## Step 1: Training a model on the data ----
model_01 <- lm(CLV ~ .-engaged,
                data = df)

# see the estimated beta coefficients
model_01

## Step 2: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(model_01)


# making predictions with the regression model
insurance$pred <- predict(ins_model02, insurance)
cor(insurance$pred, insurance$expenses)

plot(insurance$pred, insurance$expenses)
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)

predict(ins_model02,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "male", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model02,
        data.frame(age = 30, age2 = 30^2, children = 2,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))

predict(ins_model02,
        data.frame(age = 30, age2 = 30^2, children = 0,
                   bmi = 30, sex = "female", bmi30 = 1,
                   smoker = "no", region = "northeast"))
