############ Online Shoppers Purchasing Intention dataset ################



######Load libraries###########

library(rpart)
library(psych)
library(dplyr)
library(corrplot)
library(ggplot2)
library(cowplot)
library(tree)
library(VIM)
library(GGally)
library(lubridate)


# The dataset consists of feature vectors belonging to 12,330 sessions.
# The dataset was formed so that each session
# would belong to a different user in a 1-year period to avoid
# any tendency to a specific campaign, special day, user
# profile, or period.


# The dataset consists of 10 numerical and 8 categorical attributes.

# The 'Revenue' attribute can be used as the class label.
#----this is what we want to predict

# "Administrative", "Administrative Duration", "Informational", "Informational
# Duration", "Product Related" and "Product Related Duration" represent the
# number of different types of pages visited by the visitor in that session and
# total time spent in each of these page categories. The values of these
# features are derived from the URL information of the pages visited by the user
# and updated in real time when a user takes an action, e.g. moving from one
# page to another. 

#The "Bounce Rate", "Exit Rate" and "Page Value" features
# represent the metrics measured by "Google Analytics" for each page in the
# e-commerce site. 

#The value of "Bounce Rate" feature for a web page refers to
# the percentage of visitors who enter the site from that page and then leave
# ("bounce") without triggering any other requests to the analytics server
# during that session.

#The value of "Exit Rate" feature for a specific web page
# is calculated as for all pageviews to the page, the percentage that were the
# last in the session. 

#The "Page Value" feature represents the average value for
# a web page that a user visited before completing an e-commerce transaction.

# The "Special Day" feature indicates the closeness of the site visiting time to
# a specific special day (e.g. Mother’s Day, Valentine's Day) in which the
# sessions are more likely to be finalized with transaction. The value of this
# attribute is determined by considering the dynamics of e-commerce such as the
# duration between the order date and delivery date. For example, for
# Valentina’s day, this value takes a nonzero value between February 2 and
# February 12, zero before and after this date unless it is close to another
# special day, and its maximum value of 1 on February 8. 

#The dataset also
# includes operating system, browser, region, traffic type, visitor type as
# returning or new visitor, a Boolean value indicating whether the date of the
# visit is weekend, and month of the year.

#Dataset found here:
browseURL("https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+dataset")


#browseURL("https://cxl.com/blog/correlative-metrics/")

##########Import Data###################

getwd() #check working directory

#change wd
setwd("/Users/anitaowens/R_Programming/UCI Machine Learning Repo/Online Shoppers") #change working directory

getwd() #check working directory


dataset <- read.csv("online_shoppers_intention.csv", sep = ',')

head(dataset)
table(dataset$Revenue) #the variable we're trying to predict


########Review the data structure - Explore dataset###########
glimpse(dataset)

########## Any Missing Data #############

## Show cases with missing values
dataset[!complete.cases(dataset),] #no missing data

#using aggr function from VIM packages --missing values are in red
aggr(dataset, prop = F, numbers = T)

#No missing data, so we can continue


#########Recode binary variables into 0/1 #####
dataset <- dataset %>% 
      mutate(weekend = ifelse(Weekend == "FALSE", 0,1),
            revenue = ifelse(Revenue == "FALSE", 0,1))

#check results
head(dataset)

table(dataset$Revenue)
table(dataset$revenue)


##################### Transformation: Remove unneeded variables and factor variables #######################
#factor revenue
dataset$revenue <- as.factor(dataset$revenue)
str(dataset$revenue) #check results

#check month formatting
str(dataset$Month)

#turn character month into date object & add to dataframe
dataset$date <- parse_date_time(dataset$Month, orders = "m")

#check results
str(dataset$date)

#now extract only the month
dataset$month <- as.Date(dataset$date, format = "B")

#check the results-
str(dataset$month) #doesn't really work

#let's remove un-needed date variables
dataset <- dataset %>% 
  select(c(-Weekend, -Revenue, -date, -Month))

names(dataset) #check results


##### Univariate Analysis: Barplots for important categorical variables (x) ######

# Don't map a variable to y
bar1 <- ggplot(dataset, aes(x=factor(month)))+
  geom_bar(stat="count", width=0.7, fill="sienna1")+
  theme_minimal()

bar2 <- ggplot(dataset, aes(x=factor(OperatingSystems)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

bar3 <- ggplot(dataset, aes(x=factor(Browser)))+
  geom_bar(stat="count", width=0.7, fill="salmon4")+
  theme_minimal()

bar4 <- ggplot(dataset, aes(x=factor(Region)))+
  geom_bar(stat="count", width=0.7, fill="red4")+
  theme_minimal()

bar5 <-ggplot(dataset, aes(x=factor(TrafficType)))+
  geom_bar(stat="count", width=0.7, fill="plum")+
  theme_minimal()

bar6 <- ggplot(dataset, aes(x=factor(VisitorType)))+
  geom_bar(stat="count", width=0.7, fill="yellowgreen")+
  theme_minimal()

bar7 <-ggplot(dataset, aes(x=factor(weekend)))+
  geom_bar(stat="count", width=0.7, fill="forestgreen")+
  theme_minimal()

#Revenue - the response variable
bar8 <- ggplot(dataset, aes(x=factor(revenue)))+
  geom_bar(stat="count", width=0.7, fill="darkorange")+
  theme_minimal()

plot_grid(bar1, bar2, bar3, bar4, bar5, bar6, bar7, bar8, labels = "AUTO")

###### Univariate Analysis: Histogram plots for important independent numeric variables##

#A histogram is another way to visualize the spread of a numeric variable

p1 <-  ggplot(data = dataset, aes(x = Administrative))+
      geom_histogram(fill = "lightblue", binwidth = 5, colour = "black") +
      geom_vline(aes(xintercept = median(Administrative)), linetype = "dashed")

#plot on a log
p2 <- ggplot(data = dataset, aes(x = Administrative_Duration))+
      geom_histogram(fill = "blue", binwidth = 50, colour = "black") +
      geom_vline(aes(xintercept = median(Administrative_Duration)), linetype = "dashed") +
      scale_y_log10()

p3 <- ggplot(data = dataset, aes(x = Informational))+
      geom_histogram(fill = "deeppink1", binwidth = 10, colour = "black") +
      geom_vline(aes(xintercept = median(Informational)), linetype = "dashed")

#plot on a log
p4 <- ggplot(data = dataset, aes(x = Informational_Duration))+
      geom_histogram(fill = "green", binwidth = 50, colour = "black") +
      geom_vline(aes(xintercept = median(Informational_Duration)), linetype = "dashed") +
      scale_y_log10()


p5 <- ggplot(data = dataset, aes(x = ProductRelated))+
      geom_histogram(fill = "springgreen", binwidth = 50, colour = "black") +
      geom_vline(aes(xintercept = median(ProductRelated)), linetype = "dashed")

#plot on a log
p6 <- ggplot(data = dataset, aes(x = ProductRelated_Duration))+
      geom_histogram(fill = "lightslategray", binwidth = 500, colour = "white") +
      geom_vline(aes(xintercept = median(ProductRelated_Duration)), linetype = "dashed") +
      scale_y_log10()

plot_grid(p1, p2, p3, p4, p5, p6, labels = "AUTO")




#####Univariate Analysis: Boxplots for important independent numeric variables  ######


#response 
ggbi1 <- ggplot(data = dataset, aes(x = revenue, y = SpecialDay, fill = revenue))+geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red")

plot_grid(ggbi1,  labels = "AUTO")



##########Bivariate analysis: Boxplots for Revenue (as x variable) vs important numeric (y variables) ##########

bi1 <- ggplot(data = dataset, aes(x = factor(revenue), y = Administrative, fill = revenue))+geom_boxplot()
bi2 <- ggplot(data = dataset, aes(x = factor(revenue), y = Informational, fill = revenue))+geom_boxplot()
bi3 <- ggplot(data = dataset, aes(x = factor(revenue), y = ProductRelated, fill = revenue))+geom_boxplot()
bi4 <- ggplot(data = dataset, aes(x = factor(revenue), y = OperatingSystems, fill = revenue))+geom_boxplot()

plot_grid(bi1, bi2, bi3, bi4, labels = "AUTO")


# When we look at the data visualization from bivariate analysis of numeric
# variables against the categorical revenue variable, we get the following
# insights:




########### Examining Relationships through two-way cross tabulations ############

#A purchase was made in 15% of all visits 
tab <- table(dataset$revenue)
prop.table(tab) 


# 2-Way Cross Tabulation
library(gmodels)

#41% of all purchases came from OS 3.
CrossTable(dataset$OperatingSystems, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#Certain browsers convert much better twice that of the avg CvR but
#the number of users coming from these browsers are small
CrossTable(dataset$Browser, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#Region 1 makes up 39% of our traffic (the majority)
#but there doesn't seem to be a big difference in CvR amongst the regions
CrossTable(dataset$Region, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = TRUE, chisq = FALSE, format = "SAS", expected = FALSE)

#Traffic type 2 is the best when it comes to revenue
CrossTable(dataset$TrafficType, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#We're much better at converting new users than returning users 25% cVr
#returning users cvr is half that of new users 14% CvR
#returning users mkes up 86% of all traffic
#maybe we should put our focus on returning visitors
CrossTable(dataset$VisitorType, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = TRUE, chisq = FALSE, format = "SAS", expected = FALSE)

#no difference in revenue based on weekend status
CrossTable(dataset$weekend, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#May & November are the best months for revenue
CrossTable(dataset$month, dataset$revenue, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)


############# Exploring Associations Between Numeric Variables - Correlation Tests ################


#Is there a linear relationship?
#Estimate the relationship between the variables
# Build Correlation Matrix structure:
dataset %>%
  select_if(is.numeric) %>% 
  cor() %>% 
    corrplot(type = "upper", insig = "blank", diag = TRUE)
# A lot of negative correlations but nothing significant


# Compute summary statistics of all columns in the data frame
dat_summ <- summary(all_num)
print(dat_summ)

# create a pairwise scatterplot of all variables in dataset
dataset %>%
   ggpairs()

########### What question are we trying to answer? ###################

#We want to identify what characteristics makes a converting customer.

#We have a revenue column so it's
#best to try a supervised learning algorithm
# 1st choice - logistic regression algorithm



####### ML Model 1: Logistic Regression: Split dataset into development (train) and holdout (validation or test) sets #######

#remove month variable
dataset <- dataset %>% 
      select(-month)

#recode target variable to classification variable
str(dataset$revenue)
dataset$revenue <- as.factor(dataset$revenue)
str(dataset$revenue) #check results

glimpse(dataset)

library(caTools)
set.seed(123)

## split the dataset into training and test samples at 70:30 ratio
split <- sample.split(dataset$revenue, SplitRatio = 0.7)
train_data <- subset(dataset, split == TRUE)
test_data <- subset(dataset, split == FALSE)

## Check if distribution of partition data is correct for the development dataset
prop.table(table(train_data$revenue))
prop.table(table(test_data$revenue)) 

# The prop.table output above confirms that the imbalanced dataset
# characteristic that we saw in the original dataset is maintained at the same
# proportions in the development and hold out samples as well. The training
# dataset is now ready to build the model.


############### Step 1: Build logistic regression model########################

library(glmnet)

# Use the glm() function in order to model the probability that a customer will
# respond to campaign by using a logistic regression. Include every
# explanatory variable of the dataset and specify the data that shall be used.

# Specify the argument family.
# Then, extract the coefficients and transform them to the odds ratios.

#Step 1: Run Logistic Regression on the train data
logreg <- glm(revenue ~ ., 
                family = binomial, data = train_data)

# Take a look at the model
summary(logreg)


# From the summary of the generated model, we infer that there are some independent
# variables that are significant based on their p value. 

#The p-values associated with ExitRates,
# PageValues, VisitorTypes, are so tiny we can reject
#the null hypothesis. There indeed is an association between
#revenue and these characteristics of the customer.

# Take a look at the odds
coefsexp <- coef(logreg) %>% exp()%>% round(2)
coefsexp

# Odds ratio, represents which group has better odds of success,
# and it’s given by calculating the ratio of odds for each group
# If we know the coefficients of independent variables Xs and the intercept a,
#we can predict the probability. 

#We can plot the coefficients by their significance
library(coefplot)
coefplot(logreg, intercept = FALSE,
  outerCI=1.96, lwdOuter=1.5,
  ylab="Characteristics",
  xlab="")


########## Step 1a: Variable Inflation Factor check############################## 

# In general, I want my R2 to be as high as possible.
#
# R2 & F-test Is a test of the overall fit of the model whether or not r2 is
# equal to 0. Does one or more regressors has significant explantory power.

#Overfitting

#Methods to avoid overfitting
# -Keep your model lean
# -AIC() from stats package --controls for overfitting
# -stepAIC() from MASS package
# -out-of-sample model validation
# -cross-validation

library(car)
vif(logreg)

# Feature (x) variables with a VIF value above 5 indicate high degree of
# multi-collinearity.

#multicollinearity does not bias coefficients; it inflates their standard errors.
# multicollinearity does not usually alter the interpretation of the coefficients of interest unless they lose statistical significance.

#poutcome and job has a high VIF, we will remove it from our model and run log regression again
#to compare

logreg_var_remov <- glm(revenue ~ . -ProductRelated -ProductRelated_Duration, 
                family = binomial, data = train_data)

# Take a look at the model
summary(logreg_var_remov)

#check vif again
vif(logreg_var_remov)

#No VIF larger than 4, but removing the 2 variables does not change
#our log regression that much so keeping them is fine.

########## Step 2: Overall significance of the model############################## 
library(lmtest)
lrtest(logreg)

# We can see that the low p value indicates the model is highly significant i.e.
# the likelihood of a conversion (revenue) depends on
# independent x variables in the dataset.



########## Step 3: McFadden or pseudo R² interpretation############################## 

#Goodness of fit packages & measures
library(descr)
LogRegR2(logreg)

# How to evaluate your model.
# 
# With logistic regression there are several ways to go about fitting your model
# 
# There are 3 pseudo r2 statistics:
#   -McFadden
#   -Cox & Snell
#   -Nagelkerke
# 
# Interpretation:
#   
#   reasonable if > 0.2
#   good if > 0.4
#   very good if > 0.5


library(pscl)
pseudo_stats <- pR2(logreg)
print(pseudo_stats[4])
 

#custom function for determining fit based on pseudo R-squared
pseudoR_func <- function(pseudoR){
  if(pseudoR >= 0.5){print("very good fit")}
  else if(pseudoR >= 0.4 && pseudoR < 0.5) {print("good fit")}
  else if (pseudoR >= 0.2 && pseudoR < 0.4) {print("reasonsable fit")}
  else{print("try again")}
}
#pass the Mcfadden R2 into the function to get fit estimate
pseudoR_func(pseudo_stats[4])


########## Step 4: Individual coefficients significance and interpretation############################# 

library(coef.lmList)

summary(logreg)

# The Odds Ratio and Probability of each x variable is calculated based on the
# formulae, Odds Ratio = exp(Co-efficient estimate) Probability = Odds Ratio /
# (1 + Odds Ratio)

# Take a look at the odds - extract coefficients
coefsex <- coef(logreg) %>% exp()%>% round(2)

#put into a dataframe
coefsexDF <- as.data.frame(coefsex)


data.frame(matrix(unlist(coefsex), nrow=30, byrow=T))
#coef(coefsex, augFrame = TRUE)

#add probability
coefsexDF <- coefsexDF %>% 
        mutate(Probability = coefsex/(1+coefsex))

#check results
View(coefsexDF)

######Step 5: Model Accuracy#################

pred <- predict(logreg, test_data, type = "response") #predict using test data

#check results
head(pred)

predicted <- round(pred) #>0.5 will convert to 1

#contingency table - create table for confusion matrix
contingency_tab <- table(test_data$revenue, predicted)
contingency_tab 

#sum the diagnals for the percentage accuracy classified
sum(diag(contingency_tab))/sum(contingency_tab) *100
# ---- Our model leads 88% to correct predictions

#Calculate accuracy using Caret package
caret::confusionMatrix(contingency_tab)

#########Summary of Significant Variables from Log Regression Model#################

# Positive correlation
# -Visitor type is important
# -The different pages a user visits is important and where they exit
# from the site
#  - And whether they visited a product related page
# # 
# # Negative correlation
# -special day doesn't appear to make much of a difference
# 

#We presumed weekend makes a difference, but it doesn't.

########## ML Model 2: Random Forest #############

#We use the same training data
head(train_data)
head(test_data)

#load library and set seed
library(randomForest)
set.seed(98040)

#model data
(rf_mod <- randomForest(revenue ~ ., data=train_data, ntree=3000, importance = TRUE))
rf_mod

#variable importance
importance(rf_mod)

#The randomeForst package includes varImpPlot()
# Plot of the most important variables
varImpPlot(rf_mod, main="Variable Importance")


library(qplots)

#this works for the classes - 0 or 1
heatmap(t(importance(rf_mod)[,1:2]), margins = c(10,10), main = "Variable importance")

#Darker shades signify higher importance for the variable
#column in differentiating a segment row.



#This states that page values are more important
#however, page value is an arbitrary value most
#likely assigned by marketing or an analyst to measure website
#performance.

#For those who convert, month, exit rates,
#bounce rates & product related duration
#are most import variables.

#This confirms a lot of what we saw in the logistic
#regression model

#No need to improve the model