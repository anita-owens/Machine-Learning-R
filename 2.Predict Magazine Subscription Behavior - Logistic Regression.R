#######Predict whether a customer will subscribe to a magazine########


#Dataset source: Marketing Analytics-Data Driven Techniques in Excel Chap 17

#Logistic Regression to predict a binary dependent variable
#What do customers want?

#We have a dataset of subscribers who subscribed to a magazine or not.
#Can we predict which customers are more likely to subscibe?


######Set Working Directory########



getwd() #check working directory

#change wd
setwd("/Users/anitaowens/Dropbox/Marketing Analytics-Data Driven Techniques with Excel/Excel Files/Chapter 17 Excel Files") #change working directory

getwd() #check working directory



######Load Libraries########
library(tidyverse)
library(cowplot)
library(psych)
library(VIM)
library(ggcorrplot)
library(tree)
library(rpart)


#####Import Data#########

subscribed <- read.csv(file = "subscribed_data.csv", sep = ",")


#check results
glimpse(subscribed)


#########Inspect and get to know the data###########
summary(subscribed)

describe(subscribed)

#any missing data?
## Show cases with missing values
subscribed[!complete.cases(subscribed),] #no missing data

#Using VIM package
aggr(subscribed, prop = F, numbers = T) # no red - no missing values


######Visualize Data: Univariate Analysis ###############


####Boxplots for important independent numeric variables

#age on y axis - subscribed on x axis
ggbi1 <- ggplot(data = subscribed, aes(x = factor(Subscribe), y = Age))+geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red")

#distribution of gender by age - gender on x axis, age on y axis
ggbi2 <- ggplot(data = subscribed, aes(x = factor(Gender), y = Age))+geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=14, color="blue", fill="blue")


####Univariate Analysis: Histogram for numeric variables

#Age distribution - Age on x axis
gghist1 <- ggplot(data=subscribed, aes(x=Age)) +
  geom_histogram(fill = "lightblue", binwidth = 5, colour = "black") +
      geom_vline(aes(xintercept = median(Age)), linetype = "dashed") +
    labs(title= "Age variable follows a normal distribution")
gghist1

ggplot(data=subscribed, aes(x=Age)) +
  geom_histogram(fill = "lightblue", binwidth = 5, color = "black") +
      geom_vline(aes(xintercept = median(Age)), color = "red", linetype = "dashed") +
    labs(title= "Normal distribution")


describe(subscribed$Age)
#Age is normally distributed

###Univariate Analysis: Bar plot for important categorical variables

# Don't map a variable to y - How many subscribed vs did not subscribe
ggbox1 <- ggplot(data=subscribed, aes(x=factor(Subscribe)))+
  geom_bar(stat="count", width=0.7, fill="green")+
  theme_minimal()

#how many men vs. how many women
ggbox2 <- ggplot(data=subscribed, aes(x=factor(Gender)))+
  geom_bar(stat="count", width=0.7, fill="purple")+
  theme_minimal()
#the numbers are evenly split

#might be a short delay until charts are plotted in plot viewer
plot_grid(ggbi1, ggbi2, gghist1, ggbox1, ggbox2, labels = "AUTO")

###### Visualize Data: Bivariate Analysis ###############


#Boxplots for Subscription status (as x variable) vs important numeric (y variables)
bi1 <- ggplot(data = subscribed, aes(x = factor(Subscribe), y = Age, fill = factor(Subscribe)))+geom_boxplot()
#Age follows a normal distribution

#create summary dataset to group by gender for visualization
group_sub<- subscribed %>% 
  group_by(Gender) %>% 
  count(Subscribe) %>% 
  mutate(percent = n/sum(n))
group_sub #check results

#Subscription status by gender
bi2 <- ggplot(data=group_sub, aes(x=factor(Gender), y=percent, fill = factor(Subscribe))) +
  geom_bar(stat = "identity", width=0.7)
#slight more women than men subscribed

plot_grid(bi1, bi2, labels = "AUTO")

##########Analysis of Categorical Variables: Cross-Tables #############

# -We see that the response rate is 24%
tab <- table(subscribed$Subscribe)
prop.table(tab) #24% response rate

prop.table(table(subscribed$Subscribe))


# 2-Way Cross Tabulation
library(gmodels)

#Men are 0 and women are 1
CrossTable(subscribed$Gender, subscribed$Subscribe, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#More women subscribed then men 27% vs. 20%, although both groups are nearly equally represented
# but is it statistically significant?

########## Exploring Associations Between Numeric Variables - Correlation Tests #############

#Is there a linear relationship?
#Estimate the relationship between the variables
# Build Correlation Matrix structure:
subscribed %>%
  select_if(is.numeric) %>% 
  cor() %>% 
    corrplot(type = "upper", insig = "blank", addCoef.col = "black", diag = TRUE)
#There seems to be a negative correlation between subscription status and age


############################# Transform Data/Feature Engineering ###############################

#Let's bucket age into groups before analyzing
summary(subscribed$Age) #select average to split age into 2 buckets

subscribed  <- subscribed%>% 
  mutate(over40 = ifelse(Age >= 40, 1, 0))

#check results
summary(subscribed)
CrossTable(subscribed2$over40, subscribed$Subscribe, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#under40 had a higher subscription rate 33% vs 15% - the total response rate was 24%

#Let's use the cut function instead and split age in multiple groups
# subscribed <- subscribed %>% 
#  mutate(age_bucket = cut(Age, breaks = 6))

#using cut function but with explicit breaks
subscribed <- subscribed %>% 
 mutate(age_bucket = cut(Age, breaks = seq(20, 60, by = 10), include.lowest = TRUE))

# Count customers in each agebucket
subscribed %>% 
  count(age_bucket)

CrossTable(subscribed$age_bucket, subscribed$Subscribe, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#we see the highest subscription rates with the 1st bucket 40%


#Save current df into 2nd dataframe
subscribed_dup_df <- subscribed

#let's remove the 2 columns that we don't need
subscribed <- subscribed %>% 
        select(-c("over40", "age_bucket"))

#check results
names(subscribed)




#######Step 0 : Logistic Regression: Split dataset into development (train) and holdout (validation or test) sets#######

## split the dataset into training and test samples at 70:30 ratio

glimpse(subscribed)

library(caTools)
set.seed(123)


split <- sample.split(subscribed$Subscribe, SplitRatio = 0.7)
train_data <- subset(subscribed, split == TRUE)
test_data <- subset(subscribed, split == FALSE)

## Check if distribution of partition data is correct for the development dataset
prop.table(table(train_data$Subscribe))
prop.table(table(test_data$Subscribe)) 

# The prop.table output above confirms that the imbalanced dataset
# characteristic that we saw in the original dataset is maintained at the same
# proportions in the development and hold out samples as well. The training
# dataset is now ready to build the model.



############### Step 1: Build logistic regression model with train data########################

library(glmnet)

# Use the glm() function in order to model the probability that a customer will
# subscribe by using a logistic regression. Include every
# explanatory variable of the dataset and specify the data that shall be used.

# Specify the argument family.
# Then, extract the coefficients and transform them to the odds ratios.

#Step 1: Run Logistic Regression on the train data
logreg <- glm(Subscribe ~ Age + Gender, 
                family = binomial, data = train_data)

# Take a look at the model
summary(logreg)

# Take a look at the odds
coefsexp <- coef(logreg) %>% exp()%>% round(2)
coefsexp

# Odds ratio, represents which group has better odds of success,
# and it’s given by calculating the ratio of odds for each group
# If we know the coefficients of independent variables Xs and the intercept a, we can predict the probability. 

#There appears to be a negative relationship between age and subscription status
#which is statistically significant!

#Gender isn't statistically significant although more women are likely to
#subscribe!

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

#We don't have any multi-collinear variables that would need to be
#removed

########## Step 2: Overall significance of the model############################## 
library(lmtest)
lrtest(logreg)


# We can see that the low p value indicates the model is highly significant i.e.
# the likelihood of a customer subscribing depends on Age (independent x variables in the dataset)

####Variable Importance

library(caret)

# To assess the relative importance of individual predictors in the model, we
# can also look at the absolute value of the t-statistic for each model
# parameter. This technique is utilized by the varImp function in the caret
# package for general and generalized linear models.

# One of the most common tests in statistics is the t-test, used to determine
# whether the means of two groups are equal to each other. The assumption for
# the test is that both groups are sampled from normal distributions with equal
# variances. The null hypothesis is that the two means are equal, and the
# alternative is that they are not

varImp(logreg)

#By far age is the most important variable


########## Step 3: McFadden or pseudo R² interpretation############################## 

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
#   reasonable if > .02
#   good if > 0.4
#   very good if > .05


#Goodness of fit packages & measures
library(descr)
LogRegR2(logreg)

library(pscl) #does the same thing as the descr package
pR2(logreg)
    


# The measure ranges from 0 to just under 1, with values closer to zero
# indicating that the model has no predictive power.

#pass the Mcfadden R2 into the function to get fit estimates
pseudoR_func <- function(pseudoR){
  if(pseudoR >= 0.5){print("very good fit")}
  if(pseudoR >= 0.4 && pseudoR < 0.5) {print("good fit")}
  if (pseudoR >= 0.2 && pseudoR < 0.4) {print("reasonsable fit")}
  else{print("try again")}}

pseudoR_func(0.3148552) #reasonable fit

# Based on the value of McFadden R², we can conclude that 7.35% of the
# uncertainty of the intercept-only model is explained by the full model
# (current x variables).The model fits well enough.


########## Step 4: Individual coefficients significance and interpretation############################# 

#library(coef.lmList)

summary(logreg)

# The Odds Ratio and Probability of each x variable is calculated based on the
# formulae, Odds Ratio = exp(Co-efficient estimate) Probability = Odds Ratio /
# (1 + Odds Ratio)

# Take a look at the odds - extract coefficients
coefsex <- coef(logreg) %>% exp()%>% round(2)

coefsexDF <- as.data.frame(coefsex)

#attempts
data.frame(matrix(unlist(coefsex), nrow=30, byrow=T))
#coef(coefsex, augFrame = TRUE)

#add probability
coefsexDF <- coefsexDF %>% 
        mutate(Probability = coefsex/(1+coefsex))

#check results
coefsexDF

######Step 5: Model Accuracy#################

#Use predict() to receive a probability for each customer to Subscribe(yes/no)
pred <- predict(logreg, test_data, type = "response") #predict using test data

#check results
head(pred)

predicted <- round(pred) #>0.5 will convert to 1

#contingency table or Confusion Matrix
contingency_tab <- table(predicted, test_data$Subscribe)
contingency_tab 
class(contingency_tab)

#sum the diagnals for the percentage accuracy classified
sum(diag(contingency_tab))/sum(contingency_tab) *100
#76% were correctly classified according to our model

# Confusion Matrix using the caret package
caret::confusionMatrix(contingency_tab)

#########Summary of Signficant Variables from Log Regression Model#################

# Negative correlation
# -age

# The estimated coefficients of the logistic
# regression model predicts the probability
# of subscribing. A one-unit decrease in age is associated
# with in decrease in the log odds of subscribe
# by.059307 units.

#since the p-value for Age is tiny, we can reject the H0.
#There is a difference in subscriber rates due to age

#H0=there is no difference - nothing going on
#HA=there is something going on.

#The estimated intercept in the model is typically not of interest;
#its main purpose is to adjust the average fitted probabilities 
#to the proportion of ones in the data.

########ML Model 2: Regression Tree for Classification #######

#Let's see if we get the same result with the regression tree

# Classification Tree with rpart package

#transform gender to factor variable
#train_data$Gender <- as.factor(train_data$Gender)

#table(train_data$Gender)

# grow tree
tree.fit <- rpart(Subscribe ~ .,
   method="class", data=train_data)

printcp(tree.fit) # display the results
plotcp(tree.fit) # visualize cross-validation results
summary(tree.fit) # detailed summary of splits

# plot tree
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(tree.fit, uniform=TRUE,
   main="Classification Tree for Subscription Status by Age and Gender")
text(tree.fit, use.n=TRUE, all=TRUE, cex=.8)

#Tree interpretation

#Age is the most important factor in determining the response
#The first split is based on age and YES if age is greater than 33.5 years
#goes to the left branch where these observations were predicted to not subscribe.
#Having Ages below 33.5 are assigned to the right branch and then subdivided by another
#age group with mean response value of 23.5 years and then gender (0=male, 1=female).
#So of a customer is between 24 and 34 years old, they are predicted not to
#subscribe. But a customer under 24 is predicted to subscribe.


#How does the tree fit?
# The summary() function lists the variables that are used as internal nodes in
# the tree, the number of terminal nodes, and the (training) error rate.
summary(tree.fit)

# In order to properly evaluate the performance of a classification tree on
# these data, we must estimate the test error rather than simply computing the
# training error. We built the tree using the training set, and must evaluate its performance on the
# test data. The predict() function can be used for this purpose.

set.seed(2)

tree.pred <- predict(tree.fit, train_data, type="class")

table(tree.pred, train_data[,3])


# In the case of a classification tree, the argument type="class" instructs R to
# return the actual class prediction. This approach leads to correct predictions
# for around 77% of the locations in the test data set.
#add the diagnal and divide by the number of observations in the training
#data
(701+27)/942
  
#contingency table
contingency_tab <- table(tree.pred, train_data[,3])
contingency_tab 
class(contingency_tab)

#sum the diagnals for the percentage accuracy classified
sum(diag(contingency_tab))/sum(contingency_tab) *100
#this should match the above


#The regression tree confirms a bit of what we saw in the log regression model although
#less accurate than the logistic regresion model.

#Age is the most important variable.

#The older the customer, the less likely they are to subscribe.If you are a male gender < 0.5,
#then the less likely you are to subscribe.



######Recommendations##########

#look at age cross tab again
CrossTable(subscribed_dup_df$age_bucket, subscribed_dup_df$Subscribe, digits=2, prop.c = FALSE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#We can safely say that the younger the customer (less than 33 years old),
# the more likely to subscribe (median age == 40).
#More women are likely to subscribe. 
#Let's take a look at the creatives to figure out why.
#Perhaps there is some cost savings by excluding older segments from 
#marketing efforts in order to gain more subscribers.


