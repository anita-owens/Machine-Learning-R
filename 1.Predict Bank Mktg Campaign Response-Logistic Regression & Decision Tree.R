############Customer response prediction using Logistic Regression & Decision Trees ################

# The classification goal is to predict if the client will subscribe (yes/no) a
# term deposit (variable y).

# In this article, we will see how we could harness the power of machine
# learning to target the campaigns towards the right set of customers, thereby
# increasing conversion propensity. We will be using past data available on the
# demography, bank details, and transaction patterns of customers who have
# responded and not responded to a campaign, as training data to
# predict the probability if a customer will respond to the campaign.


######Load libraries###########

library(rpart)
library(psych)
library(dplyr)
library(corrplot)
library(ggplot2)
library(cowplot)
library(tree)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Customer response prediction using Logistic Regression

# The classification goal is to predict if the client will subscribe (yes/no) a
# term deposit (variable y).

# In this article, we will see how we could harness the power of machine
# learning to target the campaigns towards the right set of customers, thereby
# increasing conversion propensity. We will be using past data available on the
# demography, bank details, and transaction patterns of customers who have
# responded and not responded to a campaign, as training data to
# predict the probability if a customer will respond to the campaign.

#browseURL("https://archive.ics.uci.edu/ml/datasets/Bank+Marketing")

##########Import Data###################

getwd() #check working directory

#change wd
setwd("/Users/anitaowens/R_Programming/UCI Machine Learning Repo/Bank Marketing Data") #change working directory

getwd() #check working directory


bank_data <- read.csv("bank-full.csv", sep = ';')

head(bank_data)
glimpse(bank_data)


table(bank_data$default)
table(bank_data$loan)
table(bank_data$campaign)
table(bank_data$y) #the variable we're trying to predict
table(bank_data$housing)



######### Encoding - Recode binary variables into 0/1 #####
bank_data <- bank_data %>% 
      mutate(default_target = ifelse(default == "no", 0,1),
            housing_target = ifelse(housing == "no", 0,1),
            loan_target = ifelse(loan == "no", 0,1),
            target = ifelse(y == "no", 0,1)) #target is now the variable we want to predict

#check results
head(bank_data)
dim(bank_data)
names(bank_data)

#remove re-coded variables plus the y variable as we no longer need it
bank_data <- bank_data %>% 
      select(target, age, job, marital,
        education, balance, contact, day, month,
        duration, campaign, pdays, previous, poutcome,
        default_target, housing_target, loan_target)

#rename variables
bank_data  <- rename(bank_data,
                        default = default_target,
                        housing = housing_target,
                        loan = loan_target
)

#Convert all character variable into factor in one line:
bank_data <- bank_data %>% mutate_if(is.character, as.factor)
glimpse(bank_data)

#didn't work for our new variables
bank_data$default<- as.factor(bank_data$default)
bank_data$housing<- as.factor(bank_data$housing)
bank_data$loan <- as.factor(bank_data$loan)

glimpse(bank_data) #check results

########## Baseline Response Rate & Segments comparison #############

prop.table(table(bank_data$target))

conversion_rate  <- 0.1169848 

#data frame grouped by job
job.df <- bank_data %>% 
  group_by(job) %>% 
  summarize(total_count = n(),
    total_resp = sum(target)) %>% 
  mutate(response_rate = total_resp/total_count)

job.df #check results

#check conversions colum
sum(job.df$total_resp)
sum(job.df$total_count)

#add highlight flag column
job.df <- job.df %>% 
mutate(highlight_flag =
    ifelse(response_rate > conversion_rate, 1, 0))

#check results
head(job.df$highlight_flag)

#plot response rate by job
ggplot(data=job.df, aes(x=reorder(job, response_rate), y=response_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=conversion_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = ' ', y = 'Response Rate', title = str_c("Response rate by job type")) +
  theme(legend.position = "none")


#data frame grouped by marital status
marital.df <- bank_data %>% 
  group_by(marital) %>% 
  summarize(total_count = n(),
    total_resp = sum(target)) %>% 
  mutate(response_rate = total_resp/total_count)

marital.df #check results

#check conversions colum
sum(marital.df$total_resp)
sum(marital.df$total_count)

#add highlight flag column
marital.df <- marital.df %>% 
mutate(highlight_flag =
    ifelse(response_rate > conversion_rate, 1, 0))

#plot response rate by marital status
ggplot(data=marital.df, aes(x=reorder(marital, response_rate), y=response_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=conversion_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = ' ', y = 'Response Rate', title = str_c("Response rate by marital status")) +
  theme(legend.position = "none")

#data frame grouped by education
educ.df <- bank_data %>% 
  group_by(education) %>% 
  summarize(total_count = n(),
    total_resp = sum(target)) %>% 
  mutate(response_rate = total_resp/total_count)

educ.df #check results

#check conversions colum
sum(educ.df$total_resp)
sum(educ.df$total_count)

#add highlight flag column
educ.df <- educ.df %>% 
mutate(highlight_flag =
    ifelse(response_rate > conversion_rate, 1, 0))

#plot response rate by education
ggplot(data=educ.df, aes(x=reorder(education, response_rate), y=response_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=conversion_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = 'Education', y = 'Response Rate', title = str_c("Insights")) +
  theme(legend.position = "none")

#data frame grouped by contact
contact.df <- bank_data %>% 
  group_by(contact) %>% 
  summarize(total_count = n(),
    total_resp = sum(target)) %>% 
  mutate(response_rate = total_resp/total_count)

contact.df #check results

#check conversions colum
sum(contact.df$total_resp)
sum(contact.df$total_count)

#add highlight flag column
contact.df <- contact.df %>% 
mutate(highlight_flag =
    ifelse(response_rate > conversion_rate, 1, 0))

#plot response rate by contact
ggplot(data=contact.df, aes(x=reorder(contact, response_rate), y=response_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=conversion_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = 'contact', y = 'Response Rate', title = str_c("Insights")) +
  theme(legend.position = "none")

#data frame grouped by month
month.df <- bank_data %>% 
  group_by(month) %>% 
  summarize(total_count = n(),
    total_resp = sum(target)) %>% 
  mutate(response_rate = total_resp/total_count)

month.df #check results

#check conversions colum
sum(month.df$total_resp)
sum(month.df$total_count)

#add highlight flag column
month.df <- month.df %>% 
mutate(highlight_flag =
    ifelse(response_rate > conversion_rate, 1, 0))

#plot response rate by month
ggplot(data=month.df, aes(x=month, y=response_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=conversion_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = 'month', y = 'Response Rate', title = str_c("Insights")) +
  theme(legend.position = "none")


#data frame grouped by poutcome
poutcome.df <- bank_data %>% 
  group_by(poutcome) %>% 
  summarize(total_count = n(),
    total_resp = sum(target)) %>% 
  mutate(response_rate = total_resp/total_count)

poutcome.df #check results

#check conversions colum
sum(poutcome.df$total_resp)
sum(poutcome.df$total_count)

#add highlight flag column
poutcome.df <- poutcome.df %>% 
mutate(highlight_flag =
    ifelse(response_rate == max(response_rate), 1, 0))

poutcome.df$highlight_flag

#plot response rate by poutcome
ggplot(data=poutcome.df, aes(x=reorder(poutcome, response_rate), y=response_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=conversion_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = ' ', y = 'Response Rate', title = str_c("Outcome of the previous marketing campaign - Response Rate 11.69%")) +
  theme(legend.position = "none")

#The response rate is 64% (through the roof) if the customer had responded positively
#to a previous marketing campaign. Is this significant?

#use chi-square test
chisq<- chisq.test(bank_data$target, bank_data$poutcome)
chisq

p_value <- chisq$p.value
p_value < .05 #the p-value is tiny thus highly significant!



###### Univariate Analysis: Histogram plots for important independent numeric variables##
#?colors(distinct = FALSE)

p1 <-  ggplot(data = bank_data, aes(x = age))+
      geom_histogram(fill = "lightblue", binwidth = 5, colour = "black") +
      geom_vline(aes(xintercept = median(age)), linetype = "dashed")


p2 <- ggplot(data = bank_data, aes(x = balance))+
      geom_histogram(fill = "blue", colour = "black") +
      geom_vline(aes(xintercept = median(balance)), linetype = "dashed")

 # campaign: number of contacts performed during this campaign and for this
 # client (numeric, includes last contact)
p3 <- ggplot(data = bank_data, aes(x = campaign))+
      geom_histogram(fill = "red", binwidth = 3, colour = "black") +
      geom_vline(aes(xintercept = median(campaign)), linetype = "dashed")


# day_of_week: last contact day of the week (categorical:
# 'mon','tue','wed','thu','fri')
p4 <- ggplot(data = bank_data, aes(x = day))+
      geom_histogram(fill = "green", binwidth = 5, colour = "black") +
      geom_vline(aes(xintercept = median(day)), linetype = "dashed")

# duration: last contact duration, in seconds (numeric). Important note: this
# attribute highly affects the output target (e.g., if duration=0 then y='no').
# Yet, the duration is not known before a call is performed. Also, after the end
# of the call y is obviously known. Thus, this input should only be included for
# benchmark purposes and should be discarded if the intention is to have a
# realistic predictive model.
p5 <- ggplot(data = bank_data, aes(x = duration))+
      geom_histogram(fill = "springgreen", binwidth = 365, colour = "black") +
      geom_vline(aes(xintercept = median(duration)), linetype = "dashed")

# pdays: number of days that passed by after the client was last contacted from
# a previous campaign (numeric; 999 means client was not previously contacted)
p6 <- ggplot(data = bank_data, aes(x = pdays))+
      geom_histogram(fill = "yellow", binwidth = 365, colour = "black") +
      geom_vline(aes(xintercept = median(pdays)), linetype = "dashed")

# previous: number of contacts performed before this campaign and for this
# client (numeric)
p7 <- ggplot(data = bank_data, aes(x = previous))+
      geom_histogram(fill = "slateblue1", binwidth = 1, colour = "black") +
      geom_vline(aes(xintercept = median(previous)), linetype = "dashed")

plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'))
plot_grid(p4, p5, p6, p7, labels = c('D', 'E', 'F', 'G'))

#Just to get a sense of the data

#####Univariate Analysis: Boxplots for important independent numeric variables  ######

#age on y axis
ggbi1 <- ggplot(data = bank_data, aes(x = job, y = age, fill = job))+geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red")
ggbi2 <- ggplot(data = bank_data, aes(x = marital, y = age, fill = marital))+geom_boxplot()
ggbi3 <- ggplot(data = bank_data, aes(x = housing, y = age, fill = housing))+geom_boxplot()
ggbi4 <- ggplot(data = bank_data, aes(x = default, y = age, fill = default))+geom_boxplot()

plot_grid(ggbi1, ggbi2, ggbi3, ggbi4, labels = "AUTO")

#unsurprisingly retired customers skew older
#the avg age across the different professions are pretty similar (middle-aged) with
#the exception of the student segment with an avg age of 26
aggregate(bank_data$age, by=list(bank_data$job), FUN = summary)

#loan on x axis
ggbi5 <-  ggplot(data = bank_data, aes(x = loan, y = age, fill = loan))+geom_boxplot()
ggbi6 <-  ggplot(data = bank_data, aes(x = loan, y = duration, fill = loan))+geom_boxplot()
plot_grid(ggbi5, ggbi6, labels = "AUTO")
#no difference to response based on age if they already had a loan or duration

#response variable on x axis - numeric variables on y axis
ggbi7 <- ggplot(data = bank_data, aes(x = target, y = age, fill = target))+geom_boxplot()
ggbi8 <- ggplot(data = bank_data, aes(x = target, y = balance, fill = target))+geom_boxplot()
plot_grid(ggbi7, ggbi8, labels = "AUTO")


##### Univariate Analysis: Barplots for important categorical variables (x) ######

# Don't map a variable to y
ggplot(bank_data, aes(x=factor(job)))+
  geom_bar(stat="count", width=0.7, fill="sienna1")+
  theme_minimal()

ggplot(bank_data, aes(x=job))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

ggplot(bank_data, aes(x=education))+
  geom_bar(stat="count", width=0.7, fill="salmon4")+
  theme_minimal()

# default: has credit in default? (categorical: 'no','yes','unknown')
ggplot(bank_data, aes(x=default))+
  geom_bar(stat="count", width=0.7, fill="red4")+
  theme_minimal()

#housing: has housing loan? (categorical: 'no','yes','unknown')
ggplot(bank_data, aes(x=housing))+
  geom_bar(stat="count", width=0.7, fill="plum")+
  theme_minimal()

#loan: has personal loan? (categorical: 'no','yes','unknown')
ggplot(bank_data, aes(x=loan))+
  geom_bar(stat="count", width=0.7, fill="yellowgreen")+
  theme_minimal()

#contact: contact communication type (categorical: 'cellular','telephone')
ggplot(bank_data, aes(x=contact))+
  geom_bar(stat="count", width=0.7, fill="wheat1")+
  theme_minimal()

 # poutcome: outcome of the previous marketing campaign (categorical:
 # 'failure','nonexistent','success')
ggplot(bank_data, aes(x=poutcome))+
  geom_bar(stat="count", width=0.7, fill="violetred")+
  theme_minimal()

##########Bivariate analysis: Boxplots for TARGET (as x variable) vs important numeric (y variables) ##########

bi1 <- ggplot(data = bank_data, aes(x = factor(target), y = age, fill = target))+geom_boxplot()
bi2 <- ggplot(data = bank_data, aes(x = factor(target), y = balance, fill = target))+geom_boxplot()
bi3 <- ggplot(data = bank_data, aes(x = factor(target), y = day, fill = target))+geom_boxplot()
bi4 <- ggplot(data = bank_data, aes(x = factor(target), y = duration, fill = target))+geom_boxplot()

plot_grid(bi1, bi2, bi3, bi4, labels = c('A', 'B', 'C', 'D'))


# When we look at the data visualization from bivariate analysis of numeric
# variables against the categorical target variable, we get the following
# insights:

# -AVG Duration of last contact seems to be the only variable that is higher among responders.
#We can infer that higher duration suggests there is a higher response rate,but we don't know if this
#is significant yet
bi4


# duration: last contact duration, in seconds (numeric). Important note: this
# attribute highly affects the output target (e.g., if duration=0 then y='no').
# Yet, the duration is not known before a call is performed. Also, after the end
# of the call y is obviously known. Thus, this input should only be included for
# benchmark purposes and should be discarded if the intention is to have a
# realistic predictive model.

#######Bivariate analysis: Stacked Bar plot for Categorical x variables vs TARGET (y variable)############

# Stacked barplot with multiple groups
sb1 <- ggplot(data=bank_data, aes(x=job, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

sb2 <- ggplot(data=bank_data, aes(x=marital, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

sb3 <- ggplot(data=bank_data, aes(x=education, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

sb4 <- ggplot(data=bank_data, aes(x=default, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

sb5 <- ggplot(data=bank_data, aes(x=housing, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

sb6 <- ggplot(data=bank_data, aes(x=loan, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

sb7 <- ggplot(data=bank_data, aes(x=month, y=as.factor(target), fill=as.factor(target))) +
  geom_bar(stat="identity")

plot_grid(sb1, sb2, sb3, labels = "AUTO")
plot_grid(sb4, sb5, sb6, sb7, labels =  "AUTO")

########What were the campaign parameters?############

#last contact day of the week 
table(bank_data$day)

#last contact month of the year
table(bank_data$month)

# number of contacts performed during this campaign and for this client (numeric, includes last contact)
table(bank_data$campaign)

# - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
table(bank_data$poutcome)

## related with the last contact of the current campaign:
table(bank_data$contact)

########## Correlation Analysis & Matrix ############

#Threshold for correlation

#Cohen's rule of thumb (if data are normally distributed)
# r = 0.1 weak
# r = 0.3 medium
# r = 0.5 strong

# Build Correlation Matrix structure:
bank_data %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
    corrplot(type = "upper", insig = "blank", diag = FALSE, addCoef.col = "grey")
#duration appears to have a moderate relationship to the response variable
#previous and pdays appear to have a moderate linear relationshp with each other

#re-build as matrix
corr_matrix <- bank_data %>% 
  select_if(is.numeric) %>% 
  cor() 

round(corr_matrix,2)

####Observations


# -We see that More managerial professionals responded to the campaign when compared to other professionals.
tab <- table(bank_data$target)
prop.table(tab) #12% response rate
# BUT managers made up 25% of respondents
addmargins(table(bank_data$job, bank_data$target))

# 2-Way Cross Tabulation
library(gmodels)
CrossTable(bank_data$job, bank_data$target, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#students & retireds response rate much higher than
#the others


# -We see more married individuals responded to the campaign 52%
CrossTable(bank_data$marital, bank_data$target, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#-Also high among singles 36%

#Not Having a housing loan
CrossTable(bank_data$housing, bank_data$target, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#-Respondents who didn't already have a house loan were higher to respond (63%
#vs 37% of those who already had a housing loan)


#Not Having a personal loan
CrossTable(bank_data$loan, bank_data$target, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
##-Respondents who didn't already have a personal loan were higher to respond
##(91% had no prior personal loan)


#######ML Model 1: Logistic Regression: Split dataset into development (train) and holdout (validation or test) sets#######
glimpse(bank_data$target)

#factor target variable if needed
bank_data$target <- as.factor(bank_data$target)

#check results
glimpse(bank_data$target)

#import library and set seed for reproducibility
library(caTools)
set.seed(123)

## split the dataset into training and test samples at 70:30 ratio
split <- sample.split(bank_data$target, SplitRatio = 0.7)
train_data <- subset(bank_data, split == TRUE)
test_data <- subset(bank_data, split == FALSE)

## Check if distribution of partition data is correct for the development dataset
prop.table(table(train_data$target))
prop.table(table(test_data$target)) 

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
#not running with month or day variables
model_01_logreg <- glm(target ~ . - month -day, 
                family = binomial, data = train_data)

# Take a look at the model
summary(model_01_logreg)

# Take a look at the odds
coefsexp <- coef(model_01_logreg) %>% exp()%>% round(2)
coefsexp

# Odds ratio, represents which group has better odds of success,
# and it’s given by calculating the ratio of odds for each group
# If we know the coefficients of independent variables Xs and the intercept a, we can predict the probability. 

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
vif(model_01_logreg)

# Feature (x) variables with a VIF value above 5 indicate high degree of
# multi-collinearity.

#multicollinearity does not bias coefficients; it inflates their standard errors.
# multicollinearity does not usually alter the
#interpretation of the coefficients of interest
#unless they lose statistical significance.

#Rule of thumb for interpreting VIF value
#if VIF of a variable is one, it means that it is
#not correlated with any of the other variables.

#Job has an extremely high VIF, we will remove it from our model and run log regression again
#to compare

model_02_logreg <- glm(target ~ . -month -day -job, 
                family = binomial, data = train_data)

# Take a look at the model
summary(model_02_logreg)

#check vif again
vif(model_02_logreg)
#No extremely high VIF's so removing job improved the model


#Let's run again with month and day and see what happens
model_03_logreg <- glm(target ~ .-job, 
                family = binomial, data = train_data)

# Take a look at the model
summary(model_03_logreg)

#check vif again
vif(model_03_logreg)

#Suggests that the day of the contact makes
#a positive difference for the target variable

#Some months are significantly positive
# - mar jun dec sep oct dec

#Months significantly negative (feb barely significant)
# - jan may jul aug nov

#Let's compare the models we've run so far
anova(model_01_logreg, model_02_logreg, model_03_logreg, test="Chisq")
#Model 1 was not significant but models 2 and 3 are significant
# at 95%. Model 3 had the lowest residuals.

library(lmtest)
lrtest(model_02_logreg, model_03_logreg)
#According to likeihood ratio test, model 3 is significantly better


########## Step 2: Overall significance of the model############################## 

#Compare our best logistic regression model against null model
lrtest(model_03_logreg)

# We can see that the low p value indicates the model is highly significant i.e.
# the likelihood of a customer responding to the campaign (TARGET) depends on
# independent x variables in the dataset.



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
#   reasonable if > 0.2
#   good if > 0.4
#   very good if > 0.5

##Goodness of fit packages & measures
library(pscl)
model_fit <- pR2(model_03_logreg)
print(model_fit)

#retrieve just the mcfadden r2 from model_fit
model_fit[4]

#pass the Mcfadden R2 into the function to get fit estimate
pseudoR_func <- function(pseudoR){
  if(pseudoR >= 0.5){print("very good fit")}
  else if(pseudoR >= 0.4 && pseudoR < 0.5) {print("good fit")}
  else if (pseudoR >= 0.2 && pseudoR < 0.4) {print("reasonable fit")}
  else{print("try again")}
}

pseudoR_func(model_fit[4])

#Albeit not perfect the fit is reasonable

########## Step 4: Individual coefficients significance and interpretation############################# 

#library(coef.lmList)
summary(model_03_logreg)

# The Odds Ratio and Probability of each x variable is calculated based on the
# formulae, Odds Ratio = exp(Co-efficient estimate) Probability = Odds Ratio /
# (1 + Odds Ratio)


#plot coefficients on odds ratio
library(sjPlot)
plot_model(model_03_logreg)


######Step 5: Model Accuracy#################

pred <- predict(model_03_logreg, test_data, type = "response") #predict using test data

#check results
head(pred)

predicted <- round(pred) #>0.5 will convert to 1

#contingency table
contingency_tab <- table(test_data$target, predicted)
contingency_tab

#sum the diagnals for the percentage accuracy classified
sum(diag(contingency_tab))/sum(contingency_tab) *100

# Confusion Matrix using the caret package to validate above
caret::confusionMatrix(contingency_tab)

#Our model leads to approx 90% of correct predictions

######### Step 6: Model Diagnostics - ROC/AUC/K-Fold CV #################

#Plot ROC Curve & Calculate AUC area
library(ROCR)

#ROC Curves are useful for comparing classifiers

#check data structures first
typeof(predicted)
typeof(test_data$target)


pr <- prediction(pred, test_data$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)

#The ideal ROC curve hugs the top left corner, indicating a high
#true positive rate and a low false positive rate.
#True positive rate on y-axis
#False positive rate on the x-axis

#The larger the AUC, the better the classifier
#The AUC aline is insufficient to identify a best model
#It's used in combination with qualitative examination
#of the ROC curve
auc <- performance(pr, measure = "auc")
auc

# AUC is 0.9074364
as.numeric(performance(pr, measure = "auc")@y.values)

#AUC Interpretation
#A: Outstanding = 0.9 to 1.0
#B: Excellent/Good = 0.8 to 0.9
#C: Acceptable/Fair = 0.7 to 0.8
#D: Poor = 0.6 to 0.7
#E: No Discrimination = 0.5 to 0.6


#now let's just plot the ROC and look at true positive vs false positive
perf <- performance (pr, measure = 'tpr', x.measure = "fpr")
plot(perf) + abline(a=0, b=1, col = 'red') # the red line is randomness

## Cross-validation
library(C50)
library(irr)

#create 10 folds
set.seed(123)
folds <- createFolds(bank_data$target, k = 10)

#peek at the results
str(folds)

######### Step 7: Variable Importance #################
#Let's look at the abasolute value of the t-statistic for
#each model parameter using caret package
varImp(model_03_logreg)

#########Summary of Conclusions from Log Regression Model#################

# Positive correlation - should contact customers with these characteristics
# - has a balance
# -education - tertiary or secondary education
# -if a student
# or retired
#  -being single
# if the customer had responded to a prior campaign before
# 
# Negative correlation - do not contact customers with these characteristics
# -if customer has a personal loan
#- if customer has a mortgage loan
#- some professions housemade, entrepreneur
#-also if has credit currently in default with the bank
#- number of contacts

#The bank should limit the number of contacts it has with
# a customer. What's most likely happening is that customers
#may ignore communication from the bank if its gets too many.




############ML Model 2: Decision Tree ###########

str(bank_data)

#######Step 1: Train/Test Split dataset###################

#recode target variable to classification variable
str(bank_data$target)
bank_data$target <- as.factor(bank_data$target)
str(bank_data$target) #check results

# Step 1: Get total number of rows in the data frame
n <- nrow(bank_data)

# Step 2: Number of rows for the training set (80% of the dataset)
n_train <- round(.80 * n) 

# Step 3: get a vector of indexes of a random sample. set a seed to get reproducible results
#In this case, Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)

## Step 3: Subset the data for the training indices only
# Subset the data frame to training indices only
bank_train <- bank_data[train_indices, ]  
  
# Step 4: Exclude the training indices to create the test set
bank_test <- bank_data[-train_indices, ]  

#######Step 2: Build the decision tree ###################

#train the classification tree model
tree_bank <- tree(target ~ .-job, data = bank_train)

tree_bank

par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(tree_bank)
text(tree_bank)

#Duration < 364.5 is the first variable split in the tree; however, after reading
#the documentation about this variable, perhaps should build a tree without this variable

#how to read the tree # duration: with duration -
# last contact duration, in seconds (numeric). Important note: this
# attribute highly affects the output target (e.g., if duration=0 then y='no').
# Yet, the duration is not known before a call is performed. Also, after the end
# of the call y is obviously known. Thus, this input should only be included for
# benchmark purposes and should be discarded if the intention is to have a
# realistic predictive model.


#tree without duration
# pdays: number of days that passed by after the client was last contacted from
# a previous campaign (numeric; 999 means client was not previously contacted)
#


#re-build tree without duration variable
tree_bank_nodur <- tree(target~. -job -duration, data = bank_train)

# The summary() function lists the variables that are used as internal nodes in
# the tree, the number of terminal nodes, and the (training) error rate.
summary(tree_bank_nodur)

# We use the plot() function to display the tree structure, and the text()
# function to display the node labels. The argument pretty=0 instructs R to
# include the category names for any qualitative predictors, rather than simply
# displaying a letter for each category.

par(mfrow=c(1,1)) #reset plot parameters
#run dev.off() a few times, and then try to plot again.
#Run options(device = "RStudioGD"), and try to plot again.
#dev.off() 

#plot the new tree
plot(tree_bank_nodur)
text(tree_bank_nodur, pretty=0)


# In order to properly evaluate the performance of a classification tree on
# these data, we must estimate the test error rather than simply computing the
# training error. We split the observations into a training set and a test set,
# build the tree using the training set, and evaluate its performance on the
# test data. The predict() function can be used for this purpose.


library(rpart)
library(rpart.plot)
#Let's use rpart package
#train the classification tree model
class_tree <- rpart(target~. -job, data = bank_train, method="class")

# Draw the decision tree
fancyRpartPlot(class_tree)

#tree with no duration
class_tree_b<- rpart(target ~ . -job -duration, data = bank_train, method="class")

# Draw the decision tree
fancyRpartPlot(class_tree_b)


################Step 3 : Evaluate Model Performance & Accuracy #############

# Generate predicted classes using the model object
class_prediction <- predict(object = tree_bank,  
                        newdata = bank_test,   
                        type = "class")  
                            
# Calculate the confusion matrix for the test set
confusionMatrix(data = class_prediction,       
                reference = bank_test$target) 

#The data argument takes a vector of predicted class labels on a test set
#the reference argument a vector of the true class labels

#####RESULTS: Decision tree is 89% accurate

# Next, we consider whether pruning the tree might lead to improved results. The
# function cv.tree() performs cross-validation in order to cv.tree() determine
# the optimal level of tree complexity; cost complexity pruning is used in order
# to select a sequence of trees for consideration. We use the argument
# FUN=prune.misclass in order to indicate that we want the classification error
# rate to guide the cross-validation and pruning process, rather than the
# default for the cv.tree() function, which is deviance. The cv.tree() function
# reports the number of terminal nodes of each tree considered (size) as well as
# the corresponding error rate and the value of the cost-complexity parameter
# used (k, which corresponds to α in (8.4)).

set.seed(3)
cv_tree_bank<- cv.tree(tree_bank,FUN=prune.misclass )
names(cv_tree_bank)

cv_tree_bank

# Note that, despite the name, dev corresponds to the cross-validation error
# rate in this instance. The tree with 1 terminal nodes results in the lowest
# cross-validation error rate, with 32 cross-validation errors. We plot the
# error rate as a function of both size and k.

#par(mfrow=c(1,2))
par(mar=c(1,1,1,1))
plot(cv_tree_bank$size, cv_tree_bank$dev,type="b")

plot(cv_tree_bank$k, cv_tree_bank$dev,type="b")

#The tree with either 10 or 5 terminal modes results in the lowest cross-validatione error rate
#with 3944 cross-validation errors.

##############Step 4: Prune Tree###################

# We now apply the prune.misclass() function in order to prune the tree to prune.
# obtain the 5th
prune_tree_bank<- prune.misclass (tree_bank, best=5)
par(mar=c(1,1,1,1))
plot(prune_tree_bank)
text(prune_tree_bank,pretty=0)

#let's try again with 10
prune_tree_bank<- prune.misclass (tree_bank, best=10)
par(mar=c(1,1,1,1))
plot(prune_tree_bank)
text(prune_tree_bank,pretty=0)




#########Summary of Insights from Tree Model#################

#without duration variable, poutcome 
# becomes the most
#important variable,
#if customer has a mortgage loan

######## Final conclusion ##########


#Strongly believe that the logistic regression model
#offers better insights and performance than the tree model for marketing
#purposes.
