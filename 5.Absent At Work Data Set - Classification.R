############ Absenteeism at work data Set ################

# Abstract: The database was created with records of absenteeism at work from
# July 2007 to July 2010 at a courier company in Brazil.

#Can we predict who will miss work?

browseURL("https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work")


###### Data set Information ############
# Attribute Information:
# 
# 1. Individual identification (ID)
# 2. Reason for absence (ICD).
# Absences attested by the International Code of Diseases (ICD) stratified into 21 categories (I to XXI) as follows:
# 
# I Certain infectious and parasitic diseases
# II Neoplasms
# III Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism
# IV Endocrine, nutritional and metabolic diseases
# V Mental and behavioural disorders
# VI Diseases of the nervous system
# VII Diseases of the eye and adnexa
# VIII Diseases of the ear and mastoid process
# IX Diseases of the circulatory system
# X Diseases of the respiratory system
# XI Diseases of the digestive system
# XII Diseases of the skin and subcutaneous tissue
# XIII Diseases of the musculoskeletal system and connective tissue
# XIV Diseases of the genitourinary system
# XV Pregnancy, childbirth and the puerperium
# XVI Certain conditions originating in the perinatal period
# XVII Congenital malformations, deformations and chromosomal abnormalities
# XVIII Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
# XIX Injury, poisoning and certain other consequences of external causes
# XX External causes of morbidity and mortality
# XXI Factors influencing health status and contact with health services.
# 
# And 7 categories without (CID) patient follow-up (22), medical consultation (23), blood donation (24), laboratory examination (25), unjustified absence (26), physiotherapy (27), dental consultation (28).
# 3. Month of absence
# 4. Day of the week (Monday (2), Tuesday (3), Wednesday (4), Thursday (5), Friday (6))
# 5. Seasons (summer (1), autumn (2), winter (3), spring (4))
# 6. Transportation expense
# 7. Distance from Residence to Work (kilometers)
# 8. Service time
# 9. Age
# 10. Work load Average/day
# 11. Hit target
# 12. Disciplinary failure (yes=1; no=0)
# 13. Education (high school (1), graduate (2), postgraduate (3), master and doctor (4))
# 14. Son (number of children)
# 15. Social drinker (yes=1; no=0)
# 16. Social smoker (yes=1; no=0)
# 17. Pet (number of pet)
# 18. Weight
# 19. Height
# 20. Body mass index
# 21. Absenteeism time in hours (target)


###### Load libraries ###########

library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(cowplot)
library(GGally)
library(VIM)
library(gmodels)


######### Import Data ###################

getwd() #check working directory

#change wd
setwd("/Users/anitaowens/R_Programming/UCI Machine Learning Repo") #change working directory

getwd() #check working directory


df<- read.csv("Absenteeism_at_work_AAA/Absenteeism_at_work.csv", sep = ';')
head(df,2)
glimpse(df)

########### Check Data #################

#summarize variables in dataframe
summary(df)

######### Any Missing Data #############

## Show cases with missing values
df[!complete.cases(df),] #no missing

#using aggr function from VIM packages --missing values are in red
aggr(df, prop = F, numbers = T)

dim(df)

#No missing data to worry about


####### Data transformation ###############

#we need to rename some of the columns to make the names shorter
# and to have a consistent naming structure
df <- rename(df,
                        absent_reason = Reason.for.absence,
                        month = Month.of.absence,
                        weekday = Day.of.the.week,
                        seasons = Seasons,
                        transport_exp = Transportation.expense,
                        dist_to_work = Distance.from.Residence.to.Work,
                        service_time = Service.time,
                        age = Age,
                        avg_workload = Work.load.Average.day,
                        hit_target = Hit.target,
                        discip_fail = Disciplinary.failure,
                        educ = Education,
                        num_child = Son,
                        drinks = Social.drinker,
                        smokes = Social.smoker,
                        pet = Pet,
                        wgt = Weight,
                        hgt = Height,
                        bmi = Body.mass.index,
                        hrs_absent = Absenteeism.time.in.hours)
 
#check results
names(df)

######### Explore Data: Correlation Matrix & Pairwise Scatterplot ##############

describe(df)
#we have lots of different measures in the dataset
#will need to be re-scaled before building a model

#Do we need to subset numeric variables for
#correlation matrix?
str(df)

#No, but we do need to remove the id column before 
#building the matrix as it is not needed
df <-df[-1]

# Build Correlation Matrix structure for plotting:
#remove correlation coefficients from plot
df %>% 
  cor() %>% 
    corrplot(type = "upper", insig = "blank", diag = FALSE)#, addCoef.col = "grey")

#coefficients for correlation matrix
res <- cor(df)
round(res, 2)

# create a pairwise scatterplot - 
 # df%>%
 #  ggpairs()

######## Insights from Correlation Matrix ###########

 #Linear relationship strength - interpretation
 # 1 - perfect
 # .8 - 1 - very strong
 # .6 - .8 - strong
 # .4 - .6 - moderate
 # .2 - .4 - weak
 # 0 - .2 none to extremely weak


#Threshold for correlation
#Cohen's rule of thumb (if data are normally distributed)
# r = 0.1 weak
# r = 0.3 medium
# r = 0.5 strong
 
#Service time and age seems to be positive strong correlation as
#well as BMI & weight which is expected
 
# Weight is also correlated with service time, age and being a 
# social drinker
 
#There are some interesting and unexpected high correlations
#between transportation expense with pet and son (number of children)
#could be a sign of multi-collinearity

#Will investigate later

 
##### Target variable ##############
 
# -hrs_absent is our target variable
# It is Absenteeism.time.in.hours
# -our dependent variable

#We want to know which factors affects the number of hours
#absent


########## Baseline hours absent & compare against independent variables #############

#Let's look at our target variable
describe(df$hrs_absent)
#The mean is 6.92 and the median is 3
#Right-skew

#Let's visualize in a histogram
ggplot(data = df, aes(x = hrs_absent))+
      geom_histogram(fill = "lightgreen", binwidth = 5, color = "black") +
      geom_vline(aes(xintercept = median(hrs_absent)), color = "red", linetype = "dashed") +
  labs(title= "Hours absent variable does not\nfollow a normal distribution",
    x = "hours absent") +
  theme(legend.position="none")
#We have some extreme values on the high end but it's not that many 


ggplot(data = df, aes(x = hrs_absent))+
      geom_histogram(fill = "lightgreen", binwidth = 5, color = "black") +
      geom_vline(aes(xintercept = median(hrs_absent)), color = "red", linetype = "dashed") +
  labs(title= "Right-skew",
    x = "hours absent") +
  theme(legend.position="none")

#Which measure of central tendency should we use as our benchmark?
#Due to the skew - i would normally go for median, but in this case
#I will use average since I want to see the influence of the extreme values

mean_rate  <- 6.92

#data frame grouped by absence reason
absent_reason.df <- df %>% 
  group_by(absent_reason) %>% 
  summarize(total_count = n(),
    total_hrs_absent = sum(hrs_absent)) %>% 
  mutate(avg_absence_rate = total_hrs_absent/total_count)

absent_reason.df #check results

#check conversions colum
sum(absent_reason.df$total_hrs_absent) 
sum(absent_reason.df$total_count)

#add highlight flag column
absent_reason.df <- absent_reason.df %>% 
mutate(highlight_flag =
    ifelse(avg_absence_rate > mean_rate, 1, 0))

#check results
head(absent_reason.df$highlight_flag)

#plot avg_absence_rate by absent_reason
ggplot(data=absent_reason.df, aes(x=reorder(absent_reason, avg_absence_rate), y=avg_absence_rate,
  fill = factor(highlight_flag))) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=mean_rate, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip() +
    scale_fill_manual(values = c('#595959', 'red')) +
  labs(x = ' ', y = 'Absence Hours', title = str_c("Absence in hours by reason")) +
  theme(legend.position = "none")

#Reason 9 - Endocrine, nutritional and metabolic diseases
#has the highest avg number of hours missed than any other group
#however, there were only 4 cases that caused this. Followed by
#reason 2 - Neoplasms (1 case), and reason 12 Diseases of the skin and 
# subcutaneous tissue (8 total cases).



#Let's take a closer look at the absolute counts
View(absent_reason.df)

#Reason 23 has the highest number of absences in absolute numbers 149
#but the avg number of absent hours is below the avg at 2.48 hours.
#Reason 23 is medical consultation so it would make sense to have high number
#of participants and time off from work would be relatively short.

#Reason 28 dental consultation is the 2nd highest reason for missed hours in
#absolute hours

#Reason 27 - physiotherapy

#Reason 13 Diseases of the musculoskeletal system and connective tissue
# is the 4th highest in absolute number of workers but also has the highest
#number of missed hours out of all reasons while reason 19 - XIX Injury, 
# poisoning and certain other consequences of external causes
#has the 2nd highest number of missed hours


#Are the reasons and hours absent variables statistically significant
#The patterns we see are not caused by random chance

#Hypothesis test - Let's use t-test
ttest <- t.test(df$hrs_absent, df$absent_reason)
ttest

p_value <- ttest$p.value
p_value < .05 #True
#the p-value is tiny thus highly significant!

#Since the distribution of hours absent wasn't entirely
#normal let's also try a wilcox.test

wilcox <- wilcox.test(df$hrs_absent, df$absent_reason)
wilcox

p_value_wilcox <- wilcox$p.value
p_value_wilcox < .05
#We also get a significant result

########### Summary of Insights for absent reason ##############

# It would seem that higher than avg number of hours absent from work
# are caused by outliers (small groups of workers with uncommon medical conditions)
# While routine medical appointments & reasons cause the highest number
# of workers who miss hours but the avg time missed is relatively short
# (3 hours or less)


####### Data transformation ##############

#######ML Model: Regression - To establish Baseline and Relationship#######

glimpse(df)

linear_model_01 <- lm(hrs_absent ~ ., data = df)

coef(linear_model_01)

summary(linear_model_01)

#Let's visualize the coefficients
library(coefplot)
coefplot(linear_model_01, intercept=FALSE,
  ylab = "Features",
  xlab = "Association with
  Hours Absent")

