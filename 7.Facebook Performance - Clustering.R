############Facebook Performance Metrics ################

# The full dataset was described and analyzed in:
# 
#    S. Moro, P. Rita and B. Vala. Predicting social media performance metrics and evaluation of the impact on 
#    brand building: A data mining approach. Journal of Business Research, Elsevier, In press, Available online 
#    since 28 February 2016.

#browseURL("https://archive.ics.uci.edu/ml/datasets/Facebook+metrics")


###### Data Description ###########

# The data is related to posts' published during the year of 2014 on the
# Facebook's page of a renowned cosmetics brand. This dataset contains 500 of
# the 790 rows and part of the features analyzed by Moro et al. (2016). The
# remaining were omitted due to confidentiality issues.

###### Load libraries ###########

library(rpart)
library(psych)
library(dplyr)
library(corrplot)
library(ggplot2)
library(cowplot)
library(GGally)
library(VIM)
library(gmodels)
library(cluster)
library(mclust)
library(car)

######### Custom functions #########

#this function will help us inspect cluster solutions more efficiently later on
seg_summ_function <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}


######### Import Data ###################

#import csv file
fb_data <- read.csv("~/Documents/GitHub/Machine-Learning-R/Machine-Learning-R/datasets/dataset_Facebook.csv", sep = ';')


#check results of data import
str(fb_data)

########### Check Data #################

#summarize variables in dataframe
summary(fb_data)

#let's look at the post type
table(fb_data$Type)

#let's look at the category
table(fb_data$Category)

#let's make a cross-table of type and category
table(fb_data$Category, fb_data$Type)

#check column names - some of them are quite long
names(fb_data)


#we need to rename some of the columns to make the names shorter
fb_data <- rename(fb_data,
                        page_likes = Page.total.likes,
                        reach = Lifetime.Post.Total.Reach,
                        impressions = Lifetime.Post.Total.Impressions,
                        engaged_users = Lifetime.Engaged.Users,
                        consumers = Lifetime.Post.Consumers,
                        consumptions = Lifetime.Post.Consumptions,
                        liked_impr = Lifetime.Post.Impressions.by.people.who.have.liked.your.Page,
                        liked_reach = Lifetime.Post.reach.by.people.who.like.your.Page,
                        liked_engaged = Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,
                        interactions = Total.Interactions)
 
#check results
names(fb_data)

########## Any Missing Data #############

## Show cases with missing values
fb_data[!complete.cases(fb_data),] #we get 5 rows that have missing data

#using aggr function from VIM packages --missing values are in red
aggr(fb_data, prop = F, numbers = T)

#let's remove the missing rows
dim(fb_data) #500 rows of data
fb_data <- fb_data[complete.cases(fb_data),]
dim(fb_data) #now we have 495 rows of data

###### Explore Data: Data visualization ##############

##### Univariate Analysis: Barplots for important categorical variables (x) ######

# Don't map a variable to y
bar1 <- ggplot(fb_data, aes(x=factor(Type)))+
  geom_bar(stat="count", width=0.7, fill="sienna1")+
  theme_minimal()

bar2 <- ggplot(fb_data, aes(x=factor(Category)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

bar3 <- ggplot(fb_data, aes(x=factor(Post.Month)))+
  geom_bar(stat="count", width=0.7, fill="salmon4")+
  theme_minimal()

bar4 <- ggplot(fb_data, aes(x=factor(Post.Weekday)))+
  geom_bar(stat="count", width=0.7, fill="red4")+
  theme_minimal()

plot_grid(bar1, bar2, bar3, bar4, labels = "AUTO")

#####Univariate Analysis: Boxplots for important independent numeric variables  ######

# page_likes on y axis
ggbi1 <- ggplot(data = fb_data, aes(x = Type, y = page_likes, fill = Type))+geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red")
ggbi2 <- ggplot(data = fb_data, aes(x = factor(Category), y = page_likes, fill = Category))+geom_boxplot()
ggbi3 <- ggplot(data = fb_data, aes(x = Post.Month, y = page_likes, fill = Post.Month))+geom_boxplot()
ggbi4 <- ggplot(data = fb_data, aes(x = factor(Post.Weekday), y = page_likes, fill = Post.Weekday))+geom_boxplot()
ggbi5 <- ggplot(data = fb_data, aes(x = factor(Post.Hour), y = page_likes, fill = Post.Hour))+geom_boxplot()
ggbi6 <- ggplot(data = fb_data, aes(x = factor(Paid), y = page_likes, fill = Paid))+geom_boxplot()

plot_grid(ggbi1, ggbi2, ggbi3, ggbi4, labels = "AUTO")
plot_grid(ggbi5, ggbi6, labels = "AUTO")

###### Univariate Analysis: Histogram plots for important independent numeric variables##

ggplot(data=fb_data, aes(x=reach)) + geom_histogram()


hist1 <-  ggplot(data = fb_data, aes(x = reach))+
      geom_histogram(fill = "lightblue", binwidth = 25, colour = "black") +
      geom_vline(aes(xintercept = median(reach), linetype = "dashed"))


hist2 <- ggplot(data = fb_data, aes(x = impressions))+
      geom_histogram(fill = "blue", colour = "black") +
      geom_vline(aes(xintercept = median(impressions)), linetype = "dashed")


hist3 <- ggplot(data = fb_data, aes(x = engaged_users))+
      geom_histogram(fill = "red", binwidth = 3, colour = "black") +
      geom_vline(aes(xintercept = median(engaged_users)), linetype = "dashed")

plot_grid(hist1, hist2, hist3, labels = "AUTO")

##########Bivariate analysis: Boxplots for TYPE (as x variable) vs important numeric (y variables) ##########

bi1 <- ggplot(data = fb_data, aes(x = Type, y = page_likes, fill = Type))+geom_boxplot()
bi2 <- ggplot(data = fb_data, aes(x = Type, y = reach, fill = Type))+geom_boxplot()
bi3 <- ggplot(data = fb_data, aes(x = Type, y = impressions, fill = Type))+geom_boxplot()
bi4 <- ggplot(data = fb_data, aes(x = Type, y = engaged_users, fill = Type))+geom_boxplot()

plot_grid(bi1, bi2, bi3, bi4, labels = "AUTO")

bi5 <- ggplot(data = fb_data, aes(x = Type, y = consumers, fill = Type))+geom_boxplot()
bi6 <- ggplot(data = fb_data, aes(x = Type, y = consumptions, fill = Type))+geom_boxplot()
bi7 <- ggplot(data = fb_data, aes(x = Type, y = liked_impr, fill = Type))+geom_boxplot()
bi8 <- ggplot(data = fb_data, aes(x = Type, y = liked_reach, fill = Type))+geom_boxplot()
bi9 <- ggplot(data = fb_data, aes(x = Type, y = liked_engaged, fill = Type))+geom_boxplot()

plot_grid(bi5, bi6, bi7, bi8, bi9, labels = "AUTO")

bi10 <- ggplot(data = fb_data, aes(x = Type, y = comment, fill = Type))+geom_boxplot()
bi11 <- ggplot(data = fb_data, aes(x = Type, y = like, fill = Type))+geom_boxplot()
bi12 <- ggplot(data = fb_data, aes(x = Type, y = share, fill = Type))+geom_boxplot()
bi13 <- ggplot(data = fb_data, aes(x = Type, y = interactions, fill = Type))+geom_boxplot()

plot_grid(bi10, bi11, bi12, bi13, labels = "AUTO")

##########Bivariate analysis: Boxplots for PAID (as x variable) vs important numeric (y variables) ##########

bipd1 <- ggplot(data = fb_data, aes(x = factor(Paid), y = page_likes, fill = factor(Paid)))+geom_boxplot()
bipd2 <- ggplot(data = fb_data, aes(x = factor(Paid), y = reach, fill = factor(Paid)))+geom_boxplot()
bipd3 <- ggplot(data = fb_data, aes(x = factor(Paid), y = impressions, fill = factor(Paid)))+geom_boxplot()
bipd4 <- ggplot(data = fb_data, aes(x = factor(Paid), y = engaged_users, fill = factor(Paid)))+geom_boxplot()

plot_grid(bipd1, bipd2, bipd3, bipd4, labels = "AUTO")

#very little difference between paid and unpaid

bipd5 <- ggplot(data = fb_data, aes(x = factor(Paid), y = consumers, fill = factor(Paid)))+geom_boxplot()
bipd6 <- ggplot(data = fb_data, aes(x = factor(Paid), y = consumptions, fill = factor(Paid)))+geom_boxplot()
bipd7 <- ggplot(data = fb_data, aes(x = factor(Paid), y = liked_impr, fill = factor(Paid)))+geom_boxplot()
bipd8 <- ggplot(data = fb_data, aes(x = factor(Paid), y = liked_reach, fill = factor(Paid)))+geom_boxplot()
bipd9 <- ggplot(data = fb_data, aes(x = factor(Paid), y = liked_engaged, fill = factor(Paid)))+geom_boxplot()

plot_grid(bipd5, bipd6, bipd7, bipd8, bipd9, labels = "AUTO")

bipd10 <- ggplot(data = fb_data, aes(x = factor(Paid), y = comment, fill = factor(Paid)))+geom_boxplot()
bipd11 <- ggplot(data = fb_data, aes(x = factor(Paid), y = like, fill = factor(Paid)))+geom_boxplot()
bipd12 <- ggplot(data = fb_data, aes(x = factor(Paid), y = share, fill = factor(Paid)))+geom_boxplot()
bipd13 <- ggplot(data = fb_data, aes(x = factor(Paid), y = interactions, fill = factor(Paid)))+geom_boxplot()

plot_grid(bipd10, bipd11, bipd12, bipd13, labels = "AUTO")

############### 2-Way Cross Tabulation Tables to compare categorical variables ################

#Type
CrossTable(fb_data$Post.Month, fb_data$Type, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#85% of posts were photos - by far, followed by status posts
#Oct & July are the busiest months when it comes to posts

CrossTable(fb_data$Post.Weekday, fb_data$Type, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

#same as above just flipped
CrossTable(fb_data$Type, fb_data$Post.Weekday, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)
#32% of photos are posted on the weekends


CrossTable(fb_data$Paid, fb_data$Type, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

CrossTable(fb_data$Paid, fb_data$Category, digits=2, prop.c = TRUE,
  prop.r = TRUE, prop.t = FALSE, chisq = FALSE, format = "SAS", expected = FALSE)

############Exploring Associations between continuous variables with scatterplots####

ggplot(data = fb_data, aes(x = reach))+
      geom_histogram(fill = "lightblue", binwidth = 25, colour = "black") +
      geom_vline(aes(xintercept = median(reach), linetype = "dashed"))

ggplot(fb_data, aes(page_likes, reach)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(fb_data, aes(page_likes, impressions)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(fb_data, aes(comment, page_likes)) + 
  geom_point()+
  geom_smooth(method=lm)


######### Explore Data: Correlation Matrix & Pairwise Scatterplot ##############


#subset dataframe to get just all numeric variables
num_vars<- fb_data %>% 
        select_if(is.numeric)
head(num_vars)



# Build Correlation Matrix structure:
num_vars %>% 
  cor() %>% 
    corrplot(type = "upper", insig = "blank", diag = FALSE, addCoef.col = "grey")

# create a pairwise scatterplot - too many variables to plot at once
# fb_data%>%
#   ggpairs()

cor(num_vars)

##Insights from correlation matrix
# -lots of extreme values for certain variables
# -highly correlated variables - share, likes, interactions


#Establish a baseline with linear regression
linmod <- lm(page_likes ~ ., data = fb_data)
summary(linmod)

#1 coefficient not defined because of singularities
#We need to address collinearity. Many significant variables.
alias(lm(page_likes ~ ., data = fb_data))
#interactions is highly correlated

#remove interactions from the model
linmodnoInt <- lm(page_likes ~ . -interactions, data = fb_data)
summary(linmodnoInt)

#Let's calcualte the VIF
vif(linmodnoInt)

#very high VIF's for:
# reach
# impressions
# engaged_users
# consumers
# liked_imp
# liked_reach
# liked_engaged
# commment - slightly high, but okay perhaps
# like
# share

linmodnocol <- lm(page_likes ~ Type + Category + Post.Month +
  Post.Weekday + Post.Hour + Paid + consumptions, data = fb_data)

summary(linmodnocol)

#Let's calcualte the VIF
vif(linmodnocol) # - much better no collinear variables

####################### Target variable for modeling ####################

#What is the question we're trying to answer?: 
#----Have we successfully built our brand on Facebook?

#Have our activities been successful? This is a study of user behavior that will
# most likely be shared with management or advertising agencies.


#Let's look at the current definitions of our KPI's.

# 1. impressions: impressions measure the number of times your posts were seen.
# That includes if one post was seen multiple times by a single user. 

# 2. Page Likes: are the number of people that follow your brand on Facebook. They liked
# your page or opted-in to be able to have your posts show up in their feed. You
# can think of them as fans or subscribers.  Page Likes show your audience size on Facebook.
# Over time, that number should be growing.

# 3. Reach: Reach is an indication of the number of people who see your material on Facebook.

# 4. Engaged Users: The number of people who engaged with your Page. Engagement includes any
# click or story created.

# 5. Page Consumptions:  Total number of clicks

# 6. Interactions: The number of interactions (reactions, comments, shares)
# during a selected time range. The interactions are shown on the day the post
# they relate to was published.


# -What posts are most likely to be engaging?
# -What are the characteristics of an engaging post?
# - Are there any characteristics we can use to gain reach
# & therefore build our brand?


#What is our target variable for our modeling?

# - we will use page likes as our target kpi >> engagement with brand
# - we will also use interactions as our 2ndary kpi >> behavior customer
# - will use reach as a 3rd kpi

#try a new baseline model with the addt'l variables we removed earlier
#we want to add reach and interactions
linmodImproved <- lm(page_likes ~ Type + Category + Post.Month +
  Post.Weekday + Post.Hour + Paid + consumptions + reach + interactions, data = fb_data)

summary(linmodImproved)

#Let's calcualte the VIF
vif(linmodImproved) # - much better no collinear variables



########### ML Method 1: Hclust ####################

########### Step 1: Transform data for Hclust model####################

#Cannot assume that factor variables are irrelevant
#to our cluster definitions; it is better to use all data
#the daisy function in the cluster package works with mixing
#data types by rescaling the values, so we can use euclidean distance

dataset <- fb_data %>% 
          select(Type, Category, Post.Month, 
            Post.Weekday, Post.Hour, Paid,
            consumptions, reach, interactions, page_likes)

str(dataset)

#scale data using daisy function
fb_data_dist <- daisy(dataset) #works with mixed data types if we wanted to keep categorical variables in the dataset

#select first few rows/columns to check if scaling worked
as.matrix(fb_data_dist) [1:5, 1:5]

#the distances look reasonable(zeroes on the diagnol, symmetric, scaled[0,1])
#so we proceed to the hiearchical cluster method itself, invoking hclust on the
#dissimilarity matrix


########### Step 2: Apply Hclust Algorithm -####################

fb_data_hc <- hclust(fb_data_dist, method = "complete")

#draw cluster object
plot(fb_data_hc)

# using dendrogram objects
hcd <- as.dendrogram(fb_data_hc)
#str(hcd) #check results

# alternative way to get a dendrogram
plot(hcd)

#Dendogram is hard to read

##### Step 3: Hclust Goodnes-of-fit #########

#Finally we check for goodness of fit for a hierarchical clustering solution
#method 1: cophenetic correlation coefficient (CPCC) which assesses how well
#a dendogram matches the true distance metric.

#Use cophenetic() to get the distances from the dendogram and compare it to the dist()
#metrics with cor()

cor(cophenetic(fb_data_hc), fb_data_dist)

# CPCC is interpreted similarly to Pearson's r. A CPCC > 0.6 indicates
# a relatively strong fit, meaning the hierarchical tree represents the distances between
# observations well. .60-.80 a strong relationship

# Cor = 0.5381611
# However, in our case, the relationship isn't so strong,
# this suggests a medium fit!


#We can see where the dendogram would be cut
#by overlaying its plot with rect.hclust(), specifying
#the number of groups we want (k=...)
#the dendogram suggests possibly 5 clusters

plot(fb_data_hc)
rect.hclust(fb_data_hc, k=5, border="red")

#We obtain the assignment vector for observations using cutree()
#Basically, this means we need to get the cluster assignments
fb_data_hc_segment <- cutree(fb_data_hc, k=5)
table(fb_data_hc_segment) #check assignments


#Groups 1 dominates the assignment, followed by
#groups 4 and 5, 3, & 2. The clusters are not well-balanced.
#group 2 is the smallest clusters at 38 observations.


#We will be able to answer the following questions:

# 1-are there obvious differences in group means?
# 2-does the differentiation point to some underlying story to tell?
# 3-do we see immediately odd results such as a mean equal to the value of
#   one data level?


#check results of cluster assignments against dataset using custom function
hclust_segs <-seg_summ_function(fb_data, fb_data_hc_segment)
hclust_segs


#Category variable
#1 - Link
#2 - Photo
#3 - Status
#4 - Video

table(fb_data$Paid)
#0-unpaid
#1-paid

table(fb_data$Type) #how was type
#variable recoded in clust algorithm
#1 - link
#2 - Photo
#3 - Status
#4 - Video

#Insights based on hclust ML algorithm:


#Group 4 stands out at having the most reach 
# type (2) post - photo that has the most reach
# and average interactions and next to highest
#consumptions and is an unpaid segment
#if we care about audience reach for very little
#money this segment should take care of that


# Group 1 dominates in number of observations. It's an
# unpaid segment with
# the most average page likes. Type is photo
# This indicates that posting photos goes a long way in
# engaging users with our FB page.


# Groups 3 stands out immediately as having
#the most interactions. Not really
# actionable here because of the number of observations

#All segments indicate that the day of the week
#the post lands on is important. Also that
#photos tend to dominate the type of post that helps
#us reach our branding goals.


######### ML Method 2: Mean-Based Clustering: kmeans() ############

#Only appropriate for numeric data or data that can be reasonably coerced to numeric.
#Unlike higher -order categorical variables, binary factors can be coerced to numeric with
#no alternation of meaning.

#Although it's not optimal to cluster binary/categorical values
#with k-means, 
#we might attempt it. 
#We just need to fix the type category

#1 - Link
#2 - Photo
#3 - Status
#4 - Video
  

  fb_data_num <- dataset %>% 
    mutate(type =
      case_when(
        Type == "Link" ~ 1,
        Type == "Photo" ~ 2,
        Type  == "Status" ~ 3,
        Type == "Video" ~ 4,
        TRUE ~ 5)
      )

#check results of case when statement
str(fb_data_num$type)
table(fb_data_num$type)

#change from numeric to int
fb_data_num$type <- as.integer(fb_data_num$type) 

#remove the type variable
fb_data_num <-  fb_data_num[,-1] 

#check structure
str(fb_data_num)


####### Step 1: Re-scale data and create K means cluster - 5 centers  ######

#transformed dataframe from above
str(fb_data_num)

#Now we need to rescale
fb_scaled <- dist(fb_data_num, method = "euclidean")

# Build a kmeans model - start with 5 clusters & nstart = 20
kmeans_model <- kmeans(fb_scaled, centers = 5, nstart = 20)

# Extract the cluster assignment vector from the kmeans model
clust_kmeans_model <- kmeans_model$cluster

# Create a new data frame appending the cluster assignment
fb_data_num2 <- mutate(fb_data_num, cluster = clust_kmeans_model)

# Calculate the size of each cluster
count(fb_data_num2, cluster)
#Cluster 3 only has 11 obs was probably too many clusters to begin with

# Calculate the mean for each category
fb_data_num2 %>% 
  group_by(cluster) %>% 
  summarise_all(list(mean))

#dev.off() --uncomment and run if there is a problem with plotting

# Plot and color main KPIs using their cluster
plot1 <- ggplot(fb_data_num2, aes(x = reach, y = consumptions, color = factor(cluster))) +
  geom_point()

plot2 <- ggplot(fb_data_num2, aes(x = page_likes, y = consumptions, color = factor(cluster))) +
  geom_point()


plot_grid(plot1, plot2, labels = "AUTO")

######## Step 2: Evaluate K-means cluster with many k's & Elbow plot #########

library(purrr)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = fb_data_num, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

head(elbow_df)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

# Based on this plot, the k to choose is 3, the elbow
# occurs at 5 before it flattens out, but the first bend is at 3.



####### Step 3: Silhouette analysis: observation level performance ####

				# • Values close to 1 suggest that the observation is well matched to the assigned cluster
				# • Values close to 0 suggest that the observation is borderline matched between two clusters
				# • Values close to -1 suggest that the observations may be assigned to the wrong cluster


# Generate a k-means model using the pam() function with a k = 3
pam_k3 <- pam(fb_data_num, k = 3)

# Plot the silhouette visual for the pam_k3 model
plot(silhouette(pam_k3))

library(factoextra)
fviz_silhouette(pam_k3) #print avg silhouette in table format

#Group 1 appears to be well matched
#Group 2 is not well-matched
#Group 3 is borderline

#However, the avg silhouette width = 0.54 when k=3

#Let's try again with 5 clusters
# Generate a k-means model using the pam() function with a k = 5
pam_k5 <- pam(fb_data_num, k = 5)

# Plot the silhouette visual for the pam_k5 model
plot(silhouette(pam_k5))

library(factoextra)
fviz_silhouette(pam_k5) #print avg silhouette in table format
#average silhouette went down but Groups 1,3,5 are now better matched
#2&4 are perhaps borderline

##We continue on....


######### ML Method 3: Model-based Clustering Mclust() ############

#A limitation of k-means analysis is that it requires specifying the number of clusters
#and it can be difficult to determine whether one solution is better than another.

#There is an algorithm itself that can suggest how many
#clusters are in the data: model-based clustering

#mclust models data with normal distribution, 
# it only uses numeric data.
#data not entirely normally distributed but
#will attempt this anyway

fb_mclust_mod <- Mclust(fb_data_num)

summary(fb_mclust_mod)
#This tells us the data has 8 clusters - WTF!

#We also see log-likelihood information which we can use to
# compare models.
#We try a 5 cluster solution
fb_data_num_mc5 <- Mclust(fb_data_num, G=5)

summary(fb_data_num_mc5)

#Forcing it to find 5 clusters resulted in a similar model 
# with slightly lower log-likelihood, a
#The clusters on first appearance look well-situated.

#Comparing models with BIC()
#We compare the original cluster and 
#5-cluster models using the Bayesian information criterion

BIC(fb_mclust_mod, fb_data_num_mc5)
#The lower the value of the BIC, the better

#Bic difference is: -1803.56
42952.97-44756.53
#8 clusters seem to work better

######### ML Method 4: k-means revisited with 8 centers ############

# Build a kmeans model - start with 8 clusters
kmeans_model_8clus <- kmeans(fb_scaled, centers = 8, nstart = 20)

# Extract the cluster assignment vector from the kmeans model
labels_8clus <- kmeans_model_8clus$cluster

# Create a new data frame appending the cluster assignment
fb_data_num_8clus <- mutate(fb_data_num, cluster = labels_8clus)

# Calculate the size of each cluster
count(fb_data_num_8clus, cluster)

# Calculate the mean for each category
fb_data_num_8clus %>% 
  group_by(cluster) %>% 
  summarise_all(list(mean))

#We visualize the clusters.
library(cluster)
clusplot(fb_data, labels_8clus, color=TRUE, shade=TRUE, labels=4, lines=0, main = "K-means 8-cluster solution")
#this shows the observations on a multi-dimensional scaling plot with group membership identified by the
#ellipses.

#Cluster 6 stretches too wide.

######## Final summary #############


# Compare methods - hclust vs. kmeans with 8 clusters
table(fb_data_hc_segment, kmeans_model_8clus$cluster)

#Our hclust model assigns most of the observations to cluster 8,
#in the 8 segment solution.


# Although 8 clusters were more statistically sound,
# it didn't lead to better insights than our original
# hclust model.
