###########1. 49999 New York taxi trips###############


#Loading the tidyverse
library(tidyverse)

#change wd
setwd("/Users/anitaowens/R_Programming/Predict Taxi Fares with Random Forests") #change working directory

getwd() #check working directory


# Reading in the taxi data
taxi <- read_csv('datasets/taxi.csv')

# Taking a look at the first few rows in taxi
head(taxi)
dim(taxi)

###########2. Cleaning the Data ###############


# Renaming the location variables,
# dropping any journeys with zero fares and zero tips,
# and creating the total variable as the log sum of fare and tip
taxi <- taxi %>%
    rename(lat = pickup_latitude,
              long = pickup_longitude) %>%
      filter(fare_amount > 0 | tip_amount > 0)  %>% 
            mutate(total = log(fare_amount + tip_amount))

#dim(taxi)

###########3. Zooming in on Manhattan ###############


# Reducing the data to taxi trips starting in Manhattan
# Manhattan is bounded by the rectangle with 
# latitude from 40.70 to 40.83 and 
# longitude from -74.025 to -73.93
taxi <- taxi  %>% 
filter(between(long, -74.025, -73.93) &
      between(lat,40.70, 40.83 ))
#dim(taxi)

###########4. Where does the journey begin? ###############

# Loading in ggmap and viridis for nice colors
library(ggmap)
library(viridis)

# Retrieving a stored map object which originally was created by
# manhattan <- get_map("manhattan", zoom = 12, color = "bw")
manhattan <- readRDS("datasets/manhattan.rds")

# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
geom_bin2d(aes(x = long, y = lat), data = taxi, bins = 60, alpha = 0.6) +
labs()

###########5. ###############

# Loading in the tree package
library(tree)


# Fitting a tree to lat and long
fitted_tree <- tree(total ~ lat + long, data = taxi)

# Draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree, pretty = 0)

###########6. ###############

# Loading in the lubridate package
library(lubridate)

# Generate the three new time variables
taxi <- taxi %>% 
    mutate(hour = hour(pickup_datetime),
          wday = wday(pickup_datetime, label = TRUE),
          month = month(pickup_datetime, label = TRUE))

taxi$month[1]=="Jan"
str(taxi$month)


###########7. ###############

# Fitting a tree with total as the outcome and 
# lat, long, hour, wday, and month as predictors
fitted_tree <- tree(total ~ lat + long + hour + wday + month , data = taxi)

# draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree, pretty = 0)

# Summarizing the performance of the tree
summary(fitted_tree)


###########8. ###############

# Loading in the randomForest package
library(randomForest)

# Fitting a random forest
fitted_forest <- randomForest(total ~ lat + long + hour + wday + month , data = taxi,
                             ntree = 80, sampsize = 10000)

# Printing the fitted_forest object
fitted_forest


###########9. ###############

# Extracting the prediction from fitted_forest
taxi$pred_total <- fitted_forest$predicted

# Plotting the predicted mean trip prices from according to the random forest
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
stat_summary_2d(aes(x = long, y = lat, z = pred_total), data = taxi, bins = 60, alpha = 0.6, fun = mean) +
labs()


###########10. ###############

# Function that returns the mean *if* there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
    ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the mean trip prices from the data
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
stat_summary_2d(aes(x = long, y = lat, z = total), data = taxi, bins = 60, alpha = 0.6, fun = mean_if_enough_data) +
labs()

###########11. ###############

# Where are people spending the most on their taxi trips?
spends_most_on_trips <- "downtown" # "uptown" or "downtown"
#dim(taxi)