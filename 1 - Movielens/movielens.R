# MovieLens Project
# Author: Ismael Leal Askerova
# Date: December 2022


##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: script could take some minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(kableExtra)
library(stringr)
library(ggplot2)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



## Show the first 6 rows of the "edx" dataset
head(edx) %>% kable(caption = "First 6 rows") %>% kable_styling(font_size = 9,
                                                                position = "center",
                                                                latex_options = c("scale_down"),
                                                                bootstrap_options = c("hover"),
                                                                full_width = TRUE)


## Display the class of the data in each column of the "edx" dataset
edx %>% summarise(userId = class(userId),
                  movieId = class(movieId),
                  rating = class(rating),
                  timestamp = class(timestamp),
                  title = class(title),
                  genres = class(genres)) %>%
  kableExtra::kable(caption = "Feature types") %>% kable_styling(font_size = 9,
                                                                 position = "center",
                                                                 latex_options = c("scale_down"),
                                                                 bootstrap_options = c("hover"),
                                                                 full_width = TRUE)


## Table 3 showing basic information of the "edx" dataset
edx %>% summarise("Unique users" = n_distinct(userId),
                  "Unique Movies" = n_distinct(movieId),
                  "First rating" = as.Date(as.POSIXct(min(timestamp),
                                                      origin = "1970-01-01")),
                  "Last rating" = as.Date(as.POSIXct(max(timestamp),
                                                     origin = "1970-01-01"))) %>%
  kable(caption =  "Users, movies, dates") %>% kable_styling(font_size = 9,
                                                             position = "center",
                                                             latex_options = c("scale_down"),
                                                             bootstrap_options = c("hover"),
                                                             full_width = TRUE)


## Histogram of ratings in "edx" dataset
edx %>% ggplot(aes(rating)) +
  geom_histogram(fill = "blue",
                 bins = 10,                       # one bin for every possible rating
                 alpha = 0.9,
                 color = "black") +
  labs(x = "Rating", y = "Count",
       title = "Fig. 1 - Frequency of every rating") +
  theme_minimal()


### MOVIE EFFECT

# Create dataset "movie_summary" grouped by movies and showing the number of ratings received,
# the mean rating, and the timestamp of the first rating for each movie
movie_summary <- edx %>% group_by(movieId) %>%
  summarise(n_ratings = n(),
            mean_rating = mean(rating),
            first_rating = min(timestamp))

## Density plot of the number of ratings for each movie
movie_summary %>% ggplot(aes(n_ratings)) + geom_density(fill = "blue",
                                                        alpha = 0.9,
                                                        color = "black") +
  labs(x = "Number of ratings", y = "Density of films",
       title = "Fig. 2 - Density plot of the movies") +
  theme_minimal() +
  geom_vline(aes(xintercept = unname(quantile(n_ratings, probs = 0.75))),       # Add the 3rd quartile line
             color = "purple") +
  annotate("text", x = 1300, y = 0.002,
           label = print(round(unname(quantile(movie_summary$n_ratings, probs = 0.75)))),
           color = "purple", size = 3.6)

## Plot movie average rating vs number of ratings using "movie_summary"
movie_summary %>% ggplot(aes(n_ratings, mean_rating)) + 
  geom_point(color = "blue", alpha = 0.4) + geom_smooth() +
  geom_vline(aes(xintercept = unname(quantile(n_ratings, probs = 0.75))),       # Add the 3rd quartile line
             color = "purple") +
  theme_minimal() +
  labs(title = "Fig. 3 - Scatter plot of movie's average rating vs number of ratings",
       x = "Number of ratings",
       y = "Average rating")


### USER EFFECT

# Create dataset "user_summary" grouped by user and showing the number of ratings given
# and mean rating for each user
user_summary <- edx %>% group_by(userId) %>% summarise(n_u_ratings = n(),
                                                       mean_u_rating = mean(rating))

## Density plot of the number of ratings given by each user
user_summary %>% ggplot(aes(n_u_ratings)) + geom_density(fill = "blue",
                                                         alpha = 0.9,
                                                         color = "black") +
  labs(x = "Number of ratings",
       y = "Density",
       title = "Fig. 4 - Density plot of user ratings") +
  theme_minimal() +
  geom_vline(aes(xintercept = unname(quantile(n_u_ratings, probs = 0.75))),     # Add the 3rd quartile line
             color = "purple") +
  annotate("text", x = 500, y = 0.007,
           label = print(round(unname(quantile(user_summary$n_u_ratings, probs = 0.75)))),
           color = "purple", size = 3.6)

## Plot user average rating vs number of ratings using "user_summary"
user_summary %>% ggplot(aes(n_u_ratings, mean_u_rating)) + 
  geom_point(color = "blue", alpha = 0.4) + geom_smooth() +
  geom_vline(aes(xintercept = unname(quantile(n_u_ratings, probs = 0.75))),     # Add the 3rd quartile line
             color = "purple") +
  theme_minimal() +
  labs(title = "Fig. 5 - Scatter plot of user's average rating vs number of ratings",
       x = "Number of ratings",
       y = "Average rating")


### GENRE EFFECT

# Create dataset "genre_summary" grouped by genre and showing the number of ratings given
# and the mean rating for each genre
genre_summary <- edx %>% group_by(genres) %>% summarise(count = n(),
                                                        rating = mean(rating))

## Plot a column graph of the average rating for every genre combination
genre_summary %>% ggplot(aes(x = reorder(genres, rating),
                             rating)) +
  geom_col(fill = "blue", alpha = 0.9) +
  labs(title = "Fig. 6 - Column graph of average rating for every genre combination",
       x = "Genre combinations", y = "Average rating") +
  theme_minimal() + theme(axis.ticks.x = element_blank(),                       # hide the names of the 797
                          axis.text.x = element_blank())                        # distinct genre combinations

## Density plot of the number of ratings each genre received using "genre_summary"
genre_summary %>% ggplot(aes(x = count)) +
  geom_density(fill = "blue", alpha = 0.9) +
  labs(title = "Fig. 7 - Density plot of genre combinations",
       x = "Number of ratings", y = "Density") +
  theme_minimal() +
  geom_vline(aes(xintercept = unname(quantile(count, probs = 0.75))),           # Add the 3rd quartile line
             color = "purple") +
  annotate("text", x = 35000, y = 0.0001,
           label = print(round(unname(quantile(genre_summary$count, probs = 0.75)))),
           color = "purple", size = 3.6)

## Plot genre average rating vs number of ratings using "genre_summary"
genre_summary %>% ggplot(aes(count, rating)) + geom_point(color="blue",
                                                          alpha = 0.4) +
  geom_smooth() +
  labs(title = "Fig. 8 - Average rating versus number of ratings for each genre",
       x = "Number of ratings", y = "Average rating") +
  geom_vline(aes(xintercept = unname(quantile(count, probs = 0.75))),           # Add the 3rd quartile line
             color = "purple") +
  theme_minimal()


### TIME EFFECT

# Edit "edx" dataset
edx <- edx %>%
  mutate(release_year = as.integer(substr(title, str_length(title) - 4,         # Extract the release year from the title column
                                          str_length(title) - 1)),
         rating_time = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))   # Convert the timestamp of the ratings to date-type data

# Create dataset "year_groupings" grouped by release year and showing the number of ratings
# and the mean rating for all movies released every year
year_groupings <- edx %>% group_by(release_year) %>% summarise(n = n(),
                                                               avg = mean(rating))

## Plot the average rating for movies vs the release year of the movies
year_groupings %>% ggplot(aes(release_year, avg)) +
  geom_point(fill="blue", alpha = 0.4, color="black") +
  geom_smooth() + theme_minimal() +
  labs(title = "Fig. 9 - Average rating per release year",
       x = "Release year", y = "Average rating")

#Adding aging time to edx
edx <- edx %>% left_join(movie_summary, by = "movieId") %>%
  mutate(aging_time = round((timestamp - first_rating)/3600/24/30, 0))          # Convert number of seconds elapsed to months

#Summary table grouped by aging time
# Create dataset "time_summary" grouped by aging time in months and showing
# the number of ratings and the mean rating for each group
time_summary <- edx %>% group_by(aging_time) %>%
  summarise(n = n(),
            mean_rating = mean(rating))

## Plot average rating vs aging time
time_summary %>% ggplot(aes(aging_time, mean_rating)) + geom_point(color="black", alpha=0.4) +
  geom_line(color = "blue", alpha = 0.9) + theme_minimal() +
  labs(title = "Fig. 10 - Average rating per aging time",
       x = "Aging time", y = "Average rating")


### RMSE

#This function calculates the RMSE
RMSE <- function(actual_ratings, predictions){
  sqrt(mean((actual_ratings - predictions)^2))
}


### SPLITTING "EDX" DATASET

set.seed(1, sample.kind = "Rounding")

# Create a list of indexes for a 10% of "edx" rows that will be in the new "test_set"
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)

# Create "train_set" with the remaining 90%
train_set <- edx[-test_index2,]

# Create provisional "pre_test_set" with the selected 10% of rows
pre_test_set <- edx[test_index2,]

# Make sure userId and movieId in test set are also in train set
test_set <- pre_test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add the rows removed from "pre_test_set" to "train_set"
train_set <- rbind(train_set, anti_join(pre_test_set, test_set))

# Remove "pre_test_set" and the list of indexes
rm(pre_test_set, test_index2)


#### Algorithms


### Average

# Calculate the average rating of "train_set"
mu <- mean(train_set$rating)
# This will be our first prediction
model_1_pred <- mu
# Obtain the RMSE for model 1
rmse_1 <- RMSE(test_set$rating, model_1_pred )
# Add it to a data frame "RMSE_df"
RMSE_df <- data.frame(Method = "Average", RMSE = rmse_1)
# Show the table
RMSE_df %>% kable() %>% kable_styling(font_size = 9,
                                      position = "center",
                                      latex_options = c("scale_down"),
                                      bootstrap_options = c("hover"),
                                      full_width = TRUE)


### Movie effect

# Dataframe "b_i_df" with the b_i for each movie and the movieId
b_i_df <- train_set %>% group_by(movieId) %>% summarise(b_i = mean(rating - mu))
# Second prediction
model_2_pred <- test_set %>% left_join(b_i_df, by = "movieId") %>% pull(b_i) + mu
# Obtain RMSE for model 2
rmse_2 <- RMSE(test_set$rating, model_2_pred)
# Add it to "RMSE_df"
RMSE_df <- bind_rows(RMSE_df,
                     data.frame(Method = "Movie Effect",
                                RMSE = rmse_2))
# Show the table
RMSE_df %>% kable() %>% kable_styling(font_size = 9,
                                      position = "center",
                                      latex_options = c("scale_down"),
                                      bootstrap_options = c("hover"),
                                      full_width = TRUE)

## Regularized movie effect
# Possible values for the regularization parameter
lambdas_movie_effect <- seq(0, 5, 0.2)
# Calculate RMSE for every lambda
rmses_reg_movies <- sapply(lambdas_movie_effect, function(x){
  b_i_reg <- train_set %>% group_by(movieId) %>%
    summarise(n_i = n(),
              b_i = sum(rating - mu) / (n_i + x))
  preds <- test_set %>% left_join(b_i_reg, by = "movieId") %>% .$b_i + mu
  return(RMSE(test_set$rating, preds))
})
# Plot RMSE for every lambda
plot(lambdas_movie_effect, rmses_reg_movies)
# Obtain the best regularization parameter
min_lambda_movie <- which.min(rmses_reg_movies)
# RMSE for model 3
rmse_3 <- min(rmses_reg_movies)
# Add it to "RMSE_df"
RMSE_df <- bind_rows(RMSE_df,
                     data.frame(Method = "Regularized Movie Effect",
                                RMSE = rmse_3))
RMSE_df %>% kable() %>% kable_styling(font_size = 9,
                                      position = "center",
                                      latex_options = c("scale_down"),
                                      bootstrap_options = c("hover"),
                                      full_width = TRUE)
# Create a dataset "b_i_reg_df" similar to "b_i_df" but with the regularized values of b_i
b_i_reg_df <- train_set %>% group_by(movieId) %>%
  summarise(n = n(),
            b_i = sum(rating - mu) / (n + 1.6)) %>%
  select(-n)


### User effect

# Dataframe "b_u_df" with the b_u for each user and the userId
b_u_df <- train_set %>% left_join(b_i_reg_df, by = "movieId") %>% group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))
# Fourth prediction
model_4_pred <- test_set %>% left_join(b_i_reg_df, by = "movieId") %>%
  left_join(b_u_df, by = "userId") %>% mutate(preds = mu + b_i + b_u) %>%
  .$preds
# Obtain RMSE for model 4
rmse_4 <- RMSE(test_set$rating, model_4_pred)
# Add it to "RMSE_df"
RMSE_df <- bind_rows(RMSE_df,
                     data.frame(Method = "User Effect",
                                RMSE = rmse_4))
RMSE_df %>% kable() %>% kable_styling(font_size = 9,
                                      position = "center",
                                      latex_options = c("scale_down"),
                                      bootstrap_options = c("hover"),
                                      full_width = TRUE)

## Regularized user effect
# Possible values for the regularization parameter
lambdas_user_effect <- seq(3, 7, 0.2)
# Calculate RMSE for every lambda
rmses_reg_users <- sapply(lambdas_user_effect, function(z){
  b_u_reg <- train_set %>% left_join(b_i_reg_df, by = "movieId") %>%
    group_by(userId) %>%
    summarise(n_i = n(),
              b_u = sum(rating - mu - b_i) / (n_i + z))
  preds <- test_set %>% left_join(b_i_reg_df, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(preds = mu + b_i + b_u) %>% .$preds
  return(RMSE(test_set$rating, preds))
})
# Plot RMSE for every lambda
plot(lambdas_user_effect, rmses_reg_users)
# RMSE for model 5
rmse_5 <- min(rmses_reg_users)
# Add it to "RMSE_df"
RMSE_df <- bind_rows(RMSE_df,
                     data.frame(Method = "Regularized User Effect",
                                RMSE = rmse_5))
RMSE_df %>% kable() %>% kable_styling(font_size = 9,
                                      position = "center",
                                      latex_options = c("scale_down"),
                                      bootstrap_options = c("hover"),
                                      full_width = TRUE)
# Create a dataset "b_u_reg_df" similar to "b_u_df" but with the regularized values of b_u
b_u_reg_df <- train_set %>% left_join(b_i_reg_df) %>% group_by(userId) %>%
  summarise(n_u = n(),
            b_u = sum(rating - mu - b_i) / (n_u + 5)) %>%
  select(-n_u)


### Genre effect

# Dataframe "b_g_df" with the b_g for each genre
b_g_df <- train_set %>%
  left_join(b_u_reg_df, by = "userId") %>%
  left_join(b_i_reg_df, by = "movieId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu - b_i - b_u))
#Sixth prediction
model_6_pred <- test_set %>%
  left_join(b_i_reg_df, by = "movieId") %>%
  left_join(b_u_reg_df, by = "userId") %>%
  left_join(b_g_df, by = "genres") %>%
  mutate(preds = mu + b_i + b_u + b_g) %>%
  .$preds
# Obtain RMSE for model 6
rmse_6 <- RMSE(test_set$rating, model_6_pred)
# Add it to "RMSE_df"
RMSE_df <- bind_rows(RMSE_df,
                     data.frame(Method = "Genre Effect",
                                RMSE = rmse_6))

## Regularized genre effect
# Possible values for the regularization parameter
lambdas_genres_effect <- seq(0, 20, 2)
# Calculate RMSE for every lambda
rmses_reg_genres <- sapply(lambdas_genres_effect, function(y){
  b_g_reg <- train_set %>%
    left_join(b_i_reg_df, by = "movieId") %>%
    left_join(b_u_reg_df, by = "userId") %>%
    group_by(genres) %>%
    summarise(n_i = n(),
              b_g = sum(rating - mu - b_i - b_u) / (n_i + y))
  
  preds <- test_set %>%
    left_join(b_i_reg_df, by = "movieId") %>%
    left_join(b_u_reg_df, by = "userId") %>%
    left_join(b_g_reg, by = "genres") %>%
    mutate(preds = mu + b_i + b_u + b_g) %>% .$preds
  return(RMSE(test_set$rating, preds))
})
# Plot RMSE for every lambda
plot(lambdas_genres_effect, rmses_reg_genres)
# RMSE for model 7
rmse_7 <- min(rmses_reg_genres)
# Add it to "RMSE_df"
RMSE_df_1 <- bind_rows(RMSE_df,
                       data.frame(Method = "Regularized Genre Effect",
                                  RMSE = rmse_7))
RMSE_df_1 %>% kable() %>% kable_styling(font_size = 9,
                                        position = "center",
                                        latex_options = c("scale_down"),
                                        bootstrap_options = c("hover"),
                                        full_width = TRUE)
# No "b_g_reg_df" was created as there was no need due to the best lambda being equal to 0


### Time effect

# Dataframe "b_t_df" with the b_t for each month of aging time
b_t_df <- train_set %>%
  left_join(b_u_reg_df, by = "userId") %>%
  left_join(b_i_reg_df, by = "movieId") %>%
  left_join(b_g_df, by = "genres") %>%
  group_by(aging_time) %>%
  summarise(b_t = mean(rating - mu - b_i - b_u - b_g))
# Eigth prediction
model_8_pred <- test_set %>%
  left_join(b_i_reg_df, by = "movieId") %>%
  left_join(b_u_reg_df, by = "userId") %>%
  left_join(b_g_df, by = "genres") %>%
  left_join(b_t_df, by = "aging_time") %>%
  mutate(preds = mu + b_i + b_u + b_g + b_t) %>%
  .$preds
# Obtain RMSE for model 8
rmse_8 <- RMSE(test_set$rating, model_8_pred)
# Add it to "RMSE_df"
RMSE_df <- bind_rows(RMSE_df,
                     data.frame(Method = "Time Effect",
                                RMSE = rmse_8))
RMSE_df %>% kable() %>% kable_styling(font_size = 9,
                                      position = "center",
                                      latex_options = c("scale_down"),
                                      bootstrap_options = c("hover"),
                                      full_width = TRUE)


#### Testing final algorithm

# Edit "final_holdout_test" to add necessary information (already done with "edx")
a <- final_holdout_test %>% group_by(movieId) %>%
  summarise(first_rating = min(timestamp))                                      # Create DF "a" with the first rating for each movie
final_holdout_test <- final_holdout_test %>%
  left_join(a, by = "movieId") %>%                                              # Add the first rating to "final_holdout_test" dataset
  mutate(aging_time = round((timestamp - first_rating)/3600/24/30, 0))          # Convert its aging time to months as done before for "edx"
# Remove "a" as it will no longer be necessary
rm(a)

# Add b_i, b_u, b_g, b_t to "final_holdout_test" & obtain predictions
final_predictions <- final_holdout_test %>%
  left_join(b_i_reg_df, by = "movieId") %>%
  left_join(b_u_reg_df, by = "userId") %>%
  left_join(b_g_df, by = "genres") %>%
  left_join(b_t_df, by = "aging_time") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  .$pred
# Obtain the RMSE for the final model
final_rmse <- RMSE(final_holdout_test$rating, final_predictions)
