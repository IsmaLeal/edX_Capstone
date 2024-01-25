# MovieLens Project
# Author: Ismael Leal Askerova
# Date: January 2023

# Load libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggdendro)) install.packages("ggdendro", repos = "http://cran.us.r-project.org")
if(!require(dendextend)) install.packages("dendextend", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(xfun)) install.packages("xfun", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", dependencies = c("Depends", "Suggests"), repos = "http://cran.us.r-project.org")
if(!require(R.utils)) install.packages("R.utils", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")

# Set ggplot2 theme
plot_theme <- theme(plot.caption = element_text(size = 11), axis.title = element_text(size = 10))

# Run code chunk options
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      out.width="80%", fig.align="center")

# Set number of decimals
options(digits = 3)

# Download data set
data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
cleveland <- read.csv(data_url)

# Add column names
feature_names <- c("age",
                   "sex",
                   "chest_pain_type",
                   "rest_blood_pressure",
                   "cholesterol",
                   "fasting_blood_sugar",
                   "rest_electrocardiographic",
                   "max_heart_rate",
                   "exercise_induced_angina",
                   "old_peak",
                   "slope",
                   "vessels_number",
                   "thalassemia_type",
                   "diagnosis")
colnames(cleveland) <- feature_names

# Create a data frame with descriptions for every column
column_description <- data.frame(Feature = feature_names,
                                 Description = c("Age of the patient",
                                                 "Biological sex (0 = female, 1 = male)",
                                                 "Chest pain type (1 = typical angina, 2 = atypical angina, 3 = non-anginal pain, \n 4 = asymptomatic)",
                                                 "Resting blood pressure (in mmHg)",
                                                 "Serum cholesterol level in mg/dl",
                                                 "Fasting blood sugar > 120 mg/dl (0 = false, 1 = true)",
                                                 "Resting electrocardiographic results (0 = normal, 1 = ST-T wave abnormality,\n  2 = left ventricular hypertrophy)",
                                                 "Maximum heart rate",
                                                 "Exercise-induced angina (0 = no, 1 = yes)",
                                                 "ST depression induced by exercise relative to rest",
                                                 "Slope of the peak exercise ST segment (1 = positive, 2 = flat, 3 = negative)",
                                                 "Number of major vessels (0, 1, 2, or 3) colored by fluoroscopy",
                                                 "Thalassemia type (3 = normal, 6 = fixed defect, 7 = reversable defect)",
                                                 "Diagnosis"))
column_description %>%
  kable(caption = "Feature description", align = "ll", booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position", "scale_down"))


# Table showing summary data
summary(cleveland[, 1:7]) %>%
  kable(caption = "Feature summary (1)", align = "ccccccc", booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(position = "center",
                latex_options = c("scale_down", "hold_position"))


# Convert from character to integer class
cols <- c(12, 13)
cleveland[, cols] = apply(cleveland[, cols], 2, function(x) as.integer(x))
cleveland <- na.omit(cleveland)


summary(cleveland[, 8:13]) %>%
  kable(caption = "Feature summary (2)", align = "cccccc", booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(position = "center",
                latex_options = c("scale_down", "hold_position"))



# Table showing continuous features
data.frame(List = c("rest_blood_pressure",
                    "cholesterol",
                    "max_heart_rate",
                    "old_peak")) %>%
  kable(caption = "Continuous features", align = "l", booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(position = "center",
                latex_options = c("hold_position"))


# Create a table displaying the class of every feature
data.frame("Column name" = feature_names,
           Class = c(class(cleveland$age),
                     class(cleveland$sex),
                     class(cleveland$chest_pain_type),
                     class(cleveland$rest_blood_pressure),
                     class(cleveland$cholesterol),
                     class(cleveland$fasting_blood_sugar),
                     class(cleveland$rest_electrocardiographic),
                     class(cleveland$max_heart_rate),
                     class(cleveland$exercise_induced_angina),
                     class(cleveland$old_peak),
                     class(cleveland$slope),
                     class(cleveland$vessels_number),
                     class(cleveland$thalassemia_type),
                     class(cleveland$diagnosis))) %>%
  kable(caption = "Feature class", align = "l", booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(position = "center",
                latex_options = c("hold_position"))


# Store the diagnosis predictor as a factor
cleveland$diagnosis <- ifelse(cleveland$diagnosis == 0, "H", "D")
cleveland$diagnosis <- as.factor(cleveland$diagnosis)

# Free space used
rm(data_url, cols)


# Save list of matrix x with the data and outcomes y with the diagnosis
cleveland <- list(x = as.matrix(cleveland[, 1:13]), y = cleveland$diagnosis)


## Training and test sets creation
# Test & train set creation
set.seed(5, sample.kind = "Rounding")

# List of cleveland rows that will be in the test set
test_index <- createDataPartition(cleveland$y, times = 1, p = 0.1, list = FALSE)
train <- list(x = cleveland$x[-test_index, ], y = cleveland$y[-test_index])
test <- list(x = cleveland$x[test_index, ], y = cleveland$y[test_index])


# Center the train$x data around zero (subtracting the column mean)
train_centered <- sweep(train$x, 2, colMeans(train$x))

# Fully normalize the train$x data (dividing by the column SD)
train_set <- sweep(train_centered, 2, colSds(train$x), FUN = "/")

# Remove train_centered
rm(train_centered)


# Euclidean distances between all observations (all patients)
d <- dist(train_set)

# Average distance between observations
mean_dist <- round(mean(as.matrix(d)), 2)

# Average distance between healthy patients
mean_healthy_dist <- round(mean(as.matrix(d)[train$y == "H"]), 2)

# Average distance between patients with a heart disease
mean_disease_dist <- round(mean(as.matrix(d)[train$y == "D"]), 2)

# Average distance between healthy patients and patients with a disease
mean_healthy_disease_dist <- round(mean(as.matrix(d)[train$y == "D", train$y == "H"]), 2)


# Heatmap of distances between observations
heatmap(as.matrix(d), symm = T, revC = T,
        col = brewer.pal(4, "PuRd"),
        ColSideColors = ifelse(train$y=="H", "green", "red"),
        RowSideColors = ifelse(train$y=="H", "green", "red"),
        labRow = NA, labCol = NA)


# Create data frame with categorical data and outcomes
df <- as.data.frame(train$x) %>%
  mutate(Diagnosis = train$y)

df %>% dplyr::select(sex, chest_pain_type,
                     fasting_blood_sugar,
                     rest_electrocardiographic,
                     exercise_induced_angina,
                     slope, vessels_number,
                     thalassemia_type, Diagnosis) %>%
  gather("feature", "value", -Diagnosis) %>%
  ggplot(aes(value, fill = Diagnosis)) +
  geom_histogram() +
  labs(x = "Categorical feature values",
       y = "Count") +
  theme(legend.position = "right",
        axis.text.y = element_blank()) +
  scale_fill_discrete(labels = c("Disease", "Healthy")) +
  facet_wrap(~ feature, scales = "free", ncol = 3)


# Use nearZeroVar function and store results
nzv <- nearZeroVar(train_set, saveMetrics = TRUE)


# Display nearZeroVar results in a table
nzv %>% kable(caption = "nearZeroVar outcomes", align = "lcccc", booktabs = T,
              format = "latex", linesep = "") %>%
  kable_styling(position = "center",
                latex_options = c("hold_position"))


# Hierarchical clustering of predictors
clustering <- hclust(dist(t(train_set)))

# Dendrogram object
dend <- as.dendrogram(clustering) %>% set("branches_k_color", k = 5) %>%
  set("labels_cex", 0.39) %>%
  set("labels_colors", k = 5)

# ggdend object
ggd <- as.ggdend(dend)

# Visualization of the dendrogram
ggplot(ggd, horiz = TRUE, theme = plot_theme) + labs(x = "Features",
                                                     y = "Distance")


# Calculate the correlations between the features of the data set
train_set_correlations <- abs(cor(train_set))

# Plot as a heat map
heatmap(as.matrix(train_set_correlations),
        symm = T, revC = T,
        col = brewer.pal(4, "PuRd"),
        labCol = NA)



# Numeric vector of diagnosis where 0 means healthy and 1 indicates a heart disease
numeric_diagnosis <- ifelse(train$y == "H", 0, 1)

# Vector of correlations between each feature and the diagnosis vector
cors_with_diagnosis <- sapply(1:ncol(train$x), function(i){
  cor(train$x[, i], numeric_diagnosis)
})

# Create a data frame with the name of each of the 13 features and the correlation of each with the diagnosis
df2 <- data.frame(Correlation = cors_with_diagnosis, Features = feature_names[1:13])

# Plot it
df2 %>% ggplot(aes(x = Features, y = cors_with_diagnosis)) +
  geom_col(fill="blue") +
  labs(x = "Features",
       y = "Correlations") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Create pca object
pca <- prcomp(train_set)

# Calculate variance scores per principal component
pca.var <- pca$sdev^2
pca.var.per <- pca.var/sum(pca.var)

# Create table of principal components
summary(pca)$importance %>%
  kable(caption = "Principal Components", booktabs = T, format = "latex") %>%
  kable_styling(position = "center", latex_options = c("scale_down", "hold_position"))


# Create boxplot of the main 10 principal components by diagnosis
data.frame(pca$x[,1:10], Diagnosis = train$y) %>%
  gather(key = "PC", value = "value", -Diagnosis) %>%
  ggplot(aes(PC, value, fill = Diagnosis)) +
  geom_boxplot() +
  scale_fill_discrete(name="Diagnosis",
                      breaks=c("H", "D"),
                      labels=c("Healthy", "Disease"))


# Plot PC2 vs PC1 colored by diagnosis
data.frame(pca$x[,1:2], Diagnosis = train$y) %>%
  ggplot(aes(PC1, PC2, color = Diagnosis)) +
  geom_point() +
  stat_ellipse() +
  xlab(paste("PC1: ", percent(pca.var.per[1],0.1))) +
  ylab(paste("PC2: ", percent(pca.var.per[2],0.1))) +
  scale_color_discrete(name="Diagnosis",
                       breaks=c("H", "D"),
                       labels=c("Healthy", "Disease"))


# Data frame to compare models
metrics <- data.frame(Model = character(),
                      Accuracy = double(), 
                      Sensitivity = double(),
                      Specificity = double(),
                      F1 = double())


## Center the test data around zero
test_c <- sweep(test$x, 2, colMeans(train$x))

## Scale the test data
test_set <- sweep(test_c, 2, colSds(train$x), FUN = "/")

rm(test_c)



# Define train control parameters for appropriate models
fitControl <- trainControl(method = "repeatedcv",
                           number = 6,
                           repeats = 8, # perform each cross-validation 8 times
                           classProbs = TRUE,
                           returnResamp = "final",
                           savePredictions = "final")


# Same probability for both diagnoses
prob <- 0.5

# Create a test set for this model (no training set required as there is no training in this model)
set.seed(20, sample.kind = "Rounding")
test_index_random <- createDataPartition(train$y, times = 1, p = 0.2, list = FALSE)
test_random <- list(x = train$x[test_index_random, ], y = train$y[test_index_random])

# Randomly sample outcomes
set.seed(5, sample.kind = "Rounding")
random <- sample(c("H", "D"), length(test_index_random), prob = c(1-prob, prob), replace = TRUE) %>%
  factor(levels = levels(test$y))

# Confusion matrix
random_matrix <- confusionMatrix(random, test_random$y, positive = "D")

# Append results to metrics data frame
metrics[1, ] <- c("Random sampling",
                  format(round(random_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(random_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(random_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(random_matrix$byClass["F1"],2), nsmall = 2))


# Weighted probability for "D" or "H"
prob <- mean(train$y == "D")

# Create a test set for this model (no training set required as there is no training in this model)
set.seed(2, sample.kind = "Rounding")
test_index_weighted_random <- createDataPartition(train$y, times = 1, p = 0.2, list = FALSE)
test_weighted_random <- list(x = train$x[test_index_weighted_random, ], y = train$y[test_index_weighted_random])

# Randomly sample outcomes
set.seed(5, sample.kind = "Rounding")
random <- sample(c("H", "D"), length(test_index_weighted_random), prob = c(1-prob, prob),
                 replace = TRUE) %>%
  factor(levels = levels(test$y))

# Store confusion matrix
random_matrix2 <- confusionMatrix(random, test_random$y, positive = "D")

# Append results to metrics data frame
metrics[2, ] <- c("Weighted random sampling",
                  format(round(random_matrix2$overall["Accuracy"],2), nsmall = 2),
                  format(round(random_matrix2$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(random_matrix2$byClass["Specificity"],2), nsmall = 2),
                  format(round(random_matrix2$byClass["F1"],2), nsmall = 2))


# Exclude discrete features
continuous_cols_indices <-c(1, 4, 5, 8, 10)
train_set_continuous <- train_set[, continuous_cols_indices]

# Function for k-means prediction
predict_kmeans <- function(x, k) {
  # store centers of every cluster
  centers <- k$centers
  # distance from data-points to the cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  # select cluster with min distance to center
  max.col(-t(distances))
}

# Create a test set for this model (no training set required as there is no training in this model)
set.seed(4, sample.kind = "Rounding")
test_index_kmeans <- createDataPartition(train$y, times = 1, p = 0.2, list = FALSE)
test_kmeans <- list(x = train_set_continuous[test_index_kmeans, ], y = train$y[test_index_kmeans])

set.seed(19, sample.kind = "Rounding")

# Predict outcome using kmeans model with k=2 and 30 random sets
k <- kmeans(train_set_continuous, centers = 2, nstart = 30)

# Obtain the predictions
kmeans <- factor(ifelse(predict_kmeans(test_kmeans$x, k) == 1, "D", "H"))

# Store confusion matrix in 'kmeans_results_1' object
kmeans_matrix <- confusionMatrix(kmeans, test_kmeans$y, positive = "D")

metrics[3, ] <- c("K-means clustering",
                  format(round(kmeans_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(kmeans_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(kmeans_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(kmeans_matrix$byClass["F1"],2), nsmall = 2))


# Create a training and test set for the model development
set.seed(81, sample.kind = "Rounding")
test_index_2 <- createDataPartition(train$y, times = 1, p = 0.2, list = FALSE)
train_2 <- list(x = train_set[-test_index_2, ], y = train$y[-test_index_2])
test_2 <- list(x = train_set[test_index_2, ], y = train$y[test_index_2])


# Naive Bayes model

set.seed(54, sample.kind = "Rounding")

# Use caret package to train and predict outcomes
train_bayes <- train(train_2$x, train_2$y, 
                     method = "nb",
                     tuneGrid = expand.grid(usekernel = c(FALSE, TRUE), fL = c(0, 1), adjust = c(0, 1)),
                     trControl = fitControl)
# Store predictions
bayes <- predict(train_bayes, test_2$x)

# Confusion matrix
bayes_matrix <- confusionMatrix(bayes, test_2$y, positive = "D")

# Add model results to metrics
metrics[4, ] <- c("Naive Bayes",
                  format(round(bayes_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(bayes_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(bayes_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(bayes_matrix$byClass["F1"],2), nsmall = 2))


# LDA

set.seed(12, sample.kind = "Rounding")

# Use caret package to train a model
train_lda <- train(train_2$x, train_2$y, 
                   method = "lda", 
                   trControl = fitControl)

# Store predictions
lda <- predict(train_lda, test_2$x)

# Confusion matrix
lda_matrix <- confusionMatrix(lda, test_2$y, positive = "D")

# Add results to metrics
metrics[5, ] <- c("Linear Discriminant Analysis",
                  format(round(lda_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(lda_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(lda_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(lda_matrix$byClass["F1"],2), nsmall = 2))


# QDA

set.seed(37, sample.kind = "Rounding")

# Train the model
train_qda <- train(train_2$x, train_2$y, 
                   method = "qda", 
                   trControl = fitControl)

# Store predictions
qda <- predict(train_qda, test_2$x)

# Confusion matrix
qda_matrix <- confusionMatrix(qda, test_2$y, positive = "D")

# Append to metrics
metrics[6, ] <- c("Quadratic Discriminant Analysis",
                  format(round(qda_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(qda_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(qda_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(qda_matrix$byClass["F1"],2), nsmall = 2))


# Generalized Linear Model (GLM)

set.seed(64, sample.kind = "Rounding")

# Train model
train_glm <- train(train_2$x, train_2$y, 
                   method = "glm", 
                   trControl = fitControl)

# Predictions
glm <- predict(train_glm, test_2$x)

# Confusion matrix
glm_matrix <- confusionMatrix(glm, test_2$y, positive = "D")

# Add results to model_results data frame
metrics[7, ] <- c("Logistic regression",
                  format(round(glm_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(glm_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(glm_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(glm_matrix$byClass["F1"],2), nsmall = 2))


# KNN

set.seed(22, sample.kind = "Rounding")

# Train the knn model
train_knn <- train(train_2$x, train_2$y,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(1, 50, 1)),
                   trControl = fitControl)

# Predictions
knn <- predict(train_knn, test_2$x)

# Confusion matrix
knn_matrix <- confusionMatrix(knn, test_2$y, positive = "D")

# Add results to model_results data frame
metrics[8, ] <- c("K Nearest Neighbour",
                  format(round(knn_matrix$overall["Accuracy"],2), nsmall = 2),
                  format(round(knn_matrix$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(knn_matrix$byClass["Specificity"],2), nsmall = 2),
                  format(round(knn_matrix$byClass["F1"],2), nsmall = 2))


# Random Forest

set.seed(101, sample.kind = "Rounding")

# Train model
train_rf <- train(train_2$x, train_2$y,
                  method = "rf",
                  tuneGrid = data.frame(mtry = seq(3, 15, 2)),
                  importance = TRUE,
                  trControl = fitControl)

# Predictions
rf <- predict(train_rf, test_2$x)

# Confusion matrix
rf_results <- confusionMatrix(rf, test_2$y, positive = "D")

# Append to metrics
metrics[9, ] <- c("Random Forest",
                  format(round(rf_results$overall["Accuracy"],2), nsmall = 2),
                  format(round(rf_results$byClass["Sensitivity"],2), nsmall = 2),
                  format(round(rf_results$byClass["Specificity"],2), nsmall = 2),
                  format(round(rf_results$byClass["F1"],2), nsmall = 2))


# Neural networks

set.seed(92, sample.kind = "Rounding")

# Train model
train_nn <- train(train_2$x, train_2$y,
                  method = "nnet",
                  trace = FALSE,
                  trControl = fitControl)

# Predictions
nn <- predict(train_nn, test_2$x)

# Confusion matrix
nn_matrix <- confusionMatrix(nn, test_2$y, positive = "D")

# Append to metrics
metrics[10, ] <- c("Neural Network",
                   format(round(nn_matrix$overall["Accuracy"],2), nsmall = 2),
                   format(round(nn_matrix$byClass["Sensitivity"],2), nsmall = 2),
                   format(round(nn_matrix$byClass["Specificity"],2), nsmall = 2),
                   format(round(nn_matrix$byClass["F1"],2), nsmall = 2))


# Build ensemble with the already implemented supervised ones
ensemble <- cbind(glm_e = ifelse(glm == "H", 0, 1), bayes_e = ifelse(bayes =="H", 0, 1), lda_e = ifelse(lda == "H", 0, 1), qda_e = ifelse(qda == "H", 0, 1), rf_e = ifelse(rf == "H", 0, 1), knn_e = ifelse(knn == "H", 0, 1), nn_e = ifelse(nn == "H", 0, 1))

# Predict according to majority vote of all models
ensemble <- as.factor(ifelse(rowMeans(ensemble) < 0.5, "H", "D"))

# Confusion matrix
ensemble_matrix <- confusionMatrix(ensemble, test_2$y, positive = "D")

# Add results to model_results data frame
metrics[11, ] <- c("Ensemble",
                   format(round(ensemble_matrix$overall["Accuracy"],2), nsmall = 2),
                   format(round(ensemble_matrix$byClass["Sensitivity"],2), nsmall = 2),
                   format(round(ensemble_matrix$byClass["Specificity"],2), nsmall = 2),
                   format(round(ensemble_matrix$byClass["F1"],2), nsmall = 2))


# Table of results
metrics %>%
  kable(caption = "Performance of models used", align = "lcccc", booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position", "scale_down"))




