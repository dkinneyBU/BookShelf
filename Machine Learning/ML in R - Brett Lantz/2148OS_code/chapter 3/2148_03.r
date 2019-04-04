##### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ----

# import the CSV file
b <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# examine the structure of the b data frame
str(b)

# drop the id feature
b <- b[-1]

# table of diagnosis
table(b$diagnosis)

# recode diagnosis as a factor
b$diagnosis <- factor(b$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(b$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(b[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the b data
b_n <- as.data.frame(lapply(b[2:31], normalize))

# confirm that normalization worked
summary(b_n$area_mean)

# create training and test data
b_train <- b_n[1:469, ]
b_test <- b_n[470:569, ]

# create labels for training and test data

b_train_labels <- b[1:469, 1]
b_test_labels <- b[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

b_test_pred <- knn(train = b_train, test = b_test,
                      cl = b_train_labels, k=21)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = b_test_labels, y = b_test_pred,
           prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
b_z <- as.data.frame(scale(b[-1]))

# confirm that the transformation was applied correctly
summary(b_z$area_mean)

# create training and test datasets
b_train <- b_z[1:469, ]
b_test <- b_z[470:569, ]

# re-classify test cases
b_test_pred <- knn(train = b_train, test = b_test,
                      cl = b_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = b_test_labels, y = b_test_pred,
           prop.chisq=FALSE)

# try several different values of k
b_train <- b_n[1:469, ]
b_test <- b_n[470:569, ]

b_test_pred <- knn(train = b_train, test = b_test, cl = b_train_labels, k=1)
CrossTable(x = b_test_labels, y = b_test_pred, prop.chisq=FALSE)

b_test_pred <- knn(train = b_train, test = b_test, cl = b_train_labels, k=5)
CrossTable(x = b_test_labels, y = b_test_pred, prop.chisq=FALSE)

b_test_pred <- knn(train = b_train, test = b_test, cl = b_train_labels, k=11)
CrossTable(x = b_test_labels, y = b_test_pred, prop.chisq=FALSE)

b_test_pred <- knn(train = b_train, test = b_test, cl = b_train_labels, k=15)
CrossTable(x = b_test_labels, y = b_test_pred, prop.chisq=FALSE)

b_test_pred <- knn(train = b_train, test = b_test, cl = b_train_labels, k=21)
CrossTable(x = b_test_labels, y = b_test_pred, prop.chisq=FALSE)

b_test_pred <- knn(train = b_train, test = b_test, cl = b_train_labels, k=27)
CrossTable(x = b_test_labels, y = b_test_pred, prop.chisq=FALSE)
