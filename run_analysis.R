# setwd ("C:/getclean/UCI HAR DATASET")
#  Is the location of the zip file 

# set libraries for program.

library(base)
library(utils)
library(data.table)

# function loads the data, label and subjects into a data.frame
 

load.dataset <- function (set, features, labels) {

  prefix <- paste(set, '/', sep = '')
  file.data <- paste(prefix, 'X_', set, '.txt', sep = '')
  file.label <- paste(prefix, 'y_', set, '.txt', sep = '')
  file.subject <- paste(prefix, 'subject_', set, '.txt', sep = '')
  
  data <- read.table(file.data)[, features$index]
  names(data) <- features$name
  
  label.set <- read.table(file.label)[, 1]
  subject.set <- read.table(file.subject)[, 1]
  
  data$label <- factor(label.set, levels=labels$level, labels=labels$label)
  
  data$subject <- factor(subject.set)
  
  data.table(data)
}

run_analysis <- function () {
  setwd('c:/getclean/UCI HAR Dataset/')
  
  # Get the features and activity labels
  
  feature.set <- read.table('features.txt', col.names = c('index', 'name'))
  features <- subset(feature.set, grepl('-(mean|std)[(]', feature.set$name))
  
  label.set <- read.table('activity_labels.txt', col.names = c('level', 'label'))
  
  # Read train and test data sets
  train.set <- load.dataset('train', features, label.set)
  test.set <- load.dataset('test', features, label.set)
  
  # Create merged dataset.
  
  dataset <- rbind(train.set, test.set)
  
  # Create new dataset that is tidy.
  
  tidy.dataset <- dataset[, lapply(.SD, mean), by=list(label, subject)]
  
  # Change variable names to make them more readable.
  
  names <- names(tidy.dataset)
  names <- gsub('label', 'Label', names)
  names <- gsub('subject', 'Subject', names)
  names <- gsub('-mean', 'Mean', names) 
  names <- gsub('-std', 'Std', names) 
  names <- gsub('[()-]', '', names) 
  names <- gsub('BodyBody', 'Body', names) 
  names <- gsub('Acc','_', names)
  setnames(tidy.dataset, names)
  
  
  # Write data sets to csv format files.
  
  setwd('c:/getclean/')
  
  write.csv(dataset, file = 'mergedata.csv', row.names = FALSE)
  write.csv(tidy.dataset, file = 'tidydata.csv',
         row.names = FALSE, quote = FALSE)
  
  tidy.dataset
}