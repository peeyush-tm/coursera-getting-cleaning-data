######################################################################################## 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each
#    activity and each subject. 
########################################################################################
run.analysis <- function() {
    
    # Print Requirements first
    print(' INSTALL : data.table ')
    print(' INSTALL : reshape2 ')
    print(' INSTALL : plyr ')
    
    library(data.table)
    library(plyr)
    library(reshape2)
    
    # Setting some constant variables and the functions needed
    ##########################################################
    data.directory <- "./UCI-HAR-Dataset"
    path.sep <- "/"
    file.extension <- ".txt"
    
    ########################################################################################
    # This function imports the files and returns a data frame with subjects, activities 
    # and measures.
    # set.type must be "train" or "test"
    ########################################################################################
    create.set <- function(set.type) {
        
        ## Check that set.type is valid
        if (!set.type %in% c("train","test")) {
            stop("invalid set.type. It must be train or test")
        }
        
        # Subject
        subjects.file.name = paste("subject_", set.type, file.extension, sep = "")
        subjects <- read.table(paste(data.directory, set.type, subjects.file.name, sep = path.sep), 
                               header = FALSE,
                               col.names = c("subject.id"),
                               colClasses = "integer")
        
        
        activities.file.name = paste("y_", set.type, file.extension, sep = "")
        activities <- read.table(paste(data.directory, set.type, activities.file.name, sep = path.sep), 
                                 header = FALSE,
                                 col.names = c("activity.id"), 
                                 colClasses = "factor")
        
        x.file.name = paste("X_", set.type, file.extension, sep = "")
        x.set <- read.fwf(paste(data.directory, set.type, x.file.name, sep = path.sep),
                          rep(16, times=561), 
                          header = FALSE,
                          colClasses = "numeric",
                          buffersize = 250)
        
        x.set
        
        return.set <- cbind(subjects, activities, x.set)
        # Return
        return.set
    }
    
    
    ########################################################################################
    # This function imports the features file and returns the column names for the total 
    # dataset.
    ########################################################################################
    dataset.column.names <- function() {
        # Loading features
        features.file.name = paste("features", file.extension, sep = "")
        features.file <- read.table(paste(data.directory, features.file.name, sep = path.sep), 
                                    sep = " ", 
                                    header = FALSE,
                                    col.names = c("feature.id","feature.description"),
                                    colClasses = c("factor","character"))
        # Return
        c("subject.id", "activity.id", features.file$feature.description)
    }
    
    
    ########################################################################################
    # This function extracts only the subject, the activity and the measurements on the mean
    # and standard deviation. 
    ########################################################################################
    extract.columns.dataset <- function(total.dataset) {
        # Choosing columns to extract
        column.names <- cbind(names(total.dataset))
        column.table <- data.frame(column.names)
        # mean columns
        column.table$extract <- grepl("-mean", column.table$column.names)
        # std columns
        column.table$extract <- column.table$extract | grepl("-std", column.table$column.names)
        column.table$extract[1:2] <- TRUE
        
        # get only cols to extract
        total.dataset <- total.dataset[,column.table$extract]
        # Return
        total.dataset
    }
    
    
    ########################################################################################
    # This function extracts only the subject, the activity and the measurements on the mean
    # and standard deviation. 
    ########################################################################################
    label.activity.column <- function(total.dataset) {
        ## Loading activities
        activity.labels.file.name = paste("activity_labels", file.extension, sep = "")
        activity.labels.file <- read.table(paste(data.directory, activity.labels.file.name, sep = path.sep), 
                                           sep = " ", 
                                           header = FALSE,
                                           col.names = c("activity.id","activity.description"),
                                           colClasses = c("factor","factor"))
        
        new.total.dataset <- merge(activity.labels.file, total.dataset, all=TRUE)
        total.dataset <- new.total.dataset[,c(3,2,4:82)]
        # Return
        total.dataset
    }
    
    ########################################################################################
    # This function creates a second, independent tidy data set with the average of each 
    # variable for each activity and each subject. 
    ########################################################################################
    create.new.dataset <- function(total.dataset) {

        # variables
        vector.column.names <- names(total.dataset)
        vector.column.names <- vector.column.names[3:81]
        # Melting data frame
        dataset.melt <- melt(total.dataset,
                             id=c("subject.id","activity.description"),
                             measure.vars=vector.column.names)
        # Create the new dataset
        new.dataset <- dcast(dataset.melt, subject.id + activity.description ~ variable, mean)
        # Sorting the dataset    
        new.dataset <- arrange(new.dataset, subject.id, activity.description)
        # Return
        new.dataset
    }
    
    
    print(" Create a single dataset with train and test datasets ")
    # Create the training set
    print(" Loading 'train' data ")
    print(" please wait ... ")
    train.dataset <- create.set("train")
    
    # Create the test set
    print(" Loading 'test' data ")
    print(" please wait ... ")
    test.dataset <- create.set("test")
    
    # Create the total dataset
    print(" Merging 'train' & 'test' data ")
    print(" please wait ... ")
    total.dataset <- rbind(train.dataset, test.dataset)
    
    # Cleaning memory
    rm(train.dataset, test.dataset)
    
    
    ##########################################################
    # Setting column names
    colnames(total.dataset) <- dataset.column.names()
    # get only cols to extract
    total.dataset <- extract.columns.dataset(total.dataset)
    # Use activity names
    total.dataset <- label.activity.column(total.dataset)
    
    
    # Creating and exporting the new dataset
    ##########################################################
    write.table(create.new.dataset(total.dataset), 
                "new_dataset.txt",
                sep=",",
                row.names = FALSE,
                quote = FALSE)
    print("New merged data set has been prepared")
}   