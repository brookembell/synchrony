# FULL FUNCTION FOR GEORGE
# You should be able to insert the raw bite files and it will output a test statistic
# Updated 2-17-18
# Author: Brooke Bell

rm(list=ls())
library(plyr)
library(dplyr)
library(gmodels)
library(stringr)

# IMPORT DYAD DATA FILES -----

# let n = the number of dyads we have
n <- 24

# set working directory to where dyad datasets are saved
setwd("/Users/antunez/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data/At table")

# import dyad datasets
my_files <- paste0("dyad", 1:n, ".csv")
my_data <- lapply(my_files, read.csv)
names(my_data) <- stringr::str_replace(my_files, pattern = ".csv", replacement = "")

# START OF SYNCH_FUNC ----

SYNCH_FUNC <- function(x){

    # RESHAPE DATA ----
    
    num_subjects <- length(unique(x$SubjectID))
    
    leader <- subset(x, x$SubjectID == unique(x$SubjectID)[1]) # Leader bites
    follower <- subset(x, x$SubjectID == unique(x$SubjectID)[2]) # Follower bites
    
    my_time <- min(x$bite_sec):max(x$bite_sec)
    a_bite_list <- leader[[3]] # list of times when person1 took bites
    b_bite_list <- follower[[3]] # list of times when person2 took bites
    
    mat <- as.data.frame(my_time) # turn time matrix into data frame
    
    for (i in 1:nrow(mat)){
      mat$a[i] <- ifelse(is.element(mat$my_time[i], a_bite_list), 1, 0)
      mat$b[i] <- ifelse(is.element(mat$my_time[i], b_bite_list), 1, 0)
    }
    
    # COMPUTE SENSITIVE PERIODS ----
    
    mat$sensitive_a <- 0
    mat$sensitive_b <- 0
    
    # compute sensitive periods for person a
    for (i in 2:nrow(mat)){
      mat$sensitive_a[i] <- ifelse(mat$a[i-1] == 1 
                                 || mat$a[i-2] == 1
                                 || mat$a[i-3] == 1
                                 || mat$a[i-4] == 1
                                 || mat$a[i-5] == 1, 
                                 1, 0)
    }
    
    # compute sensitive periods for person b
    for (i in 2:nrow(mat)){
      mat$sensitive_b[i] <- ifelse(mat$b[i-1] == 1 
                                 || mat$b[i-2] == 1
                                 || mat$b[i-3] == 1
                                 || mat$b[i-4] == 1
                                 || mat$b[i-5] == 1, 
                                 1, 0)
      
      mat$sensitive_b[is.na(mat$sensitive_b)] <- 0
    }
    
    # COMPARE SENSITIVITY ----
    
    # Step 1: Divide into sensitive and nonsensitive subsets
    
    # Person A as lead
    sens_a <- subset(mat, sensitive_a == 1)
    nonsens_a <- subset(mat, sensitive_a == 0)
    
    # Person B as lead
    sens_b <- subset(mat, sensitive_b == 1)
    nonsens_b <- subset(mat, sensitive_b == 0)
    
    # Step 2-3: Compute ratio of mimicked and nonmimicked bites
    
    # Person A as lead
    b_bpm_sens <- (sum(sens_a$b) / nrow(sens_a)) * 60
    b_bpm_nonsens <- (sum(nonsens_a$b) / nrow(nonsens_a)) * 60
    
    # Person B as lead
    a_bpm_sens <- (sum(sens_b$a) / nrow(sens_b)) * 60
    a_bpm_nonsens <- (sum(nonsens_b$a) / nrow(nonsens_b)) * 60
    
    data.frame(b_bpm_sens, b_bpm_nonsens, a_bpm_sens, a_bpm_nonsens)

} # END OF SYNCH_FUNCTION

# APPLY SYNCHRONY FUNCTION TO LIST OF DYAD DATASETS ----

# "my_data" is the list of dyad datasets
my_results <- lapply(my_data, SYNCH_FUNC)

my_results
  
  

