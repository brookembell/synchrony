# My try at calculating permutations LOL
# 2-9-18

library(boot)
library(stringr)
library(tidyr)

# Import all dyad data into ONE list ----

setwd("/Users/antunez/Dropbox (Metzlab - VS)/Metz Lab/CURRENT PROJECTS/M2FED/Bite Annotations/Cleaned data/At table")

my_files <- paste0("dyad", 1:24, ".csv")
my_data <- lapply(my_files, read.csv)
names(my_data) <- stringr::str_replace(my_files, pattern = ".csv", replacement = "")

# Reshape data to wide format ----

dyad_reshape <- function(x){
  
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
  
  print(mat)
}

# reshape all dyad datasets
my_data_wide <- lapply(my_data, dyad_reshape)

# Define test statistic: proportion of mimicked bites ----

mimic_a <- function(x) {
  x <- as.data.frame(x)
  x$mimic_5 <- NA
  for(i in 2:nrow(x)){
    x$mimic_5[i] <- ifelse((x$a[i] == 1)
                           & (x$a[i+1] == 1
                           || x$a[i+2] == 1
                           || x$a[i+3] == 1
                           || x$a[i+4] == 1
                           || x$a[i+2] == 1), 1, 0) 
  }
  
  print(x)
}

# calculate mimicked bites for all datasets
my_data_wide_mimic <- lapply(my_data_wide, mimic_a)
  
# George's permtest function ----

permtest <- function(nsim=1e3, statistic, x, ...) {
  
  # Creating space in the memory
  ans <- vector("double", nsim)
  
  # Random sorting of the data (we will use this with the `order` function)
  ORD <- matrix(runif(nsim*length(x)), ncol=nsim)
  for (b in 1:nsim)
    # For each `i`, we compute the statistic changing the order of `x` as a
    # function of `order(ORD[,i])` (this returns indexes)
    ans[b] <- statistic(x[order(ORD[,b])], ...)
  
  # We return the resulting statistic
  ans
  
}


# Step one: defining test statistic (come back to this)

statistic <- function(x){
  sum(x$mimic_5, na.rm = TRUE) / sum(x$a, x$b, na.rm = TRUE)
}

statistic(my_data_wide_mimic$dyad14)

X <- my_data_wide_mimic$dyad14$a
Y <- my_data_wide_mimic$dyad14$b

S <- permtest(1e4, statistic, X, y=Y)
