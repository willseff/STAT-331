# first set working directory at the current folder (where data files are), then read in the data
# setwd() can be used to set working directory
load("final.Rdata")

# load user-defined LinearModel function, use UW_ID 87654321 as an example
source("20519497.R")
# you should change the above source file to yours when ready

# call the function
res <- LinearModel(dtrain, dtest)

# The grading team will compare the result to the solution to compute RMLSE
# for example, assume sol is the data.frame containing the true price in the same order as in res
load("solution.Rdata")
sqrt(mean((log(res$price)-log(sol$price))^2))
# for obvious reasons, the solution provided here is only for the ilustration purpose

# If you want to save the result for Kaggle submission, uncomment the next line 
write.csv(res, file="solution_sample.csv", row.names=FALSE)

