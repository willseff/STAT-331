library(ggplot2)

summary(dtrain)

# number of NAs
sum(is.na(dtrain))

# percent NA
apply(dtrain, 2, function(col)sum(is.na(col))/length(col))

# new df with no NAs
dtrain.nona <- dtrain

# drop column
dtrain.nona$yr_rmdl <- NULL

# delete NA rows in stories
ind <- is.na(dtrain$stories)
dtrain.nona <- dtrain.nona[!ind,]

# check if any NAs left
sum(is.na(dtrain.nona))



