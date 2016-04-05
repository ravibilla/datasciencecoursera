### SlideRule Foundations of Data  Science
### Baseball Analytics and Regression

## Read the data set into R
raw.bb <- read.csv(file.choose()) # prompts you to choose the file to be loaded 
attach(raw.bb)

## Get the "shape" of the data
dim(raw.bb)
head(raw.bb)
tail(raw.bb)
str(raw.bb)

summary(raw.bb) # Gives summary statistics

# The following shows the percentiles for the 15th variable 
# Note: for advanced programmers, you can consider using a FOR loop or any of the apply functions 
# to run this for all variables
quantile(raw.bb[,15],probs=c(0.01,0.05,0.5,0.95,0.99),na.rm=T) 


# These commands shows a "correlation" matrix plus histograms and scatterplots
# Note: you may want to maximize your panel where the chart is produced

library(psych)
pairs.panels(raw.bb[,-1]) # The "-1" tells R to read all variables except the first

# At this point, you should more or less have a sense of which variables 
# correlate well with TARGET_WINS", and which ones are correlated with
# each other
# For example, TEAM_BATTING_H has the highest correlation with TARGET_WINS (0.39)
# Also, TEAM_BATTING_SO appears to be highly correlated to TEAM_PITCHING_HR (0.67)
# Note: I attached a pdf version of the chart, but the quality of this is not that good

## Tree EDA (Model-Based EDA)
## A Model-Based EDA supplements a traditional EDA, especially for cases where there are
## numerous variables, making it impractical to run multiple charts.
## This type of EDA gives a sense-check of the variables that were significant
## when running the naive model.
## In this example, a regression tree was used for the EDA:

library(tree)
tree.wins <- tree(TARGET_WINS~.-INDEX, raw.bb) # run a model of wins vs all variables
summary(tree.wins) # gives the variables entered in the tree model
plot(tree.wins)
text(tree.wins,pretty=0)

### Traditional EDA
### CHARTS

## Histogram
hist(TEAM_FIELDING_E, prob=T, main="Histogram of Team Fielding E", xlab="PCT", ylab=NULL)
curve(dnorm(x, mean=mean(TEAM_FIELDING_E), sd=sd(TEAM_FIELDING_E)), add=T, col="red")
# NOTE: the curve function superimposes a "normal" curve

## Scatterplot
plot(TARGET_WINS~TEAM_FIELDING_E)
abline(lm(TARGET_WINS~TEAM_FIELDING_E,data=raw.bb),col="red",main="Wins vs Team Fielding E")
# NOTE: the abline function draws a regression line for WINS vs TEAM FIELDING E

par(mfrow=c(1,2)) # Use this to partition the plot area; useful for viewing charts side-by-side

hist(TEAM_BATTING_H, prob=T, main="Histogram of Team Batting H", xlab="PCT", ylab=NULL)
curve(dnorm(x, mean=mean(TEAM_BATTING_H), sd=sd(TEAM_BATTING_H)), add=T, col="red")

plot(TARGET_WINS~TEAM_BATTING_H)
abline(lm(TARGET_WINS~TEAM_BATTING_H,data=raw.bb),col="red",main="Wins vs Team Batting H")

# QQ-Plot to check for normality
# The plot should "hug" the main diagonal for the variable to be considered normally distributed
qqnorm(raw.bb[,2], main="QQ Plot for Target Wins")
qqline(raw.bb[,2], col="red")


# TARGET_WINS
hist(TARGET_WINS, prob=T, main="Histogram of Target Wins", xlab="PCT", ylab=NULL)
curve(dnorm(x, mean=mean(TARGET_WINS), sd=sd(TARGET_WINS)), add=T, col="red")
qqnorm(raw.bb[,3], main="QQ Plot for Team Batting H")
qqline(raw.bb[,3], col="red")

### Variable Transformations
### These are performed in order to get different "views" of your data.
### Transformations are also used to: define new variables; data cleansing (e.g. outliers)

# "Bin" or Categorize the number of wins into 5 groups:
raw.bb$wins <- ifelse(TARGET_WINS<31,1,
                      ifelse(TARGET_WINS<61,2,
                             ifelse(TARGET_WINS<91,3,
                                    ifelse(TARGET_WINS<121,4,5))))
# NOTE: the choice of cut-off points is arbitrary. In this case, the range of 0 to 150 was cut into 5 sections

boxplot(TEAM_BATTING_H~wins,data=raw.bb,col=rainbow(5),main="Team Batting H")
boxplot(TEAM_PITCHING_H~wins,data=raw.bb,col=rainbow(5), main="Team Pitching H")
boxplot(TEAM_PITCHING_HR~wins,data=raw.bb,col=rainbow(5),main="Team Pitching HR")

## Another Transformation
raw.bb$TEAM_BATTING_1B <- TEAM_BATTING_H - TEAM_BATTING_HR - TEAM_BATTING_2B - TEAM_BATTING_3B
boxplot(TEAM_BATTING_1B~wins,data=raw.bb,col=rainbow(5))

## Ad-Hoc Replacement of Missing Values
bbdata <- raw.bb   # Best Practice: retain the original data set and copy over to a new set
summary(bbdata)
attach(bbdata)

# Team Baserun SB; mean = 125
bbdata$TEAM_BASERUN_SB <- ifelse(is.na(bbdata$TEAM_BASERUN_SB),125,bbdata$TEAM_BASERUN_SB)

#bbdata$TEAM_BASERUN_SB <- ifelse(is.na(bbdata$TEAM_BASERUN_SB),median(bbdata$TEAM_BASERUN_SB,na.rm=T),bbdata$TEAM_BASERUN_SB)

# NOTE: the ifelse function works logically similar to the Excel IF(condition,result,alternative)

# Team Baserun CS; mean = 53; MEDIAN = 49
bbdata$TEAM_BASERUN_CS <- ifelse(is.na(bbdata$TEAM_BASERUN_CS),49,bbdata$TEAM_BASERUN_CS)

# Team BATTING SO; mean = 736; MEDIAN = 750
bbdata$TEAM_BATTING_SO <- ifelse(is.na(bbdata$TEAM_BATTING_SO),736,bbdata$TEAM_BATTING_SO)

# Team FIELDING DP; mean = 146; MEDIAN = 149
bbdata$TEAM_FIELDING_DP <- ifelse(is.na(bbdata$TEAM_FIELDING_DP),146,bbdata$TEAM_FIELDING_DP)


## Fix Team Pitching SO Outlier
# 95th %ile = 1173; 99th %ile = 1466.70
bbdata$TEAM_PITCHING_SO <- ifelse(bbdata$TEAM_PITCHING_SO>1173,1173,bbdata$TEAM_PITCHING_SO)


# Team PITCHING SO; mean = 818; MEDIAN = 814
bbdata$TEAM_PITCHING_SO <- ifelse(is.na(bbdata$TEAM_PITCHING_SO),818,bbdata$TEAM_PITCHING_SO)

### Define a new variable
bbdata$TEAM_BASES_EARNED <- 4*bbdata$TEAM_BATTING_HR
                         + 3*bbdata$TEAM_BATTING_3B
                         + 2*bbdata$TEAM_BATTING_2B
                         + 1*bbdata$TEAM_BATTING_1B
                         + 1*bbdata$TEAM_BATTING_BB
                         + 1*bbdata$TEAM_BASERUN_SB
                         - 1*bbdata$TEAM_BASERUN_CS
boxplot(TEAM_BASES_EARNED~wins,data=bbdata,col=rainbow(5))

### Transform Skewed Variables
### Taking the logarithm usually does the trick. However, make sure to "add 1" because
### for cases where the variable is zero, log becomes undefined. 
### Hence, adding 1 will avoid this scenario without affecting the overall analysis

bbdata$lnTF_E <- log(bbdata$TEAM_FIELDING_E+10) 

hist(bbdata$lnTF_E, prob=T, main="Histogram of (log) Team Fielding E", xlab="PCT", ylab=NULL)
curve(dnorm(x, mean=mean(bbdata$lnTF_E), sd=sd(bbdata$lnTF_E)), add=T, col="red")

######################################################################
### MODELLING STEPS
### It is essential to divide the data set into training and test sets.
### The idea is to develop a model based on the training set,
### then apply this model on the testing set.
### In this case, we can determine both the in-sample and out-of-sample performance metrics

# Split the data into Training and Testing Sets
set.seed(123)  # The "seed number" is to enable us to do a reproducible research as the
               # random number will be fixed
test <- sample(nrow(bbdata),0.3*nrow(bbdata))  # Take a random sample of 30% of the data as "test"

data.train <- bbdata[-test,]  # All non-test data are classified as part of the training set
data.test <- bbdata[test,]    # Testing set (out-of-sample data; holdout; data not yet seen)

nrow(data.train)
nrow(data.test)

### Linear Models
### (1) Simple Linear Regression (y vs x)

lm.1 <- lm(TARGET_WINS~TEAM_BASES_EARNED, data=data.train)
# The lm() function uses the ff arguments: lm(y~x, data = , ...)
# To add more independent variables, use the + sign: y ~ x1 + x2 + ...

# The following provides the results:
lm.1
summary(lm.1)

# Use the model on the training set:
lm1.train <- predict(lm.1,data=data.train)

# Show the mean squared error (MSE): known actual - predicted
mean((lm1.train-data.train$TARGET_WINS)^2) # 247.04; ~16 wins; 

# Similarly, use the model on the testing set and solve for the mean squared error:
lm1.test <- predict(lm.1,data.test)
mean((lm1.test-data.test$TARGET_WINS)^2) # 224.6544; ~15 wins

&&&&&&&&&&&&&&&&&&&&&&&&
lm.1a <- lm(TARGET_WINS~TEAM_BATTING_H+lnTF_E, data=data.train)
# The lm() function uses the ff arguments: lm(y~x, data = , ...)
# To add more independent variables, use the + sign: y ~ x1 + x2 + ...

# The following provides the results:
lm.1a
summary(lm.1a)

# Use the model on the training set:
lm1a.train <- predict(lm.1a,data=data.train)

# Show the mean squared error: known actual - predicted
mean((lm1a.train-data.train$TARGET_WINS)^2) # 200.5054; ~14 wins

# Similarly, use the model on the testing set and solve for the mean squared error:
lm1a.test <- predict(lm.1a,data.test)
mean((lm1a.test-data.test$TARGET_WINS)^2) # 181.3521; ~14 wins
&&&&&&&&&&&&&&&&&&&&&&&&
lm.mult <- lm(TARGET_WINS~., data=data.train)

### (2) Multiple LR
lm.2 <- lm(TARGET_WINS~.-INDEX-wins-TEAM_BATTING_HBP-TEAM_FIELDING_E, data=data.train)
# In the lm() function, use y~. if you want to use all variables;
# Use - to remove independent variables

# In this example, we took out INDEX (not needed), HBP (mostly NAs), TEAM FIELDING E (transformed by log)

lm.2
summary(lm.2)

# Since the result shows NAs and multicollinearity, we further remove the variables causing these:
# For example, wins is not really a predictor but another representation for the dependent
# Also, if we are using TEAM_BASES_EARNED, we have to remove the components that make this up

bb.train <- data.train[,-c(1,3:7,9,10,11,16,18)]
str(bb.train)

lm.2 <- lm(TARGET_WINS~.,data=bb.train)
lm.2
summary(lm.2)

lm2.train <- predict(lm.2,data=data.train)
mean((lm2.train-data.train$TARGET_WINS)^2) # 186.301; 14.68 wins

lm2.test <- predict(lm.2,data.test)
mean((lm2.test-data.test$TARGET_WINS)^2) # 169.0368; 13.89 wins


###############################################################
# NOTES:

### Data Cleansing/Munging

# - NAs: replace by the mean/median
#       treshold as to the % of missing to the total number of cases (e.g. 10-25%?)
# 
# - Outliers: truncation - replace by a treshold
#             do log transformation
# Why log() ?
# y = ln (x) <- inverse of the exponential function
# y = log (x)
# e.g., the values can be: 1, 10, 100, 1000, 10,000, 100,000 
# but when we take the log, it will become: 0, 1, 2, 3, 4, 5


# - Transformations: define new variables


### Divide our data into:
#  training (70-80%) <- develop your model/algorithm based on this set
#  test (20-30%) <- use that on this set to judge if your model works well 

#  Models tend to overfit the data used to develop these, hence the model should be
#  used on previously unseen data (testing set)
#  This is especially true for Big Data due to the nature of having localized relationships

### Ways to evaluate your model:

# (1) Significance of the predictors - use the p-value

# (2) Significance of the model - use the p-value for the F-stat AND the R-squared value (for regression)

# (3) Error Terms on both the training and test sets - use the mean squared error









