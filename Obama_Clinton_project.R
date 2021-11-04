################### FIRST PART OF THE PROJECT ###################

# Load the data file directly from the internet
library(curl)
x <- curl(
  "https://raw.githubusercontent.com/awhiter/TeachingRepos/master/datasets/Ob
ama.csv")

elect.df  <- read.csv(x)

# Create new derived target attributes
elect.df$ObamaRate <- 100 * elect.df$Obama / elect.df$TotalVote

# Imputing missing values:
# Missing values for AverageIncome are replaced by the
# MedianIncome for that same record
elect.df$AverageIncome <- ifelse(is.na(elect.df$AverageIncome),
                                 elect.df$MedianIncome,
                                 elect.df$AverageIncome)

# Missing values for the following list of attributes
# are replaced by 0.
for (attr in c("Black","Asian","AmericanIndian","ManfEmploy",
               "Disabilities","DisabilitiesRate","FarmArea"))
{elect.df[[attr]] <- ifelse(is.na(elect.df[[attr]]),
                            0,
                            elect.df[[attr]])}

# There still remain several attributes with 1 or 2 missing values.
# It turns out that all these final missing values are in 2 records.
# The following codes removes these records entirely.
elect.df <- elect.df[is.na(elect.df$HighSchool)==FALSE,]
elect.df <- elect.df[is.na(elect.df$Poverty)==FALSE,]

#Convert ElectionDate column to the "Date" data type
elect.df$ElectionDate <- as.Date(elect.df$ElectionDate,
                                 format="%m/%d/%Y")
#Creating known and unknown data
elect.df.known <- elect.df[elect.df$ElectionDate <
                             as.Date("2/19/2008", format = "%m/%d/%Y"), ]
elect.df.unknown <- elect.df[elect.df$ElectionDate >=
                               as.Date("2/19/2008", format = "%m/%d/%Y"), ]

#Quick veiw of how many rows are in each data set
nrow(elect.df.known)
nrow(elect.df.unknown)

# Find the number of rows in the known dataset
nKnown <- nrow(elect.df.known)

# Set the seed for a random sample
#set.seed(201)

# Randomly sample 80% of the row indices in the known dataset
rowIndicesTrain <- sample(1:nKnown,
                          size = round(nKnown*0.8),
                          replace = FALSE)
# Split the training set into the training set and the test set using these
indices.
elect.df.training <- elect.df.known[rowIndicesTrain, ]
elect.df.test <- elect.df.known[-rowIndicesTrain, ]

# first set library path to include the following directory
# if running on Azure LinuxDataScience VM ...
.libPaths('/home/vmuser/R/x86_64-pc-linux-gnu-library/3.2')

# The Metrics package includes the mae and rmse functions.
# Install Metrics if needed..
# install.packages("Metrics")

library(Metrics)
library(MicrosoftML)
genError <- function(prediction, actual)
  cat('MAE =', signif(mae(actual,prediction),4),
      ' RMSE =', signif(rmse(actual,prediction),4), "\n")

################### GRAPHS:
#Correlation heatmap:
ClintonRate <- elect.df$ClintonRate
ObamaPercentMargin <- elect.df$ObamaPercentMargin
res<-cor(elect.df[,c(7:42)],use="complete.obs")
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)
#Correlation table:
targetCol <- which(names(elect.df)=="ObamaRate")
startCol <- which(names(elect.df)=="MalesPer100Females")
endCol <- which(names(elect.df)=="DisabilitiesRate")
sort(
  cor(elect.df[,c(targetCol, startCol:endCol)],use="complete.obs")[1,],
  decreasing = TRUE) [1:6]

#Aggregation Table:
elect.df$Winner <- ifelse(elect.df$Obama>elect.df$Clinton,
                          "Obama",
                          "Clinton")
aggregate(cbind(Bachelors, IncomeAbove75K, HighSchool, MedianIncome,
                Poverty) ~ Winner,
          data=elect.df,
          FUN=mean)

#Plot 2:
ggplot(Obama, aes(x=ObamaRate,y=AverageIncome)) +
  geom_point(aes(color=Region)) + geom_smooth(method='lm')

#Plot 3:
BlackPopulation <- elect.df$Black/100*elect.df$Pop
ggplot(elect.df, aes(x=Region, y=BlackPopulation))+
  geom_bar(stat="identity", color="black")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_minimal()


################### PREDICTION MODELS:

#Basic Lasso Regulization Analysis
lm.lasso <- glmnet(xknown, yknown, family = "gaussian")
plot(lm.lasso, xvar = "lambda", label = TRUE)
coef(lm.lasso, s = exp(0))

#Using cross validation to optimise Lamdfor Lasso Regulization Model
set.seed(101)
lm.lasso.cv <- cv.glmnet(xknown, yknown, nfolds = 5, family = "gaussian")

#This gives the value for an optimised lamda value
lm.lasso.cv$lambda.min
(minLogLambda <- log(lm.lasso.cv$lambda.min))

#Plotting optimised Lamda Lasso Regulization
plot(lm.lasso, xvar = "lambda", label = TRUE)
abline(v = log(lm.lasso.cv$lambda.min))
coef(lm.lasso.cv, s = "lambda.min")

# Coefficients of the regularized linear regression with an optimal lambda.
# Determining the generalised error rate for this lasso model
xtest <- as.matrix(elect.df.test[, startCol:endCol])
lm.lasso.cv.pred <- predict(lm.lasso.cv, newx = xtest, s = "lambda.min")
genError(lm.lasso.cv.pred, elect.df.test$ObamaRate)

#rpart graph
library(rpart)
library(rpart.plot)    # install.packages("rpart.plot") is needed
rt <- rpart(ObamaRate ~ HighSchool + Poverty + Bachelors + AverageIncome +
              UnemployRate,
            data = elect.df.training)  # Fits a regression tree.
prp(rt, type = 1, extra = 1) #Plots Tree

# GBM Boost:
library(gbm)
elect.df.boost <- elect.df.training
elect.df.boost$County <- NULL
elect.df.boost$State <- NULL
elect.df.boost$Region <- NULL
elect.df.boost$FIPS <- NULL
elect.df.boost$ElectionDate <- NULL
elect.df.boost$Obama <- NULL
elect.df.boost$Clinton <- NULL
elect.df.boost$ElectionType = NULL
obama.boost = gbm(ObamaRate ~ . , data = elect.df.boost, n.trees = 100,
                  shrinkage = 0.01, interaction.depth = 4)
obama.boost
summary(obama.boost, cBars = 5, order=TRUE)

# Random Forest:
library(randomForest)
obama.forest <- randomForest(ObamaRate ~ . , data = elect.df.boost,
                             importance=TRUE, ntree=100)

# RMSE 9.15224
obama.forest <- randomForest(ObamaRate ~ Black + HighSchool + Bachelors +
                               DisabilitiesRate + SocialSecurityRate + White + IncomeAbove75K + Poverty +
                               MedianIncome, data = elect.df.boost, importance=TRUE, ntree=100)

# RMSE 9.81717
obama.forest.pred = predict(obama.forest,newdata=elect.df.test)
rmse = sqrt(mean((elect.df.test$ObamaRate-obama.forest.pred)^2))
plot(obama.forest)
plot(elect.df.test$ObamaRate,pch=16)
points(obama.forest.pred, col = "blue", pch=4)

# SVM:
library(e1071)
model_svm <- svm(ObamaRate ~ Black + HighSchool + Bachelors +
                   DisabilitiesRate + SocialSecurityRate + White + IncomeAbove75K + Poverty +
                   MedianIncome + Homeowner + AverageIncome + UnemployRate, data)

# 9.71428
model_svm <- svm(ObamaRate ~ ., elect.df.boost)

# 9.5216
pred <- predict(model_svm, elect.df.test)
rmse = sqrt(mean((elect.df.test$ObamaRate-pred)^2))
plot(elect.df.test$ObamaRate,pch=16)
points(pred, col = "blue", pch=4)