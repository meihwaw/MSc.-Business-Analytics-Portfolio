#' Author: Mei Hwa Wong
#' Date: March 16, 2023
#' Purpose: A2 Assignment National City Bank - Customer Propensity Model

# load libraries
library(ggplot2)
library(ggthemes)
library(dplyr)
library(vtreat)
library(psych)
library(DataExplorer)
library(caret)
library(rpart.plot)
library(MLmetrics)
library(ranger)
library(randomForest)
library(rpart)
library(chron)
library(graphics)
options(scipen = 999)

# set working directory
setwd("~/Hult_Visualizing-Analyzing-Data-with-R/A2 National City Bank")

# data
curcustomers <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A2_NationalCityBank/training/CurrentCustomerMktgResults.csv')
datadict <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A2_NationalCityBank/dataDictionary.csv')

# supplementary data
vehicle <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A2_NationalCityBank/training/householdVehicleData.csv')
credit <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A2_NationalCityBank/training/householdCreditData.csv')
axiom <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A2_NationalCityBank/training/householdAxiomData.csv')

# prospective customers data for final prediction
prospcustomers <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/DD1_Case_Info/A2_NationalCityBank/ProspectiveCustomers.csv')

# to call out first few rows of all datasets
head(curcustomers)
head(vehicle)
head(credit)
head(axiom)
head(datadict)
names(prospcustomers)

# customers dataframe is train dataframe
# join current customers table with vehicle table
customers <- left_join(curcustomers, vehicle, by='HHuniqueID')

# join customers dataframe with credit table
customers <- left_join(customers, credit, by='HHuniqueID')

# join customers dataframe with axiom table
customers <- left_join(customers, axiom, by='HHuniqueID')


# prospective customers is test dataframe - final predictions comes from here
prospcustomers <- left_join(prospcustomers, vehicle, by ="HHuniqueID")

prospcustomers <- left_join(prospcustomers, credit, by = 'HHuniqueID')

prospcustomers <- left_join(prospcustomers, axiom, by='HHuniqueID')

names(prospcustomers)

# head of customers dataframe
head(customers)

# tail of customers dataframe
tail(customers)

# summary of customers dataframe
summary(customers)

# describe customers dataframe
describe(customers)

# identify null values
plot_missing(customers)

# shape of dataframe
dim(customers)

# unique columns
names(customers)

# plot density graphs for continuous variables
plot_density(customers)

# plot density graph for Age
plot(density(customers$Age), main = 'Age Distribution', 
     xlab = 'Age',
     ylab = 'Density',
     col = 'skyblue')

ggplot(customers, aes(x=Age, color=headOfhouseholdGender)) +
  geom_density(alpha=1.5) +
  ggtitle("Age and Gender Distributions")+
  xlab("Age") +
  ylab("Density") +
  scale_fill_manual(values = c("skyblue", "lightpink")) +
  theme_minimal()

plot(density(customers$RecentBalance), main = 'Customers Recent Balance Distribution',
     xlab = 'Recent Balance',
     ylab = 'Density',
     col = 'sienna2')

# create data frame for customers that have insurance
customers_filtered <- customers %>% filter(HHInsurance == 1)

# plot the relationship between age and customers with insurance
ggplot(customers_filtered, aes(x=Age, y=HHInsurance)) +
         geom_histogram(fill="orchid3", stat="identity") +
         labs(title="Own insurance by customers",
              xlab="Age",
              ylab="Insurance")

# exploring all columns
customers$past_Outcome
customers$carYr
customers$Education
customers$Job
customers$LastContactDay
customers$LastContactMonth
customers$NoOfContacts
customers$DaysPassed
customers$PrevAttempts
customers$CallStart
customers$CallEnd
customers$carMake
customers$carModel
customers$DefaultOnRecord
customers$RecentBalance
customers$HHInsurance
customers$CarLoan
customers$headOfhouseholdGender
customers$annualDonations # has a lot of "" - clean
customers$EstRace # has a lot of "" - clean
customers$PetsPurchases
customers$DigitalHabits_5_AlwaysOn
customers$AffluencePurchases
customers$Age
customers$Marital

# identify outliers
ggplot(data = customers, aes(y=LastContactDay)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1) 

ggplot(data = customers, aes(y=NoOfContacts)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1)

ggplot(data = customers, aes(y=DaysPassed)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1)

ggplot(data = customers, aes(y=PrevAttempts)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1)

ggplot(data = customers, aes(y=carYr)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1)

ggplot(data = customers, aes(y=RecentBalance)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1)

ggplot(data = customers, aes(y=Age)) + 
  geom_boxplot(outlier.color = "blue", outlier.shape=1,
               outlier.size=1)

plot_histogram(customers$carYr)

# data cleaning on customers data frame
customers_clean <- customers

summary(customers_clean)
names(customers_clean)

# function to find mode for categorical columns
calc_mode <- function(x) {
  uniquex <- unique(x)
  uniquex[which.max(tabulate(match(x, uniquex)))]
}

# mode for Job column
calc_mode(customers_clean$Job) #"management"

# fill na for Job column
customers_clean$Job <- ifelse(is.na(customers_clean$Job), 
                        "management", customers_clean$Job)


customers_clean$Job
# mode for Education column
calc_mode(customers_clean$Education) #secondary

# fill na for Education column
customers_clean$Education <- ifelse(is.na(customers_clean$Education),
                              "secondary", customers_clean$Education)

# fill na for CarYr column
calc_mode(customers_clean$carYr)

customers_clean$carYr <- ifelse(is.na(customers_clean$carYr),
                                "2019", customers_clean$carYr)


# replace "" with NA for annual donations column
customers_clean$annualDonations <- ifelse(customers_clean$annualDonations == "", NA, 
                                          customers_clean$annualDonations)

# replace "" with NA for EstRace column
customers_clean$EstRace <- ifelse(customers_clean$EstRace == "", NA,
                                  customers_clean$EstRace)

# group age 
customers_clean$Age <- cut(customers_clean$Age,
                           breaks = c(0, 20, 25, 30, 35, 40, Inf),
                           label = c("under 20", "20-25", "25-30", "30-35", "35-40", "above 45"))

# remove outliers
max(customers_clean$RecentBalance) #98417

customers_clean <- subset(customers_clean, customers_clean$RecentBalance != 98417)

# group recent balance
customers_clean$RecentBalance <- cut(customers_clean$RecentBalance,
                                     breaks = c(-Inf, 0, 1000, 2000, 3000, 4000, 5000, Inf),
                                     label = c("negative balance", "0 - 1000", "1000 - 2000",
                                               "2000 - 3000", "3000 - 4000", "4000 - 5000", "above 5000"))

##
# data cleaning on prospective customers data frame
prospcustomers_clean <- prospcustomers

summary(prospcustomers_clean)

# mode for Job column
calc_mode(prospcustomers_clean$Job) #"management"

# fill na for Job column
prospcustomers_clean$Job <- ifelse(is.na(prospcustomers_clean$Job), 
                              "management", prospcustomers_clean$Job)

# mode for Education column
calc_mode(prospcustomers_clean$Education) #secondary

# fill na for Education column
prospcustomers_clean$Education <- ifelse(is.na(prospcustomers_clean$Education),
                                    "secondary", prospcustomers_clean$Education)

# fill na for CarYr column
calc_mode(prospcustomers_clean$carYr)

prospcustomers_clean$carYr <- ifelse(is.na(prospcustomers_clean$carYr),
                                "2019", prospcustomers_clean$carYr)

# replace "" with NA for annual donations column
prospcustomers_clean$annualDonations <- ifelse(prospcustomers_clean$annualDonations == "", NA, 
                                          prospcustomers_clean$annualDonations)

# replace "" with NA for EstRace column
prospcustomers_clean$EstRace <- ifelse(prospcustomers_clean$EstRace == "", NA,
                                  prospcustomers_clean$EstRace)

# group age 
prospcustomers_clean$Age <- cut(prospcustomers_clean$Age,
                           breaks = c(0, 20, 25, 30, 35, 40, Inf),
                           label = c("under 20", "20-25", "25-30", "30-35", "35-40", "above 45"))

# group recent balance
prospcustomers_clean$RecentBalance <- cut(prospcustomers_clean$RecentBalance,
                                          breaks = c(-Inf, 0, 1000, 2000, 3000, 4000, 5000, Inf),
                                          label = c("negative balance", "0 - 1000", "1000 - 2000",
                                                    "2000 - 3000", "3000 - 4000", "4000 - 5000", "above 5000"))


##### Date Preparation for Modeling #####
keepData <- c("Communication",
              "LastContactDay",
              "LastContactMonth",
              "NoOfContacts",
              "DaysPassed",
              "PrevAttempts",
              "past_Outcome",
              #"CallStart",
              #"CallEnd",
              "carMake",
              "carModel",
              "carYr",
              "DefaultOnRecord",
              "RecentBalance",
              "HHInsurance",
              "CarLoan",
              "headOfhouseholdGender",
              "annualDonations",
              "EstRace",
              "PetsPurchases",
              "DigitalHabits_5_AlwaysOn",
              "AffluencePurchases",
              "Age",
              "Job",
              "Marital",
              "Education",
              "Y_AcceptedOffer")

# 75% training & 15% validation & 10% test
set.seed(1203)
trainPercentRows      <- round(nrow(customers_clean) %*% .75)
trainPercentRows
validationPercentRows <- round(nrow(customers_clean) %*% .15)
validationPercentRows

# Sample index for training
trainIdx <- sample(1:nrow(customers_clean), trainPercentRows)

# Identify the rows not in the training set, its the "difference" 
remainingRows <- setdiff(1:nrow(customers_clean), trainIdx)

# Create another sample but limit the row numbers to only those identified as *not* in training to get the validation index
validationIdx <-sample(remainingRows, validationPercentRows)

# With the two idx vectors of randomly generated numbers, without any overlap you can put them in the "row" position for indexing. 
trainSet      <- customers_clean[trainIdx,  names(customers_clean) %in% keepData]
validationSet <- customers_clean[validationIdx, names(customers_clean) %in% keepData]

# Prepping data by creating test set.
prepData <- customers_clean[-c(trainIdx, validationIdx), names(customers_clean) %in% keepData]

### THIS IS CLASSIFICATION SO C IS USED
plan <- designTreatmentsC(dframe        = prepData, 
                          varlist       = c("Communication",
                                            "LastContactDay",
                                            "LastContactMonth",
                                            "NoOfContacts",
                                            "DaysPassed",
                                            "PrevAttempts",
                                            "past_Outcome",
                                            "CallStart",
                                            "CallEnd",
                                            "carMake",
                                            "carModel",
                                            "carYr",
                                            "DefaultOnRecord",
                                            "RecentBalance",
                                            "HHInsurance",
                                            "CarLoan",
                                            "headOfhouseholdGender",
                                            "annualDonations",
                                            "EstRace",
                                            "PetsPurchases",
                                            "DigitalHabits_5_AlwaysOn",
                                            "AffluencePurchases",
                                            "Age",
                                            "Job",
                                            "Marital",
                                            "Education"),
                          outcomename   = "Y_AcceptedOffer",
                          outcometarget = "Accepted")

# apply the plan to both sections for modeling and evaluation next
treatedTrain <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)

# summary
summary(treatedTrain)
summary(treatedValidation)

##### Logistic Regression #####
# datafit
fit <- glm(as.factor(Y_AcceptedOffer) ~ ., treatedTrain, family = 'binomial')

# first model
summary(fit)

# parsimonious
parsimonyFit <- step(fit, direction = 'backward')

# make prediction
acceptanceProbability <- predict(parsimonyFit, treatedValidation, type = 'response')

head(acceptanceProbability)

# create a prediction dataframe
predictionDf <- data.frame(actual = treatedValidation$Y_AcceptedOffer,
                           probs = acceptanceProbability)

# plot prediction
ggplot(predictionDf, aes(x=probs, group=actual, color=actual)) + geom_density()

# assess = calculate accuracy, plot the ROC and make a confusion matrix
cutoff <- 0.65
predClass <- ifelse(acceptanceProbability>=cutoff, 'Accepted', 'DidNotAccept')
table(treatedValidation$Y_AcceptedOffer, predClass)

confusionmatrix_glm <- confusionMatrix(as.factor(predClass), as.factor(treatedValidation$Y_AcceptedOffer))

# print accuracy and F1 score
cat("Accuracy:", confusionmatrix_glm$overall['Accuracy'], "\n")
cat("F1 Score:", confusionmatrix_glm$byClass['F1'], "\n")

accuracy_glm <- confusionmatrix_glm$overall['Accuracy']
f1_glm <- confusionmatrix_glm$byClass['F1']

accuracy_glm
f1_glm

##### Decision Tree #####
set.seed(1234)
fit_df <- train(as.factor(Y_AcceptedOffer) ~., 
             data = treatedTrain,
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) 

# calling out fit
fit_df

# Plot the CP Accuracy Relationship to adjust the tuneGrid inputs
plot(fit_df)

# Plot a pruned tree
prp(fit_df$finalModel, extra = 1)

# Make some predictions on the training set
trainCaret_df <- predict(fit_df, treatedTrain)
head(trainCaret_df)

# Get the confusion matrix
confusionMatrix_df <- confusionMatrix(trainCaret_df, as.factor(treatedTrain$Y_AcceptedOffer))
confusionMatrix_df

# Now more consistent accuracy & fewer rules!
testCaret_df <- predict(fit_df,treatedValidation)
validation_cm_df <- confusionMatrix(testCaret_df,as.factor(treatedValidation$Y_AcceptedOffer))
validation_cm_df

# create accuracy table
accuracy_dt <- validation_cm_df$overall['Accuracy']

# create f1 table
f1_dt <- validation_cm_df$byClass['F1']

# call out accuracy for decision tree
accuracy_dt

# call out f1 score for decision tree
f1_dt

# creating variable importance for decision tree model
var_imp_df <- varImp(fit_df)

# to call out var_imp_df
var_imp_df

# plot var_imp_df
plot(var_imp_df)

# extract top 10 variables
top_var_imp <- var_imp_df$importance %>%
  as.data.frame() %>%
  top_n(10, Overall)

# call out newly created dataframe
top_var_imp

# create barplot for top 10 variables
ggplot(top_var_imp %>% arrange(desc(Overall)), aes(x=row.names(top_var_imp), y = Overall)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(x='Variable', y='Importance score') +
  ggtitle('Top 10 Variable Importance score')

##### Random Forest #####
# fit random forest without ranger
downSampleFit <- train(Y_AcceptedOffer ~ .,
                       data = treatedTrain,
                       method = "rf",
                       verbose = FALSE,
                       ntree = 100,
                       tuneGrid = data.frame(mtry=c(2,4,6)))

# call out down sample fit
downSampleFit

# to find out probabilities
predProbs_dsf <- predict(downSampleFit,
                     treatedValidation,
                     type = c("prob"))

predProbs_dsf
# To get classes with 0.50 cutoff
predClass_dsf <- predict(downSampleFit,  treatedValidation)

# call out prediction class for down sample fit
predClass_dsf

# create confusion matrix for down sample fit
confusionMatrix_dsf <- caret::confusionMatrix(predClass_dsf, as.factor(treatedValidation$Y_AcceptedOffer))

# to call out confusion matrix for down sample fit
confusionMatrix_dsf

# to create f1 score for down sample fit
f1_dsf <- confusionMatrix_dsf$byClass['F1']

# to call out f1 score for down sample fit
f1_dsf

# Other interesting model artifacts
var_imp_dsf <- varImp(downSampleFit)
plot(var_imp_dsf, top = 10)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough)
moreAcceptance <- randomForest(as.factor(Y_AcceptedOffer) ~ .,
                           data  = treatedTrain, 
                           ntree = 500,
                           mtry  = 6)

# Confusion Matrix, compare to 3 trees 
# create prediction class
predClass_ma <- predict(moreAcceptance, treatedValidation)

# create confusion matrix
confusionMatrix_ma <- confusionMatrix(predClass_ma, as.factor(treatedValidation$Y_AcceptedOffer))

# call out confusion matrix
confusionMatrix_ma

# create f1 score for more acceptance
f1_ma <- confusionMatrix_ma$byClass['F1']

# call out new data frame
f1_ma

#observe improved var importance
varImpPlot(moreAcceptance)

# plot the RF with a legend
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(moreAcceptance, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(moreAcceptance$err.rate),col=1:4,cex=0.8,fill=1:4)

# ntree = 250
someAcceptance <- randomForest(as.factor(Y_AcceptedOffer) ~ .,
                           data = treatedTrain, 
                           ntree=250,
                           mtyr = 6)

# observe improved var importance
varImpPlot(someAcceptance)

# Confusion Matrix
predClass_sa <- predict(someAcceptance, treatedValidation)
confusionMatrix_sa <- confusionMatrix(predClass_sa, as.factor(treatedValidation$Y_AcceptedOffer))
confusionMatrix_sa

# create f1 some acceptance confusion matrix
f1_sa <- confusionMatrix_sa$byClass['F1']

# to call out the f1_sa confusion matrix
f1_sa

# Accuracy Comparison from MLmetrics and natural occurence in the test set
accuracy_dsf_rf <- Accuracy(validationSet$Y_AcceptedOffer, predClass_dsf)
accuracy_ma_rf <- Accuracy(validationSet$Y_AcceptedOffer, predClass_ma)
accuracy_sa_rf <- Accuracy(validationSet$Y_AcceptedOffer, predClass_sa)

# to call out accuracy for down simple fit, more acceptance and some acceptance frame
accuracy_dsf_rf
accuracy_ma_rf
accuracy_sa_rf

# propotions on validation set 
proportions(table(validationSet$Y_AcceptedOffer))

# create a new table to display accuracy and f1-score for all ntrees
accuracy_f1_df_table <- data.frame(
  accuracy_type = c("100", "500", "250"),
  accuracy = c(accuracy_dsf_rf, accuracy_ma_rf,
               accuracy_sa_rf),
  f1_score = c(f1_dsf, f1_ma, f1_sa)
)
print(accuracy_f1_df_table)

# comparing models 
models <- data.frame(model_type = c("logistic regression", "decision tree", "random forest"),
                     accuracy = c(accuracy_glm, accuracy_dt, accuracy_sa_rf),
                     f1_score = c(f1_glm, f1_dt, f1_sa))

# call out models data frame
models

# applying vtreat in prospective customers dataset
prospcustomers_treated <- prepare(plan, prospcustomers_clean)
summary(prospcustomers_treated)

# apply model to predict
prediction <- predict(someAcceptance, prospcustomers_treated)
prediction

# probability of prediction
prob_prediction <- predict(someAcceptance, prospcustomers_treated, type = 'prob')
prob_prediction

# create dataframe for prediction
prediction_df <- data.frame(Predicted_Class = prediction, 
                            Predicted_Probability = apply(prob_prediction,1,max))

# add new prediction and prob_prediction data frame to prospective customers dataset
prospcustomers_clean$Y_AcceptedOffer <- prediction_df$Predicted_Class

prospcustomers_clean$Y_AcceptedOffer_prob <- prediction_df$Predicted_Probability

prospcustomers_clean$Y_AcceptedOffers

prospcustomers_clean
# filter to only accepted
accepted_offer <- prospcustomers_clean[prospcustomers_clean$Y_AcceptedOffer == "Accepted",]

# sort accepted_offer by probability
sort_offer <- accepted_offer[order(-accepted_offer$Y_AcceptedOffer_prob),]

# create new data frame with top 100 observations
prospcustomers_top100 <- sort_offer[1:100,]
prospcustomers_top100

# export prospcustomers_top100 to csv file
write.csv(prospcustomers_top100, "prospective_customers_top100.csv", row.names = FALSE)

# read top 100 customers csv
top100cust <- read.csv('~/Hult_Visualizing-Analyzing-Data-with-R/A2 National City Bank/prospective_customers_top100.csv')
top100cust

## Insights ##
##### Insight 1 #####
top100cust$Age

# identify number of acceptance by age group
age_accept <- top100cust %>% 
  group_by(Age) %>% 
  summarize(num_acceptance = sum(Y_AcceptedOffer=="Accepted"),
            total_customers = n())

# to call out age_accept dataframe
age_accept

# plot bar chart 
ggplot(age_accept, aes(x=Age, fill=num_acceptance)) +
  geom_bar(position = "dodge") +
  labs(title = "Acceptance by Age")

# summarize age, household gender and acceptance
age_gender_accept <- top100cust %>%
  group_by(Age, headOfhouseholdGender, Y_AcceptedOffer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(accepted = factor(Y_AcceptedOffer, levels = c("No", "Yes")))

# plot stacked bar chart
ggplot(age_gender_accept, aes(x=Age, y=count, fill=headOfhouseholdGender)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~ headOfhouseholdGender) +
  scale_fill_manual(values=c("pink", "skyblue")) +
  labs(title = "Acceptance by Gender and Age")

##### Insight 2 #####
# last contact day filter
cust_lastcontactday <- customers_clean$LastContactDay

# create new data frame with table
cust_lastcontactday_freqtable <- table(cust_lastcontactday)

# call output for new dataframe
cust_lastcontactday_freqtable

# create barplot
barplot(cust_lastcontactday_freqtable,
        main = "Frequency of Last Contact Day",
        xlab = "Last Contact Day",
        ylab = "Frequency",
        col = "lavender")

# plot histogram for customer last contact day
hist(cust_lastcontactday,
     main = "Histogram of Last Contact Day",
     xlab = "Last Contact Day",
     col = "seagreen")

# filter by acceptance - accepted
cust_accepted <- customers_clean %>% filter(Y_AcceptedOffer == "Accepted")

# plot chart to show which last contact day have a higher acceptance
ggplot(cust_accepted, aes(x=LastContactDay, fill=Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  labs(title = 'Number of acceptance depending on Last Contact Day') +
  scale_fill_manual(values=c('mistyrose3'))

# plot for top 100 customers
ggplot(top100cust, aes(x=LastContactDay, fill=Y_AcceptedOffer)) +
  geom_bar(position = "dodge") +
  labs(title = 'Number of acceptance depending on Last Contact Day from predictions') +
  scale_fill_manual(values=c('khaki3'))

##### Insight 3 #####
# probability of acceptance according to car year
prob_acceptance <- aggregate(Y_AcceptedOffer_prob ~ carYr, 
                             data = top100cust, mean)

# to call out new dataframe
prob_acceptance

# plot chart for car year and probability of line of credit acceptance
ggplot(prob_acceptance, aes(x=carYr, y=Y_AcceptedOffer_prob)) +
  geom_bar(fill = 'darkolivegreen4', stat = "identity") +
  labs(title = 'Mean probability of line of credit acceptance according to Car Year') +
  xlab('Car Year') +
  ylab('Mean probability of acceptance')

# calculate age of car
top100cust <- top100cust %>%
  mutate(car_age = 2019 - carYr)

# create new dataframe to label age of car
acceptance_rates_caryear <- top100cust %>%
  group_by(car_age) %>%
  summarise(mean_acceptance = mean(Y_AcceptedOffer_prob)) %>%
  mutate(car_age_10 = ifelse(car_age > 10, "Yes", "No"))

# call out newly created dataframe
acceptance_rates_caryear

# plot acceptance rates car year by car_age
ggplot(acceptance_rates_caryear, aes(x=car_age_10, y= mean_acceptance, 
                                     fill = car_age_10)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('lightcyan3','snow3')) +
  labs(title = "Acceptance Rates by Car Age",
       x = "Car age more than 10 years",
       y = "Mean acceptance rate"
       )

# End