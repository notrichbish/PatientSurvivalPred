#Import Library
library(smotefamily)
library(ROSE)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(naniar)
library(reshape)
library(corrplot)
library(dplyr)
library(caret)
library(rattle)
library(ElemStatLearn) 
library(klaR)
library(ggcorrplot)

set.seed(123)

#Import the data set
df <- read.csv("C:\\Users\\richa\\Downloads\\Data Mining Dataset\\dataset.csv",stringsAsFactors = TRUE)

df_clean = df

#removing redundant columns
df_clean$patient_id <- df_clean$hospital_id<- df_clean$icu_id <- df_clean$encounter_id <- df_clean$apache_4a_hospital_death_prob <- df_clean$apache_4a_icu_death_prob <- NULL

#Exploratory Data Analysis
#Identify Missing and NA Values in each columns
colSums(is.na(df_clean))
colSums(df_clean == "")

#visualise missing values
vis_miss(df, warn_large_data = FALSE)
gg_miss_var(df_clean) + labs(y = "Missing Values")


#Identify Outliers
boxplot(df_clean)$out
boxplot(df_clean$apache_3j_diagnosis, xlab = "apache_3j_diagnosis")
var = df_clean[,c("age", "icu_type", "map_apache", "d1_spo2_max", "h1_mbp_min", "aids")]
boxplot(var)


#Replace the blank values into NA values
df_clean[df_clean == ""] <- NA

#Remove all NA values
df_clean <- na.omit(df_clean)

#Check for NA and blank Values after removing
gg_miss_var(df_clean) + labs(y = "Missing Values")
colSums(is.na(df_clean))
colSums(df_clean == "")

#Check for duplicates
table(duplicated(df_clean))

summary(df_clean)
str(df_clean)

#Count mean for gender
male = df_clean$age[df_clean$gender == "M"]
mean(male)
female = df_clean$age[df_clean$gender == "F"]
mean(female)

##Data Visualization 

#Plot the target variable "hospital_death" in histogram
hist(df_clean$hospital_death, main = "Hospital Death Histogram", xlab = "Hospital Death")

#Plot "Apache 3j Bodysystem" based on Gender
ggplot(df_clean, aes(y = apache_3j_bodysystem)) +
  geom_bar(aes(fill = gender), position = position_stack(reverse = TRUE)) + 
  ggtitle("Apache 3j Bodysystem based on Gender") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Apache 2 Bodysystem" based on Gender
ggplot(df_clean, aes(y = apache_2_bodysystem)) +
  geom_bar(aes(fill = gender), position = position_stack(reverse = TRUE)) + 
  ggtitle("Apache 2 Bodysystem based on Gender") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Apache 2 Bodysystem" based on Ethnicity
ggplot(df_clean, aes(y = apache_2_bodysystem)) +
  geom_bar(aes(fill = ethnicity), position = position_stack(reverse = TRUE)) + 
  ggtitle("Apache 2 Bodysystem based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Apache 3j Bodysystem" based on Ethnicity
ggplot(df_clean, aes(y = apache_3j_bodysystem)) +
  geom_bar(aes(fill = ethnicity), position = position_stack(reverse = TRUE)) + 
  ggtitle("Apache 3j Bodysystem based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "hospital_death" based on Ethnicity
ggplot(df_clean, aes(x = hospital_death)) + geom_bar(aes(fill = ethnicity)) + 
  ggtitle("Hospital Death based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "hospital_death" based on Gender
ggplot(df_clean, aes(x = hospital_death)) + geom_bar(aes(fill = gender)) + 
  ggtitle("Hospital Death based on Gender") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Lymphoma" based on Ethnicity
ggplot(df_clean, aes(x = lymphoma)) + geom_bar(aes(fill = ethnicity)) + 
  ggtitle("Lymphoma based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "AIDS" based on Ethnicity
ggplot(df_clean, aes(x = aids)) + geom_bar(aes(fill = ethnicity)) + 
  ggtitle("AIDS based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Leukemia" based on Ethnicity
ggplot(df_clean, aes(x = leukemia)) + geom_bar(aes(fill = ethnicity)) + 
  ggtitle("Leukemia based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Immunosuppression" based on Ethnicity
ggplot(df_clean, aes(x = immunosuppression)) + geom_bar(aes(fill = ethnicity) )+ 
  ggtitle("Immunosuppression based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Cirrhosis" based on Ethnicity
ggplot(df_clean, aes(x = cirrhosis)) + geom_bar(aes(fill = ethnicity) )+ 
  ggtitle("Cirrhosis based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Diabetes Mellitus" based on Ethnicity
ggplot(df_clean, aes(x = diabetes_mellitus)) + geom_bar(aes(fill = ethnicity) )+ 
  ggtitle("Diabetes Mellitus based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Hepatic Failure" based on Ethnicity
ggplot(df_clean, aes(x = hepatic_failure)) + geom_bar(aes(fill = ethnicity) )+ 
  ggtitle("Hepatic Failure based on Ethnicity") + theme(plot.title = element_text(hjust = 0.5))

#Plot "Gender" based on Ethnicity
ggplot(df_clean, aes(x = gender)) + geom_bar(aes(fill = ethnicity))

#Plot pie chart for Gender
gender = df_clean %>% count(gender) %>% mutate(Freq = n/56935)
pie = ggplot(gender, aes(x="", y=Freq, fill=gender)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Freq*100), "%")), position = position_stack(vjust = 0.5))
pie = pie + scale_fill_manual(values=c("#55DDE0", "#33658A")) 
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Gender")
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
#Plot pie chart for Ethnicity 
ethnicity = df_clean %>% count(ethnicity) %>% mutate(Freq = n/56935)
pie_eth = ggplot(ethnicity, aes(x="", y=Freq, fill=ethnicity)) + geom_bar(stat="identity", width=1)
pie_eth = pie_eth + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Freq*100), "%")), position = position_stack(vjust = 0.5))
pie_eth = pie_eth + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 
pie_eth = pie_eth + labs(x = NULL, y = NULL, fill = NULL, title = "Ethnicity")
pie_eth = pie_eth + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))


#Correlation analysis

#Produce the correlation value for each variable within the dataset
corr = model.matrix(~0+., data=df_clean) %>% 
  cor(use="pairwise.complete.obs")

#plot the correlation between "d1_diasbp_noninvasive_min" and "d1_diasbp_min"
x = df_clean$d1_diasbp_noninvasive_min
y = df_clean$d1_diasbp_min
plot(x,y, xlab = "d1_diasbp_noninvasive_min", ylab = "d1_diasbp_min", main = "Correlation")
abline(lm(y~x))
cor.test(x,y, "two.sided", "pearson")

#plot the correlation between "d1_diasbp_noninvasive_max" and "d1_diasbp_max"
x = df_clean$d1_diasbp_noninvasive_max
y = df_clean$d1_diasbp_max

plot(x,y, xlab = "d1_diasbp_noninvasive_max", ylab = "d1_diasbp_max", main = "Correlation")
abline(lm(y~x))
cor.test(x,y, "two.sided", "pearson")

#plot the correlation between "gcs_verbal_apache" and "h1_mbp_max"
x = df_clean$gcs_verbal_apache
y = df_clean$h1_mbp_max

plot(x,y, xlab = "gcs_verbal_apache", ylab = "h1_mbp_max", main = "Lowest Correlation")
abline(lm(y~x))
cor.test(x,y, "two.sided", "pearson")

#plot the correlation between "h1_mbp_noninvasive_max" and "gcs_verbal_apache"
x = df_clean$h1_mbp_noninvasive_max
y = df_clean$gcs_verbal_apache
plot(x,y, xlab = "gcs_verbal_apache", ylab = "h1_mbp_noninvasive_max", main = "Lowest Correlation")
abline(lm(y~x))
cor.test(x,y, "two.sided", "pearson")

#Oversampling the data set
df_clean_over = ovun.sample(hospital_death~., data=df_clean, method="over", N=length(df_clean[df_clean$hospital_death==0,]$hospital_death)*2)$data
table(df_clean_over$hospital_death)

#Split into train and test data sets
set.seed(123)
ind = sample(2, nrow(df_clean_over), replace = TRUE, prob = c(0.70,0.30))
train = df_clean_over[ind==1,]
test = df_clean_over[ind==2,]

#shows that the target variable class is balanced due to oversampling
table(train$hospital_death)

#Convert target variable into factor type
train$hospital_death = as.factor(train$hospital_death)
test$hospital_death = as.factor(test$hospital_death)

##Naive Bayes Classifier
X_train = train[,-78]
y_train = train$hospital_death
X_test = test[,-78]
y_test = test$hospital_death

#train the naive bayes classifier and cross validate
nb_model = train(X_train, y_train, method = 'nb',  
                 trControl = trainControl(method = 'cv', number = 20))

#Predict the naive bayes model based on test dataset
nb_predict = predict(nb_model$finalModel, X_test)$class
confusionMatrix(nb_predict, test$hospital_death)

#Hyperparameter Tuning for Naive Bayes
#Remove the highly correlated variable
nb_train = train
nb_test = test
nb_train$elective_surgery<- nb_train$d1_diasbp_noninvasive_max<-nb_train$d1_sysbp_noninvasive_max<-nb_train$d1_sysbp_noninvasive_min<-nb_train$d1_diasbp_noninvasive_min<-nb_train$h1_mbp_max<-nb_train$h1_mbp_noninvasive_min<-NULL
nb_test$elective_surgery<- nb_test$d1_diasbp_noninvasive_max<-nb_test$d1_sysbp_noninvasive_max<-nb_test$d1_sysbp_noninvasive_min<-nb_test$d1_diasbp_noninvasive_min<-nb_test$h1_mbp_max<-nb_test$h1_mbp_noninvasive_min<-NULL

#Split into train and test
X_train_hyp = nb_train[,-71]
X_test_hyp = nb_test[,-71]
y_train_hyp = nb_train$hospital_death
y_test_hyp = nb_test$hospital_death

#Run the naive bayes again with new train and test dataset after removing highly correlated variables
nb_model_hyp = train(X_train_hyp, y_train_hyp, method = 'nb',  
                 trControl = trainControl(method = 'cv', number = 20))

#Predict the hyperparamtered naive bayes model
nb_predict_hyp = predict(nb_model_hyp$finalModel, X_test)$class

#Evaluate the hyperparameter Naive Bayes
confusionMatrix(nb_predict_hyp, nb_test$hospital_death)

#Plot Variable Importance on the Naive Bayes
X <- varImp(nb_model_hyp)
plot(X)

###Decision Tree Classifier
#Decision Tree with rpart
fit <- rpart(hospital_death~., data = train, method = 'class')
rpart.plot(fit, extra = 106)
rpart.plot(fit)

#Predict the Decision Tree Classifier
predictions = predict(fit, test, type = "class")
confusionMatrix(predictions, test$hospital_death)

#plot the complexity parameter
printcp(fit)
plotcp(fit)

#Decision Tree pruning
DT_rpart = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
DT_rpart
#plot the pruned decision tree
rpart.plot(DT_rpart)

#Predict pruned decision tree 
DT_pred = predict(DT_rpart, test, type = "class")
confusionMatrix(DT_pred, test$hospital_death)

#Train the Decision Tree model with K-fold CV 
dt_kcv_model = train(X_train, y_train, method = 'rpart',  
                 trControl = trainControl(method = 'cv', number = 10))

x_dt = df_clean[,-78]
y_dt = as.factor(df_clean$hospital_death)

#K-Fold CV Decision tree for the whole data set
df_clean$hospital_death = as.factor(df_clean$hospital_death)

df_kcv_model = train(hospital_death~., data = df_clean, method = 'rpart', 
                     trControl = trainControl(method = 'cv', number = 10))

#Plot the Decision Tree Model K-CV
rpart.plot(df_kcv_model$finalModel)


###Random Forest Classifier

#Train the Decision Tree model with K-fold Cross Validation (KCV) 
rf_kcv = train(hospital_death~., data = df_clean, method = 'rf', 
                     trControl = trainControl(method = 'cv', number = 10))

#Plot the random forest with KCV
plot(rf_kcv$finalModel, main = "Error Plot in Random Forest with K-Fold Cross Validation")

#Plot Variable Importance based on Mean Decrease Gini 
varImp(rf_kcv$finalModel)
varImpPlot(rf_kcv$finalModel)

#Train Random Forest model with Train data set and best mtry based on RF KCV
rf_train = randomForest(hospital_death~., data = train, mtry = 55)

#plot the random forest
plot(rf_train)

#Plot the variable importance
varImpPlot(rf_train)

#Predict the Random Forest model
rf_pred = predict(rf_train, test, type = "class")
confusionMatrix(rf_pred, test$hospital_death)
