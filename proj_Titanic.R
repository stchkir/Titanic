# "https://www.kaggle.com/c/titanic/overview"

###### PACKAGES #######
library("tidyverse")
library("caret")
library("RColorBrewer")
library("here")
library("rpart.plot")

###### DATA ###########
path <- here()
setwd(path)

test <- read.csv("https://raw.githubusercontent.com/stchkir/Titanic/master/test.csv")
train <- read.csv("https://raw.githubusercontent.com/stchkir/Titanic/master/train.csv")
example <- read.csv("https://raw.githubusercontent.com/stchkir/Titanic/master/gender_submission.csv")

test_origin <- test
train_origin <- train

###### DATA EXPLORATION AND CLEANING #####
str(train)
summary(train)
head(train)

# Check NAs
train %>% sapply(is.na) %>% colSums()
test %>% sapply(is.na) %>% colSums()

# Calculate and show survival rate by Sex and Pclass
sex_class <- train %>% group_by(Pclass,Sex) %>% summarize(SRate=sum(Survived)/n())
sex_class %>% ggplot(aes(as.factor(Pclass),SRate)) + 
  geom_point(aes(col=Sex),size=5) +
  xlab("Passenger Class") +
  ylab("Survival Rate") 

# Calculate and show survival rate by Age, Sex and Pclass
sex_class_age <- train %>% group_by(Pclass,Sex,Age) %>% summarize(SRate=sum(Survived)/n())
sex_class_age %>% ggplot(aes(Age,SRate)) + 
  geom_point() +
  geom_smooth() +
  facet_grid(Pclass~Sex,margins=FALSE) +
  xlab("Age") +
  ylab("Survival Rate") +
  ylim(0,1)

# Calculate and show survival rate by Sex and Port
port <- train %>% group_by(Embarked,Sex) %>% summarize(SRate=sum(Survived)/n())
port %>% ggplot(aes(Embarked,SRate)) + 
  geom_point(aes(col=Sex),size=5) +
  xlab("Port of Embarkation") +
  ylab("Survival Rate") 

# Show fare by Age, Embarked and Pclass
train %>% ggplot(aes(Age,Fare)) + 
  geom_point() +
  geom_smooth() +
  facet_grid(Pclass~Embarked,margins=FALSE) +
  xlab("Age") +
  ylab("Fare") +
  ylim(0,200)

###### DATA CLEANING TRAIN SET #####
# Define PClass and Survived as Factor for Classification
train <- train %>% mutate(Pclass=factor(Pclass),
                          Survived=factor(Survived))

# Identify factor levels
Pclass.levels <- levels(train$Pclass)
Embarked.levels <- levels(train$Embarked)

# Calculate median Age as average per Pclass and Sex
age_medians <- train %>% filter(!is.na(Age)) %>%
  group_by(Pclass,Sex) %>% summarize(Age_median=median(Age))

print(age_medians)

# Calculate median Fare per Pclass and Embarked
fare_medians <- train %>% filter(!is.na(Fare)) %>%
  group_by(Pclass, Embarked) %>% summarize(Fare_median=median(Fare))

print(fare_medians)

# Replace NAs with median Age per group
train <- train %>% left_join(y=age_medians,by=c("Pclass","Sex"))
train <- train %>% mutate(Age = ifelse(is.na(Age),Age_median,Age))

###### DATA CLEANING TEST SET #####
# Define factor levels similar to train set
test <- test %>% mutate(Pclass=factor(Pclass, levels=Pclass.levels),
                        Embarked=factor(Embarked, levels=Embarked.levels))

# Replace NAs with median Age per group
test <- test %>% left_join(y=age_medians,by=c("Pclass","Sex"))
test <- test %>% mutate(Age = ifelse(is.na(Age),Age_median,Age))

###!!! Test if Age missing mostly for kids (high survival rate) !!!

# Replace NAs with median Fare per group
test <- test %>% left_join(y=fare_medians,by=c("Pclass","Embarked"))
test <- test %>% mutate(Fare = ifelse(is.na(Fare),Fare_median,Fare))

###### PREDICTION #####
# Use sex only
sex_predict <- train %>% select(Survived,Sex) %>%
  mutate(predict=ifelse(Sex=="female",1,0))

mean(sex_predict$predict==train$Survived)

Survived_predict <- data.frame(sex=example)
Survived_predict <- Survived_predict %>% rename(PassengerId=sex.PassengerId,
                                                Survived=sex.Survived)
# Use k nearest neighbors
knn.model <- train(Survived ~ Pclass+Sex+Age, method = "knn", data = train,
             tuneGrid = data.frame(k = seq(1, 20, 1))
             )
             
knn.model$results

Survived_predict <- Survived_predict %>% mutate(knn=predict(knn.model, test, type = "raw"))

# Use decision trees
tree.model <- train(Survived ~ Pclass+Sex+Age, method = "rpart", data = train,
              tuneGrid = data.frame(cp = seq(0.002, 0.02, 0.002)))

tree.model$results
rpart.plot(tree.model$finalModel)

Survived_predict <- Survived_predict %>% mutate(tree=predict(tree.model, test, type = "raw"))

# Use random forest
control <- trainControl(method = "cv", number = 10, p = .9) # 5 cross validation samples with .9 of data
forest.model <- train(Survived ~ Pclass+Sex+Age, method="rf", data = train, 
                tuneGrid = data.frame(mtry = 3),
                ntree=15)

forest.model$results

Survived_predict <- Survived_predict %>% mutate(forest=predict(forest.model, test, type = "raw"))


#### SUBMISSION #####
submission <- Survived_predict %>% select(PassengerId,forest) %>% rename(Survived=forest)
write.csv(submission,"C:/Users/Stephan/Documents/R/projects/proj_Titanic/submission.csv",row.names = FALSE)

