# "https://www.kaggle.com/c/titanic/overview"

###### PACKAGES #######
library("tidyverse")
library("caret")
library("RColorBrewer")
library("here")
library("rpart.plot")
library("stringr")

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

# Calculate general survival rate
total <- train %>% summarize(No=n(),SRate=sum(Survived)/n())

# Calculate and show survival rate by Sex and Pclass
sex_class <- train %>% group_by(Pclass,Sex) %>% summarize(Share=n()/total$No,SRate=sum(Survived)/n())
sex_class %>% ggplot(aes(as.factor(Pclass),SRate)) + 
  geom_point(aes(col=Sex),size=5) +
  xlab("Passenger Class") +
  ylab("Survival Rate") 

# Calculate and show survival rate by Age, Sex and Pclass
sex_class_age <- train %>% group_by(Pclass,Sex,Age) %>% summarize(Share=n()/total$No,SRate=sum(Survived)/n())
sex_class_age %>% ggplot(aes(Age,SRate)) + 
  geom_point() +
  geom_smooth() +
  facet_grid(Pclass~Sex,margins=FALSE) +
  xlab("Age") +
  ylab("Survival Rate") +
  ylim(0,1)

# Calculate and show survival rate by Sex and Port
port <- train %>% group_by(Embarked,Sex) %>% summarize(Share=n()/total$No,SRate=sum(Survived)/n())
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

# Calculate survival rate for passengers with unknown age
train <- train %>% mutate(Age.info=as.factor(ifelse(is.na(Age),0,1))) 
age_unknown <- train %>% group_by(Age.info) %>% 
  summarize(Share=n()/total$No,SRate=sum(Survived)/n())

print(age_unknown)

# Calculate survival rate by Sex and Pclass for passengers with unknown age
sex_class_noage <- train %>% group_by(Pclass,Sex,Age.info) %>% 
  summarize(Share=n()/total$No,SRate=sum(Survived)/n())

print(sex_class_noage)

# Change Name to character and extraxt Title
train <- train %>% mutate(Name.char = as.character(Name), 
                          Title = factor(case_when(str_detect(Name.char,"Mr\\.")~"Mr",
                                                   str_detect(Name.char,"Mrs\\.")~"Mrs",
                                                   str_detect(Name.char,"Miss\\.")~"Miss",
                                                   str_detect(Name.char,"Master\\.")~"Master",
                                                   TRUE ~ "other")))

train[train$Title=="other","Name.char"]

train %>% filter(!is.na(Age)) %>% group_by(Pclass,Sex,Title) %>% 
  summarise(Age=mean(Age))

# Calculate survival rate by Title and Sex
title <- train %>% group_by(Sex, Title) %>% summarize(Share=n()/total$No,SRate=sum(Survived)/n())

print(title)

# Split cabins into areas and rooms
train <- train %>% mutate(Cabin=as.character(Cabin)) %>% 
  separate(Cabin, into=c("Cabin1", "CabinRest"), sep="[\\s]+", remove=FALSE, extra="merge") %>%
  separate(Cabin1, into=c("Area1", "Room1"), sep="(?<=[A-Za-z])(?=[0-9])", remove=FALSE) %>%
  mutate(Area1=factor(Area1),Room1=ifelse(is.na(Room1),0,as.integer(Room1)))

area <- train %>% group_by(Area1,Sex) %>% summarize(Share=n()/total$No,SRate=sum(Survived)/n())
area %>% ggplot(aes(Area1,SRate)) + 
  geom_point(size=5) +
  facet_grid(~Sex,margins=FALSE) +
  xlab("Area") +
  ylab("Survival Rate") 

###### DATA CLEANING TRAIN SET #####
# Define PClass and Survived as Factor for Classification
train <- train %>% mutate(Pclass=factor(Pclass),
                          Survived.class=factor(Survived))

# Identify factor levels
Pclass.levels <- levels(train$Pclass)
Embarked.levels <- levels(train$Embarked)
Title.levels <- levels(train$Title)
Area.levels <- levels(train$Area1)

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
train <- train %>% mutate(Age.predict = ifelse(is.na(Age),Age_median,Age))

# Replace NAs with median Fare per group
train <- train %>% left_join(y=fare_medians,by=c("Pclass","Embarked"))
train <- train %>% mutate(Fare.predict = ifelse(is.na(Fare),Fare_median,Fare))

# Estimate Age for NAs using RANDOM FOREST
train.age <- train %>% filter(!is.na(Age))

control <- trainControl(method = "cv", number = 15, p = .85) # 5 cross validation samples with .9 of data
forest.model.age <- train(Age ~ Pclass+Sex+SibSp+Parch+Title, 
                      method="rf", data = train.age, 
                      tuneGrid = data.frame(mtry = c(2,3,4,5)),
                      ntree=15)

forest.model.age$results

train <- train %>% mutate(Age.predict2=ifelse(is.na(Age),
                                              predict(forest.model.age, train, type = "raw"),
                                              Age))

###### DATA CLEANING TEST SET #####
# Define factor levels similar to train set
test <- test %>% mutate(Pclass=factor(Pclass, levels=Pclass.levels),
                        Embarked=factor(Embarked, levels=Embarked.levels))

# Replace NAs with median Age per group
test <- test %>% mutate(Age.info=as.factor(ifelse(is.na(Age),0,1))) 
test <- test %>% left_join(y=age_medians,by=c("Pclass","Sex"))
test <- test %>% mutate(Age.predict = ifelse(is.na(Age),Age_median,Age))

# Replace NAs with median Fare per group
test <- test %>% left_join(y=fare_medians,by=c("Pclass","Embarked"))
test <- test %>% mutate(Fare.predict = ifelse(is.na(Fare),Fare_median,Fare))

# Change Name to character and extraxt Title
test <- test %>% mutate(Name.char = as.character(Name), 
                          Title = factor(case_when(str_detect(Name.char,"Mr\\.")~"Mr",
                                                      str_detect(Name.char,"Mrs\\.")~"Mrs",
                                                      str_detect(Name.char,"Miss\\.")~"Miss",
                                                      str_detect(Name.char,"Master.")~"Master",
                                                      TRUE ~ "other"),
                                         levels=Title.levels))

# Estimate Age for NAs using decision tree
test <- test %>% mutate(Age.predict2=ifelse(is.na(Age),
                                              predict(forest.model.age, test, type = "raw"),
                                              Age))

# Split cabins into areas and rooms
test <- test %>% mutate(Cabin=as.character(Cabin)) %>% 
  separate(Cabin, into=c("Cabin1", "CabinRest"), sep="[\\s]+", remove=FALSE, extra="merge") %>%
  separate(Cabin1, into=c("Area1", "Room1"), sep="(?<=[A-Za-z])(?=[0-9])", remove=FALSE) %>%
  mutate(Area1=factor(Area1,levels = Area.levels),Room1=ifelse(is.na(Room1),0,as.integer(Room1)))

###### PREDICTION #####
# Use sex only
sex_predict <- train %>% select(Survived.class,Sex) %>%
  mutate(predict=ifelse(Sex=="female",1,0))

mean(sex_predict$predict==train$Survived.class)

Survived.predict <- data.frame(sex=example)
Survived.predict <- Survived.predict %>% rename(PassengerId=sex.PassengerId,
                                                Survived=sex.Survived)

# Use k nearest neighbors
knn.model <- train(Survived.class ~ Pclass+Sex+Age.predict2, method = "knn", data = train,
             tuneGrid = data.frame(k = seq(1, 20, 1))
             )
             
knn.model$results

Survived.predict <- Survived.predict %>% mutate(knn=predict(knn.model, test, type = "raw"))

# Use decision trees
tree.model <- train(Survived.class ~ Pclass+Sex+SibSp+Parch+Embarked+Age.predict2+Fare.predict+Title+Area1+Room1, 
                    method = "rpart", data = train,
                    tuneGrid = data.frame(cp = seq(0.002, 0.02, 0.002)),
                    control = rpart.control(minsplit = 15))

tree.model$results
rpart.plot(tree.model$finalModel)

Survived.predict <- Survived.predict %>% mutate(tree=predict(tree.model, test, type = "raw"))

# Use random forest
control <- trainControl(method = "cv", number = 15, p = .85) # 5 cross validation samples with .9 of data
forest.model <- train(Survived.class ~ Pclass+Sex+SibSp+Parch+Embarked+Age.predict2+Fare.predict+Title+Area1+Room1, 
                      method="rf", data = train, 
                      tuneGrid = data.frame(mtry = c(2,3,4,5,6,7,8,9,10)),
                      ntree=15)

forest.model$results

Survived.predict <- Survived.predict %>% mutate(forest=predict(forest.model, test, type = "raw"))

#### SUBMISSION #####
submission <- Survived.predict %>% select(PassengerId,forest) %>% rename(Survived=forest)
write.csv(submission,"C:/Users/Stephan/Documents/R/projects/proj_Titanic/submission.csv",row.names = FALSE)

