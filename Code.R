rm(list = ls())
cat('\014')
setwd('D:\\Kaggle Titanic Dataset')

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
test.survived <- test.survived[, c(2, 1, 3,4,5,6,7,8,9,10,11,12)]
data.combined <- rbind(train, test.survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)
table(data.combined$Survived)
table(data.combined$Pclass)
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + geom_bar(width = 0.5) + labs(fill = "Survived")

head(as.character(train$Name))
length(unique(as.character(data.combined$Name)))
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names),]
library(stringr)
library(ggplot2)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrs[1:5,]

males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

extractTitle <- function(name){
  if(length(grep("Miss", name))>0){
    return("Miss.")}
  else if(length(grep("Mrs.", name))>0){
    return("Mrs.")}
  else if(length(grep("Mr.", name))>0){
    return("Mr.")}
  else if(length(grep("Master", name))>0){
    return("Master")}
  else {
    return("Other")}
}

titles <- NULL
for (i in 1:nrow(data.combined)){
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$title <- as.factor(titles)

aa <- data.combined[which(data.combined$title == 'Other'),]

ggplot(data.combined[1:891,], aes(x=title, fill = Survived)) + geom_bar() + facet_wrap(~Pclass) + ggtitle("Pclass")

table(data.combined$Sex)
ggplot(data.combined[1:891,], aes(x=Sex, fill = Survived)) + geom_bar() + facet_wrap(~Pclass) + ggtitle("Pclass")

summary(data.combined[1:891,"Age"])
ggplot(data.combined[1:891,], aes(x=Age, fill = Survived)) + geom_bar(binwidth = 10) + facet_wrap(~Sex+Pclass) + ggtitle("Pclass")
boys <- data.combined[which(data.combined$title=="Master"),]
summary(boys$Age)

misses <- data.combined[which(data.combined$title=="Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x=Age, fill=Survived))+facet_wrap(~Pclass)+geom_bar(binwidth = 10)

misses.alone <- misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<=14.5))

summary(data.combined$SibSp)
length(unique(data.combined$SibSp))
ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived))+facet_wrap(~Pclass + title)+geom_bar(binwidth = 1)+ylim(0,300)+labs(fill="Survived")
ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived))+facet_wrap(~Pclass + title)+geom_bar(binwidth = 1)+ylim(0,300)+labs(fill="Survived")

temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp+temp.parch+1)
ggplot(data.combined[1:891,], aes(x=family.size, fill=Survived))+facet_wrap(~Pclass + title)+geom_bar()+ylim(0,300)+labs(fill="Survived")

str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived))+ geom_bar() + ggtitle("Survivability by Ticket.first.char") + ylim(0,350) + labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived))+ geom_bar() + facet_wrap(~Pclass) + ylim(0,150) + labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived))+ geom_bar() + facet_wrap(~Pclass + title) + ylim(0,200) + labs(fill = "Survived")

summary(data.combined$Fare)       
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x=Fare))+ geom_histogram(binwidth = 5)

ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived))+ geom_histogram(binwidth = 5) + facet_wrap(~Pclass + title) + ylim(0,50) + labs(fill = "Survived")

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(is.na(data.combined$Cabin) == T), "Cabin"] <- "U"
data.combined$Cabin[1:100]
Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
levels(Cabin.first.char)

data.combined$Cabin.first.char <- Cabin.first.char

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+ geom_bar() + ylim(0,750) + labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+ geom_bar() + facet_wrap(~Pclass) + labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+ geom_bar() + facet_wrap(~Pclass + title) + labs(fill = "Survived")

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))
ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived))+ geom_bar() + facet_wrap(~Pclass + title) + labs(fill = "Survived")

str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived))+ geom_bar() + facet_wrap(~Pclass + title) + labs(fill = "Survived")
View(data.combined)

# Video 4
#install.packages("randomForest")
library(randomForest)

rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
#str(train$Survived)
rf.label <- as.factor(train$Survived)

set.seed(123)
rf.1 <- randomForest(x = rf.train.1, y=rf.label, importance = T, ntree = 1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]
set.seed(1234)
rf.2 <- randomForest(x=rf.train.2, y=rf.label, importance = T, ntree = 1000)
rf.2
varImpPlot(rf.2)

rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3, y=rf.label, importance = T, ntree = 1000)
rf.3
varImpPlot(rf.3)


rf.train.4 <- data.combined[1:891, c("Pclass", "title", "Parch", "SibSp")]
set.seed(1234)
rf.4 <- randomForest(x=rf.train.4, y=rf.label, importance = T, ntree = 1000)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size" )]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, importance = T, ntree = 1000)
rf.5
varImpPlot(rf.5)