#Load Raw Data
Train <- read.csv("Train.csv", header = TRUE)
test <- read.csv("Titanic_Test_Data.csv", header = TRUE)

#Add a "Survived" variable to the test data set for combining data sets
test.survived <- data.frame(Survived= rep("None", nrow(test)), test[,])

#Moving Survived over for rbind to run properly
test.survived <- test.survived[c(2,1,3:12)]

#Combine Data sets 
data.combined <- rbind(train, test.survived)

#A bit about R data types (e.g. factors)
str(data.combined)
#Changed PClass and Survived to factors 
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

#Take a look at gross survival rates
table(data.combined$Survived)

#Take a distribution across classes
table(data.combined$Pclass)

#load up ggplot2 package to use for visualization (Packages --> Install --> ggplot2)
library(ggplot2)

#Hypothesis - Rich folks survived at a higher rate
Train$Pclass <- as.factor(Train$Pclass)
ggplot(Train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = .5) + 
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Examine the first few names in the training data set
head(as.character(Train$Name))

#How many unique names are there in the train & test datasets 
length(unique(as.character(data.combined$Name)))

#Two duplicates names - closer look
#First get duplicate names and store them as a vector 
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

#Whats up with the "Miss" and "Mr" thing?
library(stringr)

#Any correlation with other variables (e.g. sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#Check out males to see if that pattern continues
males <- data.combined[which(Train$Sex == "male"),]
males[1:5,]

#Expand upon relationship between "Survived" and "PClass" by adding new 'Title' variable to the 
#data set and then explore a potential 3-dimensional relationship

#Create a utility function to help with title extraction
extractTitle <- function(Name){
  name <- as.character(Name)
  
  if(length(grep("Miss.", Name)) > 0){
    return ("Miss.")
  }else if (length(grep("Master.", Name)) > 0){
    return ("Master.")
  }else if (length(grep("Mrs.", Name)) > 0){
    return ("Mrs.")
  }else if (length(grep("Mr.", Name)) > 0){
      return ("Mr.")
  }else{
    return ("Other")
  }
}

#Create vector that is populated with utility function
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

#Add to data.combined dataframe
data.combined$title <- as.factor(titles)

#Let's plot against our new factor (only using train rows)
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) + 
  geom_bar(width = .5)+
  facet_wrap(~Pclass) +
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count") +
  labs("Survived")

#Visualize a 3way relationship of sex, pclass, and survival
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = .5) + 
  facet_wrap(~Pclass) + 
  xlab("Sex")+
  ylab("Total Count")+
  labs("Survived")

#Let's now look at age
summary(data.combined$Age)

#Let's look at survival rates by age, sex, and pclass
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) + 
  geom_histogram(biwidth = 10) +
  xlab("Age")+
  ylab("Total Count")+
  labs("Survived")

#Confirm Master Title is a good proxy for young boys
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

#Confirm the same for Misses
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

#How many misses are there with no siblings or parent aboard?
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0), ]
summary(misses.alone$Age)
length(which(misses.alone$age <= 14.5))

#No on to the SibSp variable 
summary(data.combined$SibSp)
#heavily skewed towards 0

#Can we treat SibSp as a factor?
length(unique(data.combined$SibSp))

#Only 7 variables - reasonable for a factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

#let's visualize it - is this predictive?
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp")+
  ylab("Total Count")+
  labs(fill = "Survived")
  
#Now Perch
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Parch")+
  ylab("Total Count")+
  labs(fill = "Survived")

#Lets make a family size variable
temp.SibSp <- c(Train$SibSp, test$SibSp)
temp.Parch <- c(Train$Parch, test$Parch)
data.combined$FamilySize <- as.factor(temp.SibSp + temp.Parch + 1)

#visualization 
ggplot(data.combined[1:891,], aes(x = FamilySize, fill = Survived))+
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Family Size")+
  ylab("Total Count")+
  labs(fill = "Survived")

#ticket variable (str --> Structure)
str(data.combined$Ticket)
#929 levels --> should probably be a character
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#Ticket doesn't appear to have a consistent structure
#let's look at the first character of each ticket
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1,1))
unique(ticket.first.char)

#Ok, we can make a factor for analysis purposes 
data.combined$Ticket.first.char <- as.factor(ticket.first.char)

#High-level plot
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived))+
  geom_bar() +
  #facet_wrap(~Pclass) +
  ggtitle("Survivability based on firstchar") +
  xlab("Ticket.First.Char") +
  ylab("TotalCount") +
  ylim(0,350) +
  labs(fill = "Survived")


#Drill down by Pclass
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived))+
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Ticket.First.Char") +
  ylab("TotalCount") +
  ylim(0,350) +
  labs(fill = "Survived")

#Another Drill down by Pclass and Title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived))+
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Ticket.First.Char") +
  ylab("TotalCount") +
  ylim(0,200) +
  labs(fill = "Survived")
#It doesn't look like this is a good signal --> Will probably exclude "Awktors Razor"?

#Now lets look at Fare
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#Visualize 
ggplot(data.combined, aes(x = Fare))+
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("TotalCount") +
  ylim(0,200)


#let's see if Fare has any predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived))+
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("TotalCount") +
  ylim(0,50) +
  labs(fill = "Survived")
#Doesn't appear to have predictive power --> Better to use Pclass 

#Analysis of the cabin Variable 
str(data.combined$Cabin)

#Cabin better as a Factor
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace Empty cabins with "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#Creating new variable cabin.first.char
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1,1))
str(cabin.first.char)
levels(cabin.first.char)

#add to data frame
data.combined$Cabin.first.char <- cabin.first.char

#High-level plot
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+
  geom_bar() +
  ggtitle("Survivability by Cabin Letter") +
  xlab("Cabin.First.Char") +
  ylab("TotalCount") +
  ylim(0,750)

#Drill down by PClass
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by Cabin Letter") +
  xlab("Cabin.First.Char") +
  ylab("TotalCount") +
  labs(fill = "Survived")
  ylim(0,500)
  
#Drill down by PClass and Title
  ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived))+
    geom_bar() +
    facet_wrap(~Pclass + title) +
    ggtitle("Pclass, Title") +
    xlab("Cabin.First.Char") +
    ylab("TotalCount") +
    labs(fill = "Survived")
  ylim(0,500)
#Doesn't look like a signal - Title and Pclass are better predictors 
  
#People with multiple cabins 
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

#Drill down by PClass and Title
ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived))+
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.Multiple") +
  ylab("TotalCount") +
  ylim(0,350) +
  labs(fill = "Survived")
#Doesn't look like a signal 

#Does suvivability depend on where you got on board?
summary(data.combined$Embarked)
str(data.combined$Embarked)
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived))+
  geom_bar() +
  ggtitle("Survivability by Embarked") +
  xlab("Embarked") +
  ylab("TotalCount") +
  ylim(0,750)

#Drill down
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived))+
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by Embarked") +
  xlab("Embarked") +
  ylab("TotalCount") +
  ylim(0,400)
#Doesn't appear to be a signal --> Correlation between embarked and Pclass

-------------------------------------------------------------------------
#Exploratory Modeling
library(randomForest)

#Train a Random Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <-randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)

table(factor(rf.train.1$title))
table(factor(Train$Survived))
