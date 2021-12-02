library(ggplot2)

#loading the data
train_data<-read.csv("train.csv", stringsAsFactors = FALSE)

head(train_data)

#Transform the variables as factors
train_data$Survived<- as.factor(train_data$Survived)
train_data$Pclass<- as.factor(train_data$Pclass)
train_data$Sex<- as.factor(train_data$Sex)
train_data$Embarked<- as.factor(train_data$Embarked)

#Exploratory Data Analysis

#Survival rate
prop.table(table(train_data$Survived))

# Bar Chart
BarChart_surv<- ggplot(train_data,aes(x=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y="Passenger Count",
       title="Titanic Survival Rates")
BarChart_surv


#Survival rate by gender
tab<-train_data %>%group_by(Sex)%>%
  count(Survived==1)%>%
  mutate(proportion=n/sum(n))
tab
#Bar Chart
Bar_chart_sex<- ggplot(train_data,aes(x=Sex,fill=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y="Passenger Count",
       title="Titanic Survival Rates by Sex")
Bar_chart_sex

#Survival rate by class of ticket
Bar_chart_group1<- ggplot(train_data,aes(x=Pclass,fill=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y="Passenger Count",
       title="Titanic Survival Rates by Pclass")
Bar_chart_group1

#Survival rate by class of ticket and gender
Bar_chart_group2<- ggplot(train_data,aes(x=Sex,fill=Survived))+
  theme_bw()+
  facet_wrap(~Pclass)+
  geom_bar()+
  labs(y="Passenger Count",
       title="Titanic Survival Rates by Pclass and Sex")
Bar_chart_group2

#Distribution of passenger ages
Dist_age<- ggplot(train_data,aes(x=Age, fill= Survived))+
  theme_bw()+
  geom_histogram(binwidth=5)+
  labs(y="Passenger Count",
       x="Age (binwidth =5)",
title=" Titanic Age Distribution")
Dist_age

# Survival rates by Age
# Box and whisker plot
ggplot(train_data, aes(x=Survived, y=Age))+
  theme_bw()+
  geom_boxplot()+
  labs(y="Age",
       x="Survived",
       title="Titanic Survival Rates by Age")

#survival rates by age when segmented by gender and class of ticket
Density_plot<- ggplot(train_data, aes(x=Age, fill=Survived))+
  theme_bw()+
  facet_wrap(Sex~Pclass)+
  geom_density(alpha=0.5)+
  labs(y="Age",
       x="Survived",
       title="Titanic Survival Rates by Age, Pclass and Sex")
Density_plot

# Histogram of survival rates by age when segmented by gender and class of ticket
Hist_plot<- ggplot(train_data, aes(x=Age, fill=Survived))+
  theme_bw()+
  facet_wrap(Sex~Pclass)+
  geom_histogram(alpha=0.5)+
  labs(y="Age",
       x="Survived",
       title="Titanic Survival Rates by Age, Pclass and Sex")
Hist_plot

