#perform exploratory data analysis on a classic machine learning dataset: Titanic survival!
#The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 from the United Kingdom to New York. More than 1,500 of the estimated 2,224 passengers and crew died in the accident, making this one of the largest maritime disasters ever outside of war. The ship carried a wide range of passengers of all ages and both genders, from luxury travelers in first-class to immigrants in the lower classes. However, not all passengers were equally likely to survive the accident. We use real data about a selection of 891 passengers to learn who was on the Titanic and which passengers were more likely to survive.
#Source  https://www.kaggle.com/c/titanic/data
library(tidyverse)
library(titanic)
options(digits=3)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
head(titanic)
str(titanic)

?describe
?titanic_train

#Make density plots of age grouped by sex. Try experimenting with combinations of faceting, alpha blending, stacking and using variable counts on the y-axis
titanic%>%group_by(Sex)%>%
  ggplot(aes(Age,y=..count..,color=Sex,fill=Sex))+
  geom_density(alpha=0.2,bw=0.8)+
  facet_grid(Sex~.)

female<- titanic%>% filter(Sex=="female")
nrow(female)
male<- titanic%>% filter(Sex=="male")
nrow(male)

tab<- table(titanic$Sex)
tab
prop.table(tab)

titanic%>%group_by(Sex)%>% summarize(n=n())%>% knitr::kable()

titanic%>%filter(Age==40)%>% group_by(Sex)%>% summarize(n=n())%>% knitr::kable()
titanic%>%filter(Age<17)%>% group_by(Sex)%>% summarize(n=n())%>% knitr::kable()
titanic%>%filter(Age%in%(18:35))%>% group_by(Sex)%>% summarize(n=n())%>% knitr::kable()

#Smooth Density
titanic%>%group_by(Sex)%>%
  ggplot(aes(Age,y=..count..,color=Sex,fill=Sex))+
  geom_density(alpha=0.2)+
  facet_grid(Sex~.)

#QQ Plot
params<-titanic %>%
  filter(!is.na(Age))%>%
  summarize(mean=mean(Age),sd=sd(Age))

titanic %>% ggplot(aes(sample=Age))+
  geom_qq(dparams=params)+
  geom_abline()

# Bar Plot clearly shows that less than half of the passengers survived and most of them were female.
titanic%>% ggplot(aes(Survived,fill=Sex))+geom_bar(position=position_dodge())

tab<-titanic %>%group_by(Sex)%>%
  count(Survived==1)%>%
  mutate(proportion=n/sum(n))
tab    

tab_survived<- titanic %>%
  count(Survived)%>%
  mutate(proportion=n/sum(n))
tab_survived

tab_survived%>% ggplot(aes(Survived,proportion))+geom_bar(stat="identity")

#Density Plot of age-Survival by Age
# The Density plot clearly shows that the only age group more likely to survive than die was 0-8,
# the age group that had most deaths was 18-30 and
# the age group that had highest proportion of deaths was 70-80
titanic %>% ggplot(aes(Age,y=..count..,fill=Survived))+geom_density(alpha=0.2)
titanic %>% ggplot(aes(Age,y=..count..,fill=Survived))+geom_density(position="stack",alpha=0.2)

#Boxplot of Fare grouped by Survival Status
#This plot reveals that Passengers who survived generally payed higher fares than those who did not survive
# Also the median fare was lower for passengers who did not survive and most individuals who paid around $8 did not survive.
titanic %>% filter(Fare!=0)%>% ggplot(aes(Survived,Fare,fill=Survived))+
  geom_boxplot()+
  scale_y_continuous(trans="log2")+
  geom_jitter(alpha=0.5)

# Survival by Passenger Class
titanic%>% ggplot(aes(Pclass,fill=Survived))+
  geom_bar()
#Bar Plot showing the relative proportion in each group instead of counts
titanic%>% ggplot(aes(Pclass,fill=Survived))+
  geom_bar(position=position_fill())

titanic%>% ggplot(aes(Survived,fill=Pclass))+
  geom_bar(position=position_fill())

#Survival by Age,Sex,Passenger Class
#Grid for Density Plots for age,filled by survival status
titanic %>% ggplot(aes(Age,y=..count..,fill=Survived))+geom_density()+
  facet_grid(Sex~Pclass)

prop_survived<- titanic %>%filter(Age %in% (50:70)& Survived==0)%>% group_by(Age)%>% 
  count(Survived)%>%
  mutate(proportion=n/sum(n))
prop_survived

titanic%>% filter(Age %in% (0:8) & Survived==0)%>% summarize(n=n())%>% knitr::kable()
titanic%>% filter(Age %in% (10:18) & Survived==0)%>% summarize(n=n())%>% knitr::kable()
titanic%>% filter(Age %in% (18:30) & Survived==0)%>% summarize(n=n())%>% knitr::kable()
titanic%>% filter(Age %in% (30:40) & Survived==0)%>% summarize(n=n())%>% knitr::kable()
titanic%>% filter(Age %in% (50:70) & Survived==0)%>% summarize(n=n())%>% knitr::kable()
titanic%>% filter(Age %in% (70:80) & Survived==0)%>% summarize(n=n())%>% knitr::kable()

library(ggridges)
titanic%>%filter(Survived==1)%>% ggplot(aes(Age,Sex))+
  geom_density_ridges(jittered_points=TRUE,
                      position=position_points_jitter(height=0),
                      point_shape ='|',point= 3,
                      point_alpha= 1,alpha=0.7)
titanic%>%filter(Survived==0)%>% ggplot(aes(Age,Sex))+
  geom_density_ridges(jittered_points=TRUE,
                      position=position_points_jitter(height=0),
                      point_shape ='|',point= 3,
                      point_alpha= 1,alpha=0.7)
