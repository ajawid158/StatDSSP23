#ITC 255 SP 23

#dataset

dtTips=read.csv('tips.csv')
head(dtTips)

#Joint Table
#Gender and smoker associated?

jtSomkGender=table(dtTips$sex, dtTips$smoker)
jtSomkGender

addmargins(jtSomkGender)


#who give more tip, male or female
library(dplyr)
names(dtTips)

genGroup=group_by(dtTips, sex)
summarise(genGroup, mean(tip), sd(tip), min(tip), max(tip))

#which day the customers pay more tips
dayGroup=group_by(dtTips, day)
summarise(dayGroup, mean(tip), sd(tip), min(tip), max(tip))

#how tip changes as total_bill changes 
scatter.smooth(dtTips$total_bill, dtTips$tip)
abline(v=25, col='red')

cor(dtTips$total_bill, dtTips$tip)