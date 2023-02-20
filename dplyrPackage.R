#+ITC 255 SP23


#data manipulation with dplyr package
library(dplyr)
dfTips=read.csv('tips.csv')
names(dfTips)



#filter rows
fNonS=filter(dfTips, sex=="Female", smoker=='No')
head(fNonS)
dim(fNonS)

#logical operators &, |, !
unique(dfTips$day)

weekend=filter(dfTips, day=='Sun'| day=='Sat')
head(weekend)
dim(weekend)

#weekend and female
wkEndF=filter(dfTips, (day=='Sun'|day=='Sat') & sex=='Female')
head(wkEndF)
dim(wkEndF)

#Weekdays 
wkDays=filter(dfTips, day!='Sun' &  day !='Sat')
head(wkDays)

#use the function %in%
unique(dfTips$size)

filter(dfTips, size %in% c(5,6))
filter(dfTips, day %in% c('Sun','Sat'))

#for numerical >, <, ==
filter(dfTips, size==4)


#Arrange
names(dfTips)
head(arrange(dfTips, desc(tip)))
head(arrange(dfTips, sex))

#select
names(dfTips)
select(dfTips, c(total_bill, tip, size))
select(dfTips, size, everything())
select(dfTips, tip:smoker)
select(dfTips, -(tip:smoker))


#rename
names(dfTips)
dfTips1=rename(dfTips, obs=X)
head(dfTips1)


#mutate
head(dfTips)
#total spending=total_bill+tip
head(mutate(dfTips, total_cost=total_bill+tip))

#summarize
summarise(dfTips, mean(total_bill), mean(tip))

gender=group_by(dfTips, sex)
summarise(gender, mean(total_bill), sd(total_bill))


#pull a column as a vector 
gender = pull(dfTips, sex)

gender


#sample_n
dim(dfTips)
sampledfTips=sample_n(dfTips, 100)
head(sampledfTips)

