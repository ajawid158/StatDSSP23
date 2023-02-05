####+++++++++++++++++++ITC 255 SP 23+++++#

#Descriptive methods
  #Univar case 
    #FDT 
      #Categorical variables

##Upload the data 
#dataset tips

dfTips=read.csv(url('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv'))
head(dfTips)

#FDT smoker
absFreq=table(dfTips$smoker)
absFreq
relFreq=round(prop.table(absFreq), 2)
relFreq
cumFreq=cumsum(relFreq)
cumFreq

fdtSmoker=cbind(absFreq, relFreq, cumFreq)
fdtSmoker

names(dfTips)
##Create a function that returns the FDT of a cat var
fdtCat=function(x){
  absFreq=table(x)
  relFreq=prop.table(absFreq)
  cumFreq=cumsum(relFreq)
  fdtx=cbind(absFreq, relFreq, cumFreq)
  return(fdtx)
}

#test
fdtCat(dfTips$day)
#export using write.csv(....)

#Numerical variables

#1. Transform the variable into a categorical var
#based a definition/we specify them 
names(dfTips)

#tips
summary(dfTips$tip)

#define catgories: small whtn tip<3 meduim when tip is 3>= but less than 7, large otherwise

#selection + Loop
catTips=c()

for (k in 1:length(dfTips$tip)) {
  if(dfTips$tip[k]<3){
    catTips[k]="smallTip"
  } else if (dfTips$tip[k] >=3 & dfTips$tip[k]<7) {
    catTips[k]="meduimTip"
  } else {
    catTips[k]="largetip"
  }
}


tipAmount=cbind(dfTips$tip, catTips)
View(tipAmount)




#Descriptive methods
#Univar case 
#Graphs 
#Categorical vars (pie and bar)

#create the FDT 
fdtSmoker=fdtCat(dfTips$smoker)[,1]
fdtSmoker

pie(fdtSmoker, 
    col = rainbow(2), 
    main = 'Smoker distribution')

barplot(fdtSmoker, 
        col=rainbow(2), 
        main = 'Smoker distribution')

#Descriptive methods
#Univar case 
#Graphs 
#Num vars (hist and density)

hist(dfTips$tip, 
     col='blue', 
     main = 'Tips distibution')

plot(density(dfTips$tip), 
     col='#0033FF', 
     main='Tips distribution')

#Descriptive methods
#Univar case 
#Numerical methods
#Center of distribution (mean, median)

mean(dfTips$tip)
median(dfTips$tip)

#Descriptive methods
#Univar case 
#Numerical methods
#variation in the distribution (range, sd, var, mad)
range(dfTips$tip)
sd(dfTips$tip)
var(dfTips$tip)
mad(dfTips$tip)

#Descriptive methods
#Univar case 
#Numerical methods
#location (quantiles and percentiles)
quantile(dfTips$tip)
boxplot(dfTips$tip,
        horizontal = T, 
        col='#0033FF')
boxplot.stats(dfTips$tip) #outliers
quantile(dfTips$tip, .9)
plot(density(dfTips$total_bill))

quantile(dfTips$total_bill, .4)

dim(dfTips)
#write a fun that retuns any location in the dist

myQnt=function(x,q){
  pr=quantile(x, q)
  return(pr)
}
myQnt(dfTips$total_bill, .4)

##ECDF 
plot(ecdf(dfTips$total_bill), 
     color='blue', 
     main='ECDF of Total bill', 
     xlab='total bill')


ecdf(dfTips$total_bill)(15)

quantile(dfTips$total_bill, .3)

##Data Manipulation dplyr yes