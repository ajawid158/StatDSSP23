####+++++++++++++++++++ITC 255 SP 23+++++#

#Descriptive methods
  #Univar case 
    #FDT 
      #Categorical variables

##Upload the data 
#dataset tips

dfTips=read.csv(url('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv'))
View(dfTips)
head(dfTips)
dim(dfTips)
names(dfTips)

  #FDT smoker
    #Smoker variable

absFreq=table(dfTips$smoker)
absFreq

#round(prop.table(absFreq),2)

relFreq=round(prop.table(absFreq), 2)
relFreq

cumFreq=cumsum(relFreq)
cumFreq

fdtSmoker=cbind(absFreq, relFreq, cumFreq)
fdtSmoker

names(dfTips)
##Create a function that returns the FDT of a cat var

#Write a function that adds two numbers

sm=function(x,y){
  s=x+y
  p=x*y
  d=x-y
  return(c(s,p,d))
}

sm(3,4)

###
fdtQL=function(x){
  
  absFreq=table(x)
  relFreq=round(prop.table(absFreq), 3)
  cumFreq=cumsum(relFreq)
  fdtx=cbind(absFreq, relFreq, cumFreq)
  return(fdtx)
}

fdtQL(dfTips$smoker)
fdtQL(dfTips$sex)
fdtQL(dfTips$time)
#export using write.csv(....)

#Loops and conditional functions work in R

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

fdtQL(catTips)


#Descriptive methods
  #Univar case 
    #Graphs 
      #Categorical vars (pie and bar)

#create the FDT 
fdtSmoker=fdtQL(dfTips$smoker)[,2]
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





#+Descriptive methods+#
  #Univar case 
    #Numerical methods
      #variation in the distribution (range, sd, var, mad)
dfTips=read.csv(url('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv'))
head(dfTips)
#tip
plot(density(dfTips$tip))

#Center
#Midpoint/median, where most observations are, average,middle, Mode 
mean(dfTips$tip)
median(dfTips$tip)

#location (quantiles and percentiles)
quantile(dfTips$tip)
quantile(dfTips$tip, 0.4)
#write a fun that returns any location in the dist

myQnt=function(x,q){
  pr=quantile(x, q)
  return(pr)
}

#1- myQnt()

boxplot(dfTips$tip,
        horizontal = T, 
        col='#0033FF')

boxplot.stats(dfTips$tip) #outliers
quantile(dfTips$tip, .9)

View(dfTips)
#remove the outliers

tipNew=dfTips$tip[dfTips$tip<5.5]


boxplot(tipNew, horizontal = T)
mean(tipNew)

#Variation
range(dfTips$tip)
sd(dfTips$tip)
var(dfTips$tip)   #center means the mean
mad(dfTips$tip)

##ECDF 
plot(ecdf(dfTips$tip), 
     col='blue', 
     main='ECDF of Tip', 
     xlab='tip')
abline(v=3.9, col='red', lty=3)
abline(h=0.8, col='darkgreen', lty=3)


ecdf(dfTips$tip)(4)

quantile(dfTips$tip, ecdf(dfTips$tip)(4))

#Next:Data Manipulation dplyr package