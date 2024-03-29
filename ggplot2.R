#ggplot2 
#install.packages("ggplot2")
library(ggplot2)
dtTips=read.csv('tips.csv')
head(dtTips)

#univar gaphs
##++++++++++++++++++Pie chart
#Gender

fdtGender=table(dtTips$sex)
fdtGender=as.data.frame(fdtGender)
fdtGender
colnames(fdtGender)=c("Gender","Count")


g0=ggplot(fdtGender, aes(x="", y=Count, fill=Gender))
g1=g0+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "blue",
                                  size = 14, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Gender Distribution of Customers')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#99FF33', '#BE2A3E'))+
  theme(legend.position = 'bottom')
ggsave('genderDist.png')

g1


###++++++++++++++++++++++Bar Chart

tGender=table(dtTips$sex)
tGender=as.data.frame(tGender)
colnames(tGender)=c('Gender', 'Count')

g0=ggplot(tGender, aes(x=Gender, y=Count, fill=Gender))
g0+geom_bar(stat='identity')+
  theme_classic()+
  theme(legend.position = '')+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust=.5))+
  ggtitle('Customers Gender Distribution')+
  geom_text(aes(label=Count), vjust=2)+
  scale_fill_manual(values=c('#FF9933', '#0000CC'))
ggsave('genderBar.pdf')


###++++++++++++++++++++++++++Histogram

g0=ggplot(dtTips, aes(x=tip))
g0+geom_histogram(bins = 10, fill='#99FFFF', colour=4)+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title.x = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip Distribution')+
  xlab('Tip Amount')+
  ylab('Frequency')+
  geom_vline(xintercept = 3,
             linetype='dashed',
             color='red', 
             size=1)
ggsave('tipDistHist.png')


###++++++++++++++++++++Density plot

g0=ggplot(dtTips, aes(x=tip))
g0+geom_density(color='red', size=.1)+
  theme_classic()+
  xlim(0,12)+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip Distribution')+
  xlab('Tip Amount')+
  ylab('Density')+
  geom_vline(xintercept = 3,
             linetype='dashed',
             color='blue', 
             size=1)
ggsave('tipDistHist.png')


x=as.data.frame(rnorm(1000000, 170,10))
colnames(x)=c('xN')
head(x)
y=as.data.frame(rnorm(1000000, 10.2,1))
colnames(y)=c('y')


plot(density(x$xN), 
     col='red',
       ylim=c(0,.04))
lines(density(y$y),
      col='blue')


g0=ggplot(x, aes(x=xN))
g0+geom_density(color='red', size=.1)+
  theme_test()+
  geom_vline(xintercept = 0,
             linetype='dashed',
             color='blue', 
             size=1)

###+++++++++++++++++++Box Plot

g0=ggplot(dtTip, aes(y='',x=tip))
g0+geom_boxplot(fill=4, 
                color=2, 
                alpha=0.6, 
                outlier.colour = 'blue', 
                linetype=2, 
                lwd=.6)+
  theme_classic()+
  theme(axis.title.x = element_text(), 
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color='darkgreen'))+
  ggtitle('Box Plot of the Tip')+
  xlab('Tip Amount')
ggsave('boxplotTip.png')


#Bivariate graphs
###+++++++++++++++++++Joint graphs
#Gender[F, M] and Smoker[Y, N] 

jtable=table(dtTip$sex, dtTip$smoker)
jtable=as.data.frame(jtable)
jtable

ggplot(jtable, aes(x=Var1, y=Freq, fill=Var2))+
  geom_col(position = position_dodge())+
  theme_classic()+
  theme(axis.title.x = element_text(),
        legend.title = element_text(color = 'blue'),
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color='darkgreen'))+
  ggtitle('Join bar graph of Gender and Smoking')+
  xlab('Gender')+
  theme(legend.position='bottom')+
  guides(fill=guide_legend('Smoking'))
ggsave('jointBarGenderSmoke.pdf')


####+++++++++++++++++++Joint density 
##Gender[F, M]  tip

g0=ggplot(dtTip, aes(x=tip, color=sex))
g0+geom_density()+
  theme_replace()+
  scale_color_manual(values = c('red', 'blue'))+
  xlim(-1,11)+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'))+
  ggtitle('Joint distribution of Tip amount across Gender')+
  xlab('Tib Amount')+
  theme(legend.position = 'bottom')
ggsave('jointDensity.png')

#####+++++++++++++++++++Ridgeline plot
#smoker[y, n] tip
#install.packages('ggridges')
library(ggridges)

ggplot(dtTip, aes(x=tip, y=smoker, fill=smoker))+
  geom_density_ridges(color=4, 
                      lwd=.3)+
  theme_gray()+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'))+
  ggtitle('Joint dist of Tip and Smoker')+
  xlab('Tip Amount')
ggsave('jointDistTipsSmoker.png')


######+++++++++++++++++++Joint box plot
#Gender[y,N] tip


ggplot(dtTip, aes(x=tip, y=sex, fill=sex))+
  geom_boxplot(color=2, 
               alpha=0.9, 
               outlier.colour = 'blue', 
               linetype=2, 
               lwd=.6)+
  stat_boxplot(geom = 'errorbar', 
               width=.5)+
  theme_gray()+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'))+
  ggtitle('Joint dist of Tip and Gender')+
  xlab('Tip Amount')+
  ylab('Gender')+
  xlim(-1,11)
ggsave('jointboxplot.png')

######+++++++++++++++++++boxplot with points

boxplot(dtTip$tip, col = 'white', horizontal = T)
stripchart(dtTip$tip, 
           method = 'jitter', 
           pch=1, 
           col=4, 
           add = TRUE)

######+++++++++++++++++++Joint boxplot
##Gender[y,n] tip

boxplot(tip~sex,
        data = dtTip,
        col='white',
        horizontal = T)
stripchart(tip~sex,
           data = dtTip,
           method = 'jitter', 
           pch=19, 
           col=2:4,
           add = TRUE)
ggsave('jointboxplotwithpoints.pdf')
####+++++++++++++++++++Beeswarm graph

##install.packages('ggbeeswarm')

library(ggbeeswarm)
#smoker[y, n] tip

ggplot(dtTip, aes(x=smoker, y=tip, color=smoker))+
  geom_beeswarm(cex=1)


#2 QNT vars: Scatter plot
g0=ggplot(dtTip, aes(x=total_bill, y=tip))
g0+geom_point()

#modifications inside geom_point
g0=ggplot(dtTip, aes(x=total_bill, y=tip))
g0+geom_point(color=10, shape=1, size=3)

names(dtTip)
#Modifications in aes
g0=ggplot(dtTip, aes(x=total_bill, y=tip, color=sex))
g0+geom_point()

#Modifications in both 
g0=ggplot(dtTip, aes(x=total_bill, y=tip, alpha=size))
g0+geom_point(color=3, size=3)

#Adding a vertical line and size
g0=ggplot(dtTip, aes(x=total_bill, y=tip,color=time, size=size))
g0+geom_point()+
  geom_vline(xintercept = 40, linetype='dashed')+
  geom_hline(yintercept = 5, linetype='dashed')

#Adding Facet:
g0+geom_point()+
  facet_wrap(~day)

g0=ggplot(dtTip, aes(x=total_bill, y=tip, color=time, size=size))
g0+geom_point()+
  facet_wrap(~smoker)

##Adding Regression line
g0=ggplot(dtTip, aes(x=total_bill, y=tip))
g0+geom_smooth()

g0=ggplot(dtTip, aes(x=total_bill, y=tip, group=sex))
g0+geom_smooth()+
  geom_point()


#Adding more detials
g0=ggplot(dtTip, aes(x=total_bill, y=tip, color=sex))
g1=g0+geom_smooth(se=FALSE)+
  geom_point(mapping = aes(color=smoker))+
  theme_bw()+
  theme(axis.title.x = element_text(), 
        axis.title.y = element_text(), 
        plot.title = element_text(hjust = .5), 
        legend.title = element_blank())+
  ggtitle('Scatter plot total_bill/tip')+
  xlab('Total Bill in USD')+
  ylab('Tip Amount')+
  theme(legend.position = 'bottom')
g1

##Export as html and ggplotly
#install.packages('plotly')
library(plotly)

g2=plotly::ggplotly(g1)
htmlwidgets::saveWidget(g2, 
                        file = 'scatter.html')
