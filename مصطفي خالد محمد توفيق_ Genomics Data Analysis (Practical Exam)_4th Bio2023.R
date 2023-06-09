# Mustapha Khaled Mohamed Tawfiq 

setwd("C:/Users/El-Hajjaj/Documents")
# Read CSV file
d1<-read.csv('G2_anthropometry.csv')
dim(d1)
str(d1)
d1
# creating data frame from en existing Data set
VIP<-d1[c(1:5),c(2,4)]
# Re naming Rows
row.names(VIP)<-c('row1','row2','row3','row4','row5')
VIP
# Renaming cols
colnames(d1)<-c('citizen_age','citizen_gender','citizen_footlength','citizen_height')
View(d1)
# Getting the first 6 rows 
head(d1,6)
# Getting the last 6 rows 
tail(d1,5)
# Showing specific cols 
d1[,c(2:4)]
# Removing specific rows
d1[c(-1,-5),]
# Removing text in numeric values
d1$citizen_height<-gsub('cm','',d1$citizen_footlength)
# Converting from specific data type to another (integer)
d1$citizen_height<-as.integer(d1$citizen_height)
# Filter the data set according to specific criteria
d1[d1$citizen_age>10&d1$citizen_gender=='F',]
d1[order(d1$citizen_height,decreasing =T),]
# Data variable re-coding 
d1$citizen_gender[d1$citizen_gender=='F']<-'female'
d1$citizen_gender[d1$citizen_gender=='cm']<-'male'
d1
# Adding Col according to an existing col
d1$tall_OR_Short[d1$citizen_height>180]<-'tall'
d1$tall_OR_Short[d1$citizen_height<=180]<-'short'
d1
# Check data set according to specific criteria
which(d1$tall_OR_Short=='short')
which(d1$tall_OR_Short=='tall'&d1$citizen_gender=='male')
# Adding Row
d2<-data.frame(citizen_age=20.000000,citizen_gender='male',citizen_footlength=250,citizen_height=155,tall_OR_Short='short')
d3<-rbind(d1,d2)
d3
# Calculate mean & median for specific Cols
mean(d1$citizen_age)
median(d1$citizen_footlength,na.rm = T)
# Dealing with NA Values
m_mean<-mean(d1[d1$citizen_gender=="male" , 'citizen_footlength' ], na.rm=T)
d1[is.na(d1$citizen_footlength) & d1$citizen_gender=="male"  , 'citizen_footlength']=m_mean
f_mean<-mean(d1[d1$citizen_gender=="female" , 'citizen_footlength' ], na.rm=T)
d1[is.na(d1$citizen_footlength) & d1$citizen_gender=="female"  , 'citizen_footlength']=f_mean
sd(d1$citizen_height,na.rm = T)
any(duplicated(d1))
d1[!complete.cases(d1), ]
is.na(d1)
na.omit(d1)
# Data visualization using Histogram 
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
draw_hist <- ggplot(d1, aes(citizen_age))
draw_hist
draw_hist + geom_histogram()
draw_hist + geom_histogram(binwidth = 5)
draw_hist + geom_histogram(color="red")
draw_hist + geom_histogram(fill="blue")
draw_hist + geom_histogram(alpha = 0.5)
draw_hist + geom_histogram() + ggtitle("Age of citizens")
draw_hist + geom_histogram() + labs(x="citizen_age", y="Numbers")
#display the effect of height on foot_length colored by the groups of age range using scatter plot
fig2<-ggplot(d1 , aes(citizen_footlength , citizen_height))
fig2 + geom_point(aes(color=citizen_age)) +stat_smooth(se=FALSE) 





