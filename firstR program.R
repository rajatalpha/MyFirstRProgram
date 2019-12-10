https://www.hackerrank.com/challenges/weather-observation-station-6/problem?h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen&h_r=next-challenge&h_v=zen
https://hackathons.tcscodevita.com/SUPERCoders/main_page.jsp
setwd("C:/Users/Acer/Desktop/Internship IIM Lucknow/Datasets/Offlinel4")
store.df <- read.csv(paste("StoreData.csv",sep=""))

library("xlsx")
dd <- read_excel("Tata_walkin_audit_report.xlsx", sheet =4 )
View(dd)

summary(store.df)
describe(store.df)
describe(store.df $p2sales)
some(store.df)
dim(store.df)
table(store.df$p1prom)
table(store.df$p1price,store.df$p1prom)
min(store.df$p1sales)
max(store.df$p1sales)


aggregate(store.df$p2sales,by=list(country=store.df$country),sum)
aggregate(store.df$p1sales,by=list(Store_Id=store.df$storeNum),sum)
aggregate(store.df$storeNum,by=list(country=store.df$country),sum)
by(store.df$p1sales,store.df$storeNum,mean)
apply(store.df[,2:9],MARGIN = 2,FUN = mean)
apply(store.df[,2:9],2,mean)
apply(store.df[,2:9],2, function(x) {max(x)-min(x)})

                    Module-2
cust.df<-read.csv(paste("CRMData.csv",sep=""))
hist(store.df$p1sales,
     main="Product 1 weekly sales across all stores",
     xlab="Product 1 sales(Units)",
     ylab="Counts",breaks=10 ,col="lightblue")

hist(store.df$p1sales,
     main="Product 1 weekly sales across all stores",
     xlab="Product 1 sales(Units)",
     ylab="Counts",breaks=10 ,col="lightblue", xlim=c(50,200),ylim=c(0,500))


hist(store.df$p1sales,
     main="Product 1 weekly sales across all stores",
     xlab="Product 1 sales(Units)",
     ylab="Counts",breaks=30 ,col="lightblue",freq= FALSE)

lines(density(store.df$p1sales, bw=10),
       col="darkred",lwd=2)

boxplot(store.df$p2sales,
        xlab="Weekly sales",ylab="P2",
        main="Weekly sales of product 2 across All stores",horizontal=TRUE)

# las gives size of dots,side=2 means y axis,1 means x axis
boxplot(store.df$p2sales ~ store.df$storeNum,las=1,
        xlab="Weekly sales for particular Store",ylab="P2",
        col=c("red","blue","green","yellow","orange"),
        main="Weekly sales of product 2 across individual store",horizontal=TRUE)

boxplot( p2sales ~ p2prom, data= store.df,horizontal=TRUE,yaxt="n",
        xlab="Weekly sales",ylab="Whether the product was under promotion",
        main="Does promotion has any effect???")
        axis(side=2,at=c(1,2),labels=c("NO","Yes"))
        
str(cust.df)        
summary(cust.df)
describe (cust.df)

plot(x=cust.df$age,y=cust.df$credit.score)


#Converting customerid to factors

str(cust.df$cust.id)
cust.df$cust.id <-factor(cust.df$cust.id)
str(cust.df$cust.id)
str(cust.df)


        
plot(cust.df$age,  cust.df$credit.score, xlim=c(15,55),
     ylim=c(500,900),main="Active Members all About",xlab="Customer's Age(years)",
     ylab="Credit_Score",col="red")

abline(v=mean(cust.df$age),col="dark blue",lty="dotted")
abline(h=mean(cust.df$credit.score),col="dark blue",lty="dotted")
abline(lm(cust.df$credit.score ~ cust.df$age))

plot(cust.df$store.spend,cust.df$online.spend,cex=0.6,
     xlab="Store spend for last 12 months",ylab="Online spend for last 12 months",
     main="Plot for spends")

# for studying the skew use histogram

hist(cust.df$store.spend, 
     xlim=c(0,400), ylim=c(0,450),
     breaks =(0:ceiling(max(cust.df$store.spend)/10))*10,
     col=c("red","blue","green","yellow"), xlab= "Prior 12 months stores spend",
     main="Study Skewness")
     

boxplot(cust.df$online.spend,horizontal=TRUE)

boxplot(cust.df$online.spend ~ cust.df$store.spend, yaxt="n",
        horizontal=TRUE)

plot(cust.df$online.spend,cust.df$store.spend,
     main="Customers as of June 2014",
     xlab="Stores Sales ",
     ylab="Online Sales",
     cex=0.6)
##for removing skewness
plot(cust.df$online.spend,cust.df$store.spend,
     main="Customers as of June 2014",
     xlab=" Online Sales ",
     ylab="Stores Sales",
     cex=0.6,log="xy")
##No outliers in the above plot as Log of -ve and 0 is not defined...
plot(cust.df$online.spend+1,cust.df$store.spend+1,
     main="Customers as of June 2014",
     xlab=" Online Sales ",
     ylab="Stores Sales",
     cex=0.6,log="xy")
##drawing multiple graphs 
par(mfrow=c(2,2))
with(cust.df,plot(distance.to.store,store.spend,cex=0.5))
with(cust.df,plot(distance.to.store,online.spend,cex=0.5))
with(cust.df,plot(distance.to.store,store.spend+1,cex=0.5,log="xy"))
with(cust.df,plot(distance.to.store,online.spend+1,cex=0.5,log="xy"))

##now resetting is reqired for spacing of graphs
par(mfrow=c(1,1))
#colored plots to check who has email id for online promotions
my.col<-c("black","red")
my.pch<-c(1,19)
plot(cust.df$store.spend,cust.df$online.spend,cex=0.8,
     main="Customers As of June 2014",
     col=my.col[cust.df$email],pch=my.pch[cust.df$email],
     xlab="Store Spend($)",
     ylab="Online Spend($)")
    legend (x="topright", legend=paste("email on file:",levels=(cust.df$email)),
       col=my.col,pch=my.pch)
    plot(cust.df$store.spend,cust.df$online.spend,cex=0.8, log="xy",
         main="Customers As of June 2014",
         col=my.col[cust.df$email],pch=my.pch[cust.df$email],
         xlab="Store Spend($)",
         ylab="Online Spend($)")
    legend (x="topright", legend=paste("email on file:",levels=(cust.df$email)),
            col=my.col,pch=my.pch)
    
    
##Scatter plots using Pairs & scatterplot matrix command
    pairs(formula= ~ age + credit.score + online.spend,cex=0.6,
            data=cust.df)
    
##dont forget to include the car Module
    scatterplotMatrix(formula= ~ age + credit.score + online.spend,cex=0.6,
                      data=cust.df,diagonal="histogram")
    
##to get the correlation value  
    cor(cust.df$age,cust.df$credit.score)
    
##To check if this correlation is statistically significant or not
    cor.test(cust.df$age,cust.df$credit.score)
## for getting correlations between every variables    
    cor(cust.df[,c(2,3,5:10)])
## visualizing correlations
    corrplot(corr=cor(cust.df[,c(2,3,5:10)]),use="complete.obs",
             method="ellipse")
    
##visualization using corrplot.mixed (install gplots package)
    par(mfrow=c(1,1))
    corrplot.mixed(corr=cor(cust.df[ ,c(2,3,5:12)]),use="complete.obs",
                           upper="ellipse", 
                           tl.pos="lt",
                           col=pcolorpanel(50,"red","gray60","blue4"))
    
##jitter Command
    plot(cust.df$sat.service,cust.df$sat.selection,
         xlab="sat Service",ylab="Product placement")
    
    plot(jitter(cust.df$sat.service),jitter(cust.df$sat.selection),
         xlab="sat Service",ylab="Product placement")
    
                        
      ##Module 3: Visualizizng data ussing Lattice Data Package
seg.df <- read.csv(paste("CableTVSubscribersData.csv", seg=""))
describe(seg.df)
str(seg.df)
plot(table(seg.df$kids))
  

##visualizing groups using Lattice package with one variable(Discrete)
histogram(~subscribe | Segment, data = seg.df)

histogram(~subscribe | Segment, data = seg.df,
          layout=c(4,1),type="count",
          col= c("burlywood","darkolivegreen"))
##visualizing groups using Lattice package with two variable(Discrete)
histogram(~subscribe | Segment + ownHome, data = seg.df,
          layout=c(4,2),type="count",
          col= c("burlywood" ,"red"))

##visualizing groups using Lattice package with one variable(Continous)BARCHART
seg.mean <- aggregate(income ~ Segment,data = seg.df,mean)

barchart( income ~ Segment,data = seg.mean, col="red")

##visualizing groups using Lattice package with two variable(Continous)BARCHART
seg.mean1 <- aggregate(income ~ Segment + ownHome,data = seg.df,mean)

barchart( income ~ Segment, data = seg.mean1,groups=ownHome, 
          auto.key=TRUE,par.settings=simpleTheme(col=c("gray95","gray50")))


##Visualizing for single factor using BWPLOT()
bwplot(Segment ~ income, data= seg.df,horizontal=TRUE,
       xlab="INCOME")

##Visualizing for 2 factor using BWPLOT()
bwplot(Segment ~ income | ownHome, data= seg.df,horizontal=TRUE,
       xlab="INCOME")   



                 ###Module 4:Frequency and Contingency Tables
##working with arthritis dataset
##one way Contingency tables
mytable <-with(Arthritis,table(Improved))
prop.table(mytable)        ##ratio
prop.table(mytable)*100    ##percentages

##Two way Contingency tables
mytable1=xtabs(~Treatment + Improved,data =Arthritis)
prop.table(mytable1)
## for agggregation across the rows 
margin.table(mytable1,1)
prop.table(mytable1,1)
## for agggregation across the columns
margin.table(mytable1,2)
prop.table(mytable1,2)

##adding sum variable directly
x <- addmargins(mytable1)
## y=addmargins(xtabs(~Treatment + Improved,Arthritis))  all in one
prop.table(x)
## this command will give the proportion considering sum as one the rowss andd columns
##instead do this
addmargins(prop.table(x))
  

## use gmodels for crosstables two way contigency tables

CrossTable(Arthritis$Treatment,Arthritis$Improved)

## 3 way contingency tables
## Using xtabs which shows multiple 2 way tables

mytable <- xtabs(~Treatment + Sex + Improved,data=Arthritis) 


# ftable commmand is used to print 3 way tables in a compact and attractive form
ftable(mytable)

#converting to 1 way table
margin.table(mytable,1)
margin.table(mytable,2)
margin.table(mytable,3)

# Converting a 3 way tableto a 2 way table
margin.table(mytable,c(1,3))
margin.table(mytable,c(2,3))
margin.table(mytable,c(1,2))

# Jointly using prop table and ftable
ftable(prop.table(mytable,c(1,2))*100)
# these calaculations are done rowwise as we are passing the second argument as
#2,as seen it has all the 3 variables


#Jointly using prop table cell wise
ftable(prop.table(mytable)*100)

# chi-sqaure test of Independence
# Here we check if that 21/41 pateients who were 'treated' and showed marked improvements
# is greater than the patients who were 'PLACEBO' and showed marked improvements ie 7/28
chisq.test(mytable)
p=0.0014163
# reject null hypothesis that treatment type and outcome are independent

# check if marked males are greater than marked females
chisq.test(mytable2)
p-value = 0.08889

#fisher test and Measures of association
fisher.test(mytable)
#Strength of Association using Cramer's V and Phi coefficient
assocstats(mytable)

#Selecting a subset of Data
mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & seg.df$subscribe == "subNo"])

#count of factor level occurences using table
table(seg.df$Segment,seg.df$subscribe)

# Getting proportions fro this table
prop.table(table(seg.df$Segment,seg.df$subscribe))*100
 
# Adding an extra column
# step 1: Find the average mean of each Segment
seg.income.mean <- aggregate(seg.df$income,list(seg.df$Segment),mean)
#step 2: Create a new column and populate it correctly
seg.df$segIncome <- seg.income.mean[seg.df$Segment,2]
View (seg.df)


# Linear Regression 
# here we work with womens data set women datasets
View(women)
data(women)
women.df <- as.data.frame(women)

## Attach the Data frame to R
attach(women.df)
describe(women.df)
str(women.df)
plot(height,weight,xlab="HT in Inches",ylab="WT in lbs")
 fit <- lm(weight ~ height,data = women) 
 summary(fit)
# gives only thr Beta co-efficients 
fit$coefficients
# Range for 95% confidence interval 
confint(fit)
#Validating our model using the height given
women.df$weight
fitted(fit)
residuals(fit)
 

###Deans Dilemma Data 
dd <- read.csv(paste("DeansDilemmaData.csv",sep=""))
tail(dd)
str(dd)
hist(dd$Salary,dd$SlNo,data=dd,breaks=10, xlim=c(250000,800000))
plot(x=dd$SlNo+1, y=dd$Salary+1 ,log="xy")
max(dd$Salary)

histogram()
min(dd$Salary)
sum(count(dd$SlNo))
summary(dd$Salary)

str(dd)
summary(dd$Percent_SSC)
summary(dd$Percent_HSC)
summary(dd$Percent_Degree)
describe(dd$Salary)
plot()

hist(dd$Salary)
summary(dd$S.TEST.SCORE)
hist(newdata$Percent_MBA)

hist(newdata$Percent_MBA,notplaced$Percent_MBAxlim=c(50,80),ylim=c(0,150),xlab="MBAPercentage",
     ylab="Count",
     breaks=3,
     main="MBA Performance of placed students",
     col=c("lightblue"),layout=c(2,2))

       
par(mfrow=c(1,2))
hist(newdata$Percent_MBA,xlim=c(50,80),ylim=c(0,150),xlab="MBAPercentage",
     ylab="Count",
     breaks=3,
     main="MBA Performance of placed students",
     col=c("lightblue"),layout=c(2,2))
hist(notplaced$Percent_MBA,xlim=c(50,80),ylim=c(0,150),xlab="MBAPercentage",
     ylab="Count",
     breaks=3,
     main="MBA Performance of placed students",
     col=c("lightblue"),layout=c(2,2))
                                      

#######################End of Dean's Dilemma#############################   


####T_test Using Mass Library########
library(MASS)
str(Mass)
attach(UScrime)
UScrime
str(UScrime)
##### Assignment Week2 day6 ###########
View(state.x77)
state.df <- as.data.frame(state.x77)
str(state.df)
names(state.df)[4]<-"Life.Exp"
names(state.df)[6]<-"Hs.Grad"
View(state.df)
str(state.df)
fit<-lm(Life.Exp ~ ., data=state.df)
summary(fit)
model <- lm(Life.Exp ~ Hs.Grad + Murder, data = state.df)
summary(model)


########
store <- read.csv(paste("Store24.csv",sep=""))
head(store)
corrgram(store,)






dd <- read.csv(paste("SixAirlinesDataV2.csv",sep=""))
dd
min(dd$PriceEconomy)
