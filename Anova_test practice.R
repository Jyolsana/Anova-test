### Code to implement ANOVA models using the aov() command
#   and to implement pairwise testing using the TukeyHSD() command
####################################################################

library(car)                   # for qqPlot function

# Import lakedata CSV files
Lake<-read.csv(file.choose(), header=TRUE)

#create response variables and factors using the columns of the CSV file
O2 = Lake[,2]         #response variable
Lk = factor(Lake[,1]) # lake no. coded as factor

##checking assumptions and transforming

boxplot(O2~Lk)      #boxplot by lake
O2_sq = sqrt(O2)    #square root transformation
boxplot(O2_sq~Lk)
aggregate(O2~Lk, FUN=sd)  #sd of original data
aggregate(O2_sq~Lk, FUN=sd)  

qqPlot(O2_sq~Lk)

### Implement one way ANOVA

anova1 = aov(O2_sq~Lk)
summary(anova1)

par(mfrow=c(2,2))			 # to get the plots as 2x2 grid
plot(anova1)                   #diagnostic plots for ANOVA

par(mfrow=c(1,1))      #back to 1 plot and not in a 2x2 grid
anova1[[2]]            #printing the residuals
qqPlot(anova1[[2]])    #QQ plot of the residuals

TukeyHSD(anova1)       #pairwise comparisons using Tukey's test