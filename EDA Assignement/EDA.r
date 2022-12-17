library(ggplot2)
data = read.csv('C:/Users/nqhun/DS311-Technologies-in-Data-Analytic/Week_9_Exploratory_Data_Analysis/EDA_Python/data/ames.csv')
View(data)
##
hist(data$SalePrice,col = "green",xlab = 'Sale Price',ylab = 'Distribution of Sale Prices')
abline(v = mean(data$SalePrice),col="red",lwd=3)
##

hist(data$TotRmsAbvGrd,col = 'blue')
abline(v = mean(data$TotRmsAbvGrd),col="red",lwd=3)
##
a = rep(NA,length(data$OverallCond))
for (i in 1:length(data$OverallCond))
{
  if (data$OverallCond[i]< 5){
    a[i] = "below_average_condition"
  }
  if (data$OverallCond[i]==5){
    a[i] = "average_condition"
  }
  if (data$OverallCond[i]> 5){
    a[i] = "above_average_condition"
  }
} 
#
ggplot(data = data, mapping = aes(x = SalePrice))+
  geom_histogram(aes(color=a,alpha = 0.2,fill=a),position = "identity", bins = 30, alpha = 0.4)+
  scale_color_manual(values = c("cyan", "gray","yellow")) +
  scale_fill_manual(values = c("cyan", "gray","yellow"))

#
boxplot(SalePrice~OverallQual,data = data,col=c(1:10))

#
data$age = data$YrSold-data$YearBuilt
plot(data$age,data$SalePrice,col = 'green',ylab = 'SalePrice',xlab = 'age')
