#Instructions
#Set Working Directory in the datasets folder

#Install package quantmod files if not already installed
if(!'quantmod'%in%installed.packages()){
  install.packages('quantmod')
}

library(quantmod)

#Install package to open xlsx files if not already installed

if(!'openxlsx'%in%installed.packages()){
  install.packages('openxlsx')
}
library(openxlsx)

#Install package to read XLSX files if not already installed
if(!'readxl'%in%installed.packages()){
  install.packages('readxl')
}
library(readxl)

#Install package MatrixStats (if not already installed)

if(!'matrixStats'%in%installed.packages()){
  install.packages('matrixStats')
}
library(matrixStats)

#Install packages for plotting (if not already installed)
if(!'ggplot2'%in%installed.packages()){
  install.packages('ggplot2')
}
library(ggplot2)

#Install packages for plotting (if not already installed)
if(!'plotly'%in%installed.packages()){
  install.packages('')
}
library(plotly)


#Install packages for plotting (if not already installed)
if(!'cluster'%in%installed.packages()){
  install.packages('cluster')
}
library(cluster)


stocks<-read_excel('nasdaq_symbols.xlsx')
symbols<-stocks$Symbol

#Loading the daily returns for each stock
stock_data <- lapply(symbols, function(x){
  dailyReturn(na.omit(getSymbols(x, auto.assign = F)))
})

stocks_merged <- do.call(merge, stock_data)

#renaming the columns to assign each of them the name of the corresponding stock
colnames(stocks_merged)<-symbols
str(stocks_merged)

#creating a dataframe with average return and standard deviation, removing the NAs
average_return <- colMeans(stocks_merged, na.rm=T)
standard_deviation <- colSds(stocks_merged, na.rm=T)
df<-data.frame(average_return,standard_deviation)
head(df)

# is dayly profitability related with daily volatility?
#scatterplot between average return and standard deviation (volatility)

ggplotly(ggplot(df, aes(x = average_return, y = standard_deviation, text= rownames(df)))+geom_point()+
           geom_smooth(method = "lm", inherit.aes = F, aes(average_return, standard_deviation))+ggtitle("Relation between daily volatility and return"))

#Correlation coefficient
cor(df$average_return, df$standard_deviation, method = c("pearson", "kendall", "spearman"))
#Correlation test
cor.test(df$average_return, df$standard_deviation, method=c("pearson", "kendall", "spearman"))


#Create a clustering of companies, using the mean and sd of daily returns as 
#the spliting variables. Analize the results

#clustering with K-Means
#normalize

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

df$standard_deviation<- normalize(df$standard_deviation)
df$average_return<- normalize(df$average_return)
head(df)

set.seed(398742)
clus<-kmeans(df,3)
df$cluster<-clus$cluster
head(df)

#plot to understand if k= 3 is fine
clusplot(df, clus$cluster, color=TRUE, shade= FALSE, labels=0, lines=0, 
              main = "K-means clustering of companies")


#interactive plotting of the companies coloured by cluster

ggplotly(ggplot(df, aes(x = average_return, y = standard_deviation, colour = factor(cluster), text = rownames(df)))+
  geom_point() + ggtitle("K-means clustering of companies"))







