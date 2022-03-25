movies <- read.csv("Movies2016.csv", header = T)


movies

print("five num summary opening gross sales")
fivenum(movies$Opening.Gross.Sales.millions)
print("five num Total.Gross.Sales.millions")
fivenum(movies$Total.Gross.Sales.millions)
print("five num Number.of.Theaters")
fivenum(movies$Number.of.Theaters)
print("five num Weeks.in.Release")
fivenum(movies$Weeks.in.Release)

library(e1071)
skewness(movies$Opening.Gross.Sales.millions)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
getiqr <- function(i) {
    IQR(i)
}

getq1 <- function(i) {
    quantile(i,0.25)
}

getq3 <- function(i) {
    quantile(i,0.75)
}


row1 <- sapply(movies[, 2:5], mean)
row2 <- sapply(movies[, 2:5], median)
row3<- sapply(movies[, 2:5], getmode)
row4<- sapply(movies[, 2:5], var)
row5<- sapply(movies[, 2:5], sd)
row6<- sapply(movies[, 2:5], skewness)
row7<- sapply(movies[, 2:5], max)
row8<- sapply(movies[, 2:5], min)
row9<- sapply(movies[, 2:5], getiqr)
row10<- sapply(movies[, 2:5], getq1)
row11<- sapply(movies[, 2:5], getq3)

stats.table <- data.frame("mean" = row1, "median" = row2, "mode"=row3, "variance"=row4, "sd"=row5, "skewness"=row6, "max"=row7, "min"=row8, "IQR"=row9, "1st quartile"=row10,"3rd quartile"=row11)
stats.table


# stats.table2 <- t(stats.table)
# stats.table2



# mean(movies$Opening.Gross.Sales.millions)
# median(movies$Opening.Gross.Sales.millions)
# getmode <- function(v) {
#    uniqv <- unique(v)
#    uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# getmode(movies$Opening.Gross.Sales.millions)
# var(movies$Opening.Gross.Sales.millions)
# sd(movies$Opening.Gross.Sales.millions)
# max(movies$Opening.Gross.Sales.millions)
# min(movies$Opening.Gross.Sales.millions)
# max(movies$Opening.Gross.Sales.millions)-min(movies$Opening.Gross.Sales.millions)
# quantile(movies$Opening.Gross.Sales.millions, 0.25)
# quantile(movies$Opening.Gross.Sales.millions, 0.75)




library(dplyr)
library(ggplot2)
ggplot(movies, aes(x=(movies[,4]))) + 
  geom_histogram(binwidth=500, boundary=0, color="black", fill="white")+
     ggtitle ("Histogram to show the distribution for number of theaters")+
        xlab("number of theaters")
ggplot(movies, aes(x=(movies[,5]))) + 
  geom_histogram(binwidth=2, boundary=0, color="black", fill="white")+
     ggtitle ("Histogram to show the distribution for weeks in release")+
        xlab("weeks in release")
#number of theaters is moderately skewed to the left 
#Weeks in release is moderately skewed to the right with an outlier

v1z.score <- (movies$Opening.Gross.Sales.millions-mean(movies$Opening.Gross.Sales.millions))/sd(movies$Opening.Gross.Sales.millions)
v2z.score <- (movies$Total.Gross.Sales.millions
-mean(movies$Total.Gross.Sales.millions
))/sd(movies$Total.Gross.Sales.millions
)
v3z.score <- (movies$Number.of.Theaters-mean(movies$Number.of.Theaters))/sd(movies$Number.of.Theaters)
v4z.score <- (movies$Weeks.in.Release-mean(movies$Weeks.in.Release))/sd(movies$Weeks.in.Release)


#v1z.score
#v2z.score
#v3z.score
#v4z.score
#movies$Opening.Gross.Sales.millions
#movies$Total.Gross.Sales.millions
#movies$Number.of.Theaters	
#movies$Weeks.in.Release
movies$v1z.score <- with(movies,v1z.score)
movies$v2z.score <- with(movies,v2z.score)
movies$v3z.score <- with(movies,v3z.score)
movies$v4z.score <- with(movies,v4z.score)

movies


movies[which(movies[,7]>3),]

movies[which(movies[,7]<(-3)),]

#Rogue one and Finding Dory are outliers, which are Blockbusters.

#23.59 40.555 62.985 110.385 532.18
#Q3 +1.5IQR= 110.385 +1.5*(110.385-40.555)
#Q1- 1.5IQR=40.555 -1.5*(110.385-40.555)
110.385 +1.5*(110.385-40.555)
40.555 -1.5*(110.385-40.555)

movies[which(movies[,3]>215.13),]
#Yes there are changes as more movies are now classified as Blockbusters. 

movies[which(movies[,3]<(-64.19)),]


breaks <- c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180)
# specify interval/bin labels
tags <- c("[0-10)","[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)","[60-70)", "[70-80)","[80-90)", "[90-100)", "[100-110)", "[110-120)", "[120-130)", "[130-140)", "[140-150)", "[150-160)", "[160-170)", "[170-180)")
# bucketing values into bins
group_tags <- cut(movies$Opening.Gross.Sales.millions, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)

library("plyr")
dist_df <- count(group_tags )
str(dist_df)
colnames(dist_df)[1] <- "sales.interval"

library("reshape2")
n <- nrow(movies)
dist_df$rel.freq <- with(dist_df, freq/n)
dist_df

#rbind(dist_df, data.frame(sales.interval = "Total", t(colSums(dist_df[, -1]))))



#the most common number of weeks in release is from 10-19 weeks. 11 weeks most movie usually release.
stem(movies$Weeks.in.Release)

plot(x=movies$Opening.Gross.Sales.millions, y=movies$Total.Gross.Sales.millions, main="Scatter diagram", xlab="Opening gross sales", ylab="total gross sales")
#There is a strong positive linear relationship.

cor(movies$Opening.Gross.Sales.millions,movies$Total.Gross.Sales.millions)

elephants <- read.csv("AfricanElephants.csv", header = T)
print(elephants)
typeof(elephants[,1])
typeof(elephants[,2])
typeof(elephants[,3])
typeof(elephants[,4])
typeof(elephants[,5])
colnames(elephants)

#year_median <- sapply(elephants[, 2:5], median)

#year_median

#newelephants <- rbind(elephants, year_median)

#newelephants

year_median <- sapply(elephants[, 2:5], median)

lastrow <- c("Median value", year_median)

newelephants <- rbind(elephants, data.frame(data.frame(Country = "Year Median", t(year_median))))
newelephants



library(ggplot2)
library(reshape2)

graph1 <- newelephants[c(9,13,15),c(1:5)]
graph1
graph2 <- reshape(graph1,
                  direction = "long",
                  varying = list(names(graph1)[2:5]),
                  v.names = "elephants",
                  idvar = "Country",
                  timevar = "Year",
                  times = c(1979, 1989, 2007, 2012))

ggplot(graph2, aes(x = Year, y = elephants)) +
  geom_line(aes(color = Country), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FC4E07")) +
  theme_minimal()




names(elephants) <- c("Country","Year 1979","Year 1989","Year 2007","Year 2012")

boxplot(elephants[,2:5], col="orange", main="No of elephants", ylab="years", xlab="countries") 



X1979.Elephant.Population' 'X1989.Elephant.Population' 'X2007.Elephant.Population' 'X2012.Elephant.Population'


library(dplyr)
growth_rate = growth %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year1 = elephants$X1989.Elephant.Population - elephants$X1979.Elephant.Population,  # Difference in time (just in case there are gaps)
         Diff_growth1 = no of elephants - lag(no of elephants),
         Diff_year2 =elephants$X2007.Elephant.Population - elephants$X1989.Elephant.Population,
         Diff_growth2 = no of elephants - lag(no of elephants),
         Diff_year3 = elephants$X2012.Elephant.Population - elephants$X2007.Elephant.Population
         Diff_growth3 = no of elephants - lag(no of elephants),# Difference in route between years
         Mean annual change 1 = (Diff_growth1 / Diff_year1)^(0.1)
         Mean annual change 2= (Diff_growth2 / Diff_year2)^(1/18)
         Mean annual change 3= (Diff_growth3 / Diff_year3)^(0.2)



tab <- table(df$row_variable, df$column_variable)
tab
# growth rate in percent




