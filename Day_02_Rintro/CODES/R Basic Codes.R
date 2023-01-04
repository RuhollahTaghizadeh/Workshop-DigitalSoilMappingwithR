###################Basic Codes
###################Basic Codes
###################Basic Codes
a <-1 #assigning values to variables
b <-1
c <--1
(-b + sqrt(b^2 - 4*a*c))/(2*a)#solving the quadratic equation


ls() #To check the objects
help ()#To check the help
data()#To check the data
args(log)#To check the arguments of functions


data("iris")# loading the iris dataset
help(iris)
class(iris)# determining that the iris dataset is of the "data frame" class
str(iris)# finding out more about the structure of the object
head(iris)# showing the first 6 lines of the dataset
names(iris)# displaying the variable names in the iris dataset
iris$Sepal.Length# using the accessor operator to obtain the Sepal.Length column
hist(iris$Sepal.Length , breaks = 20, col= "orange")
length(iris$Sepal.Length)# determining how many entries are in a vector
class(iris$Sepal.Length)# vectors can be of class numeric and character
class(iris$Species)# vectors can be of class numeric and character
levels(iris$Species)# obtaining the levels of a factor
mean( iris$Sepal.Length[iris$Species=="versicolor"] ) # return the mean of Sepal.Length of versicolor
names(iris) = c("A", "B", "C","D","F") #chagne the names of the variables
names(iris)
data("iris")
names(iris)
x=iris$Sepal.Length
y=iris$Sepal.Width
mean(x)  
var(x)   
sd(x)    
sqrt(var(x))  
min(x)   
max(x)   
range(x) 
median(x) 
quantile(x) 
sort(x)  
summary(x)  
abs(x)   
log(abs(x))   
exp(x)   
seq(from=1 , to=100, by=1)  
rep(1:3, 2)  
hist(x)     
plot(x) 
data2 = cbind(x,y)
print(data2)
plot(data2)


z <- 3 == 2# logical vectors are either TRUE or FALSE
z
class(z)


#Install Packages
install.packages("randomForest")
library(randomForest)
installed.packages()


# reading data from external sources
setwd("")
SoilData = read.table(file = 'SoilData.txt', header = TRUE, sep = "\t")
SoilData = read.csv(file = 'SoilData.csv')
SoilData = read.xlsx(file = 'SoilData.xlsx', sheetName="SOC")


# exporting data to external sources
write.table(SoilData, file = "SoilData.txt", sep = "\t")
write.csv(SoilData, file = "SoilData.csv")
write.xlsx(SoilData, file = "SoilData.xlsx", sheetName = "SOC", col.names = TRUE, row.names = FALSE)


#plotting in R
demo(graphics) # pretty pictures...

data("iris")
names(iris)
x=iris$Sepal.Length
y=iris$Sepal.Width
plot(x=x,y= y, pch=17,col="red")
points(iris$Sepal.Length,iris$Petal.Length,pch=16,col="blue")

par(mfrow=c(1,2))
plot(x=x,y= y, pch=17,col="red")
title("Sepal.Length vs Sepal.Width")
plot(iris$Sepal.Length,iris$Petal.Length,pch=16,col="blue")
title("Sepal.Length vs Petal.Length")

library(corrplot) # corrolation
corrMat=cor(iris[,-ncol(iris)])
corrplot(corrMat, method = "number")

library(ggplot2) #ggplot
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram()

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density()

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point()
