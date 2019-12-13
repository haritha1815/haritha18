install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
library(C50)
data()
data("BJsales")
sales <- read.csv(file.choose())
View(sales)
sales <- read.csv("C:/Users/HARITHA RAJJ/Downloads")
#########spitting of data

sales_Sales <- sales[sales$Sales=="Sales"]
sales_CompPrice <- sales[sales$CompPrice=="CompPrice"]


