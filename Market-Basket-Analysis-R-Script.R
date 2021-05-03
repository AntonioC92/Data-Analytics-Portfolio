

library(stringr) # for all functions and arguments to be consistent
library(reshape2) # for using data tables
library(plyr) # for implementing the split-apply-combine pattern and break down into small pieces
library(dplyr) #useful for data manipulation
library(tidyr) # it helps creating tidy data where every column is a variable and every row is an observation
library(miceadds) # Contains functions for multiple imputation which complements existing functionality in R
library(mice) #requested for miceadds to work
library(lubridate) # good for manipulating dates
library(arules) #useful for visualization techniques
library(arulesViz) #extension of arules
library(reshape2) # useful for transforming data between wide and long format
library(tidyverse) #useful for sharing common data representations and API design
library(knitr) # good for dynamic report generation
library(ggplot2) # for visualising plot
library(jsonlite) # needed to activate Aruleviz


#1st step####

#Imported data from csv file

Groceries <- read.csv(file="Groceries.csv", header = FALSE, sep = ",", na.strings = c(""," "))#read the file, 
set.seed(123) 



#2nd Step: Factual analysis

nrow(Groceries) #1499 rows
ncol(Groceries)#35 variables
str(Groceries)# all the columns except the last one which is logical are characters
complete.cases(Groceries) # there are blank space or NA

Groceries$V1 #checked the first column to see the data


#3rd step: Used the read.table function separator function to see the column of interest(#http://www.ddiez.com/teac/r/basics.php)
Groceries1 <- read.table("Groceries.csv", header = FALSE, sep = "\t")

head(Groceries1) #read the file

#4th Step: split the data appearing in column V1 and created two separate variables with proper names (https://stackoverflow.com/questions/32042621/how-to-split-column-into-two-in-r-using-separate

Groceries2 <- separate(Groceries1, V1, into = c('date', 'product'), sep="(?<=[0-9])(?=[A-Za-z])")

head(Groceries2) #quick overview, now with two columns with dates and products


#5th Step: cleaned and convert the date column into a date since it was originally a character
Groceries_cleaned<- Groceries2[c("date", "product")]
Groceries_cleaned2 <- Groceries_cleaned[!is.na(Groceries_cleaned$product), ] #Na removed
is.na(Groceries_cleaned2)#confirmed

Groceries_cleaned3 <- arrange(Groceries_cleaned2, date, product)
Groceries_cleaned3$date <-  as.Date(Groceries_cleaned3$date)
Groceries_cleaned3$product <-  as.factor(Groceries_cleaned3$product)


Groceries_cleaned3 #view

glimpse(Groceries_cleaned3) #it worked


#6th Step: added a transaction ID column, useful later to convert the datafreame into transactions
Groceries_cleaned3$transactionID <- seq(nrow(Groceries_cleaned3))
Groceries_cleaned3$transactionID <- as.factor(Groceries_cleaned3$transactionID)

str(Groceries_cleaned3) # now there are 3 variables 


#initiated the conversion into transactions

transactionData <- ddply(Groceries_cleaned3, c("transactionID", "date"),
                         function(df1) paste(df1$product,
                                             collapse = ","))

#NULL is used as thesecolumns are no longer relevant
transactionData$date <-NULL
transactionData$transactionID <- NULL


colnames(transactionData) <- c("Items") 


transactionData 
#For the format for transaction data is called the basket format it has to be stored next into a .csv

write.csv(transactionData,"~/Desktop/R programming/New Data Mining for class/First CA FIRST PART/Groceries_market_basket_transactions.csv", quote = FALSE, row.names = FALSE)

#
Transactions <- read.transactions('~/Desktop/R programming/New Data Mining for class/FIRST CA FIRST PART/Groceries_market_basket_transactions.csv', format = 'basket', sep=',')


#The summary(tr) is a very useful command that gives us information about our transaction object.
summary(Transactions)

#1500 Transactions (rows) are collections of 39 items (Columns). 
#Density is 0.373556 and tells the percentage of non zero cells in a sparse matrix
#1500 x 39 x 0.37355556 = 21,853 ITEMS WERE PURCHASED
  

#Visualisation: Created an item frequency plot with the package RColorBrewer

if(!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

itemFrequencyPlot(Transactions,topN = 10, type = "relative", col = brewer.pal(8, 'Pastel2'), 
                  main= "Relative Product Frequency Plot")
#Vegetables are the items that were absolutely mostly purchased and accounted for so over 70% of frequency in the transactions
#poultries accounted for 40%, the other 8 slightly below

################################APRIORI ALGORITHM##################################

#1st Step: Application of the APRIORI algorithm.The function apriori() is from the package arules 

##application with 0.4 confidence and application with min 3 and max 3 lenght of combination of products
fitmodelfinal <- apriori(Transactions, parameter = list(confidence = 0.4, support = 0.1, minlen = 3, maxlen = 3))
fitmodelfinal<- sort(fitmodelfinal, by="support")

inspect(fitmodelfinal[1:10]) #a confidence of 0.85%, means that 85% of people who bought eggs and soda also bought vegetables


#a summary of interpretation
summary(fitmodelfinal) #interpretation below
#total number of rules : the set of 1,820 rules
#Distribution of rule length: A length of 3 items has the most rules: 1820 that correspond to 1500 transactions


########### PLOTTING RESULTS#####


library(arulesViz)

#### 1st plot  used
top10fit <- head(fitmodelfinal, n=10, by="confidence")# plot below selecting 10 rules with the highest confidence
plot(top10fit, method = "graph", engine = "htmlwidget") 

### 2nd plot: Individual Rule Representation also called as Parallel Coordinates Plot. 
subfit <- head(fitmodelfinal, n=5, by="lift") #filters top 5 rules with the highest lift 
plot(subfit, method = "paracoord")

############################# End of script ############







