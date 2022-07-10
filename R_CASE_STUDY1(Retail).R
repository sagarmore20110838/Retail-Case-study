## Changeing working directory : 
getwd()
setwd("C:/Users/ACER/Downloads/R/R/R case study 1 (Retail)")
library(dplyr)
library(ggplot2)

## Import files : 
cust = read.csv("Customer.csv")
prod = read.csv("prod_cat_info.csv")
trans  = read.csv("Transactions.csv")

# 1.	Merge the datasets Customers, Product Hierarchy and Transactions 
# as Customer_Final. Ensure to keep all customers who have done transactions 
# with us and select the join type accordingly.

cust_trans = inner_join(trans , cust , by= c("cust_id"="customer_Id"))


final = inner_join(cust_trans , prod, by =c("prod_cat_code","prod_subcat_code"="prod_sub_cat_code" ))

#2.	Prepare a summary report for the merged data set.
#a.	Get the column names and their corresponding data types

str(final)

##b.	Top/Bottom 10 observations
head(final, 10)

tail(final,10)


# c.“Five-number summary” for continuous variables 
#(min, Q1, median, Q3 and max)
summary(final)

##d.	Frequency tables for all the categorical variables

final_cat = final[, c("transaction_id" ,"Store_type", "DOB", "Gender", "prod_cat", "prod_subcat")]

length(final_cat$transaction_id)

var_name = c("Store_type", "Gender", "prod_cat", "prod_subcat")

for (i in var_name){
print(paste( i,length(final_cat[,i])))
}


## 3.	Generate histograms for all continuous variables and frequency bars
##for categorical variables.

for (i in var_name){
  barplot(table(final_cat[,i]),,main = i)
}

final_num = final[ ,c(1,2,4,5,6,7,8,9,13)]

num_var_name = colnames(final_num)

for (i in num_var_name){
  hist(table(final_num[,i]),,main = i)
}


## 4.	Calculate the following information using the merged dataset :
## a.	Time period of the available transaction data

time_1 = max(as.Date(final$tran_date , format = "%d/%m/%Y") ,na.rm = T)

time_2 =min(as.Date(final$tran_date , format = "%d/%m/%Y") ,na.rm = T)

difftime(time_1, time_2 , units = "day")
difftime(time_1, time_2 , units = "week")
difftime(time_1, time_2 , units = "hours" )



#b.	Count of transactions where the total amount of 
# transaction was negative
count_neg = 0


 for (i in 1:23053){
   if(substr(final$total_amt[i] , 1,1) == "-" )
     {
     count_neg = count_neg+1
   }
 }

print(count_neg)
## Its 2177 number of negative transaction in data.



#5.	Analyze which product categories are more popular
#among females vs male customers.

summarise(group_by(final,Gender, prod_cat), table(prod_cat))

## More poplar category : 
## In Male -: Books 
## In Female -: Books


##6.	Which City code has the maximum customers and what was 
##    the percentage of customers from that city? 

code_cust = summarise(group_by(final , city_code) , table(city_code))

code_cust$percentage = (code_cust$`table(city_code)`/length(final$city_code))*100

## maximum cust belongs to city code of 4 and 
## percentage is 10.5 percent 


## 7.	Which store type sells the maximum products by value 
## and by quantity?

summarise(group_by(final , Store_type), length(Store_type))
summarise(group_by(final , Store_type), sum(total_amt))

## by quantit  - : e-shop 
## by sales  -:  e-shop 


##8.	What was the total amount earned from the "Electronics" and
## "Clothing" categories from Flagship Stores?

flagship = final[final$Store_type == "Flagship store", ]

new_flagship = flagship [flagship$prod_cat ==c("Clothing", "Electronics"),]

summarise(group_by(new_flagship,prod_cat), total_sales =sum(total_amt))




## 9.	What was the total amount earned from "Male" customers under
## the "Electronics" category?

final%>%filter(prod_cat == "Electronics")%>%group_by(Gender)%>%summarise(total_sales = sum(total_amt))

## Male total_sales by electronics category = 5703109 


## 10.	How many customers have more than 10 unique transactions, 
##after removing all transactions which have any negative amounts?
profit_trans = final[!substr(final$total_amt,1,1)=="-",]
new_profit_trans =  summarise(group_by(profit_trans, cust_id,transaction_id), t_count = length(transaction_id))
new_profit_trans =  summarise(group_by(new_profit_trans, cust_id), t_count = count(transaction_id))

new_profit_trans[new_profit_trans$t_count>10,]

## 11.	For all customers aged between 25 - 35, find out:
## a.	What was the total amount spent for “Electronics” and
## “Books” product categories?
final$DOB =  as.Date(final$DOB, format = "%d-%m-%Y")
final$age = (as.integer(round(difftime(Sys.Date(),final$DOB)/365)))
new_data = final[(final$age>25 & final$age<35),]

new_data = new_data[new_data$prod_cat ==c("Electronics","Books"), ]

summarise(group_by(new_data ,prod_cat ), total_sales =sum(total_amt))



#b.	What was the total amount spent by these customers between 
#1st Jan,2014 to 1st Mar, 2014?
install.packages("stringr")
library("stringr")
new_data$tran_date

for (i in 1:1180){
  new_data$tran_date[i]=str_replace_all(new_data$tran_date[i],"/","-")

}
new_data_copy = new_data
new_data_copy$tran_date= as.Date(new_data_copy$tran_date, format = "%d-%m-%Y")
new_data_copy$tran_date
new_data_copy = new_data_copy[new_data_copy$tran_date>("2014/01/01")& new_data_copy$tran_date<("2014/03/01"),]

sum(new_data_copy$total_amt)


## the total amount is 106089.9 















  











 
















































