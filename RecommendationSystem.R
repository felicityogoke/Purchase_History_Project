getwd()
library("data.table")
library("ggplot2")
library("recommenderlab")
library(dplyr)
library(tidyr)
library(pander)
library(plotly)


# IMPORTING DATA:
tbl<- read.csv("C:/Users/ogoke/OneDrive/Desktop/DATA SCIENCE CAPSTONE/PROJECT/DATA/transaction_history.csv", header = TRUE)
tbl<-tbl %>% separate(prod, c('categ', 'subcateg', 'product'), sep='_')

class(tbl)

tbl <- data.table(tbl)
tbl <-tbl[,categ := paste(categ, subcateg, sep="_")] 
tbl[,subcateg := NULL]

#View(tbl)

unique(tbl[,domestic]) # domestic or not
unique(tbl[,ind_code]) # there ind code
unique(tbl[,state]) # there is empty string

unique(is.na(tbl[,domestic]))  # no empty strings
unique(is.na(unique(tbl[,ind_code])))

# Feature Selection:
#we can focus on the users,products, and prices they purchased and drop the other columns:
# also convert it to datatable, for processing data easily.

table_users <- data.table(tbl$customer, tbl$categ,tbl$new_orders,tbl$domestic,tbl$ind_code)
colnames(table_users) <- c("Customers", "Products", "Price","Domestic", "Industrial code")  # Customer and product ID's

#View(table_users)




# --------------------------------------------- BUILDING PRICE MATRIX--------------------------------------

table_users[, value := Price]
price_wide <- reshape(data = table_users,
                      direction = "wide",
                      idvar = "Customers",
                      timevar = "Products",
                      v.names = "value")


#View(price_wide)
# In order to build the matrix, we need to keep only the columns containing the products status.
# In addition, the customer id will be the matrix row names, so we need to store them in the vector_customers vector:

vector_customers <- price_wide[, Customers] # store customers in vector

#View(price_wide)
price_wide[, Customers := NULL]
price_wide[, Price := NULL]
price_wide[, Domestic := NULL]
price_wide[,"Industrial code" := NULL]

# changing name of columns to the product id's
setnames(x = price_wide,
         old = names(price_wide),
         new = substring(names(price_wide), 7))



# We need to store the matrix within a recommenderlab object. 
# For this purpose, we need to convert table_wide into a matrix first. 
# In addition, we need to set the row names equal to the user names:

matrix_price_wide <- as.matrix(price_wide)   # convert to matrix
rownames(matrix_price_wide) <- vector_customers

#View(matrix_price_wide)

# transpose matrix price 
t_price_matrix <- t(matrix_price_wide)


dist_price_matrix <- dist(t_price_matrix,method = "cosine") # get distance

dist_price_matrix <-as.matrix(dist_price_matrix) # distance matrix
View(dist_price_matrix)

sim_prices<- 1 - dist_price_matrix # similarity matrix
#View(sim_prices)




# --------------------------------------------- BUILDING INDUSTRIAL CODE MATRIX --------------------------------------
#View(table_users2) <- table_user
table_users2 <- data.table(tbl$customer, tbl$categ,tbl$new_orders,tbl$domestic,tbl$ind_code)
colnames(table_users2) <- c("Customers", "Products", "Price","Domestic", "Ind_code")
#View(table_users2)



table_users2$Ind_code <- as.numeric(factor(table_users2$Ind_code))

table_users2[, value := Ind_code]

#View(table_users2)

is.numeric(table_users2$Ind_code)

Indcode_wide <- reshape(data = table_users2,
                      direction = "wide",
                      idvar = "Customers",
                      timevar = "Products",
                      v.names = "value")


View(Indcode_wide)
# In order to build the matrix, we need to keep only the columns containing the products status.
# In addition, the customer id will be the matrix row names, so we need to store them in the vector_customers vector:

vector_customers <- Indcode_wide[, Customers] # store customers in vector


Indcode_wide[, Customers := NULL]
Indcode_wide[, Price := NULL]
Indcode_wide[, Domestic := NULL]
Indcode_wide[,Ind_code := NULL]

# changing name of columns to the product id's
setnames(x = Indcode_wide,
         old = names(Indcode_wide),
         new = substring(names(Indcode_wide), 7))


# View(Indcode_wide)
# We need to store the matrix within a recommenderlab object. 
# For this purpose, we need to convert table_wide into a matrix first. 
# In addition, we need to set the row names equal to the user names:

matrix_Indcode_wide <- as.matrix(Indcode_wide)   # convert to matrix
rownames(matrix_Indcode_wide) <- vector_customers

View(matrix_Indcode_wide)

# transpose matrix ind_code 
t_Indcode_matrix <- t(matrix_Indcode_wide)

#View(matrix_Indcode_wide)
dist_Indcode_matrix<- dist(t_Indcode_matrix, method = "cosine") # get distance

dist_Indcode_matrix <- as.matrix(dist_Indcode_matrix) # distance matrix
View(dist_Indcode_matrix)

sim_Indcode <- 1 - dist_Indcode_matrix # similarity matrix
View(sim_Indcode)





# --------------------------------------------- BUILDING DOMESTIC MATRIX --------------------------------------
#View(table_users2) <- table_user
table_users3 <- data.table(tbl$customer, tbl$categ,tbl$new_orders,tbl$domestic,tbl$ind_code)
colnames(table_users3) <- c("Customers", "Products", "Price","Domestic", "Ind_code")
#View(table_users2)



table_users3$Domestic <- as.numeric(factor(table_users3$Domestic))

table_users3[, value := Domestic]


is.numeric(table_users3$Domestic)

Domestic_wide <- reshape(data = table_users3,
                        direction = "wide",
                        idvar = "Customers",
                        timevar = "Products",
                        v.names = "value")


View(Domestic_wide)
# In order to build the matrix, we need to keep only the columns containing the products status.
# In addition, the customer id will be the matrix row names, so we need to store them in the vector_customers vector:

vector_customers <- Domestic_wide[, Customers] # store customers in vector


Domestic_wide[, Customers := NULL]
Domestic_wide[, Price := NULL]
Domestic_wide[, Domestic := NULL]
Domestic_wide[,Ind_code := NULL]

# changing name of columns to the product id's
setnames(x = Domestic_wide,
         old = names(Domestic_wide),
         new = substring(names(Domestic_wide), 7))


# View(Indcode_wide)
# We need to store the matrix within a recommenderlab object. 
# For this purpose, we need to convert table_wide into a matrix first. 
# In addition, we need to set the row names equal to the user names:

matrix_Domestic_wide <- as.matrix(Domestic_wide)   # convert to matrix
rownames(matrix_Domestic_wide) <- vector_customers

View(matrix_Domestic_wide)

# transpose matrix ind_code 
t_Domestic_matrix<- t(matrix_Domestic_wide)

#View(matrix_Indcode_wide)
dist_Domestic_matrix<- dist(t_Domestic_matrix, method = "cosine") # get distance

dist_Domestic_matrix <- as.matrix(dist_Domestic_matrix) # distance matrix
View(dist_Domestic_matrix)

sim_Domestic<- 1 - dist_Domestic_matrix # similarity matrix
View(sim_Domestic)









# --------------------------------- BUILDING BINARY MATRIX --------------------------------------------------------------------------------

# coercing matrix_wide into a binaryRatingmatrix using as
matrix_price_wide[is.na(matrix_price_wide)] <- 0
Bin_Purchase_matrix <- as(matrix_price_wide, "binaryRatingMatrix")
Bin_Purchase_matrix # 2762 x 2013 rating matrix of class ‘binaryRatingMatrix’ with 75705 ratings.


#There are many items that have been purchased by a few users only, and we won't recommend them. Since they increase the computational time, we can just remove them 
# by defining a minimum number of purchases, for example, 10:
Bin_Purchase_matrix <- Bin_Purchase_matrix[, colCounts(Bin_Purchase_matrix) >= 10]
Bin_Purchase_matrix # 2762 x 716 rating matrix of class ‘binaryRatingMatrix’ with 77697 ratings.


image(Bin_Purchase_matrix[1:100, 1:100], main = "Binary Purchase matrix")


# However, there might be users(empty rows) that have purchased only items that we removed.

sum(rowCounts(Bin_Purchase_matrix) == 0) # 18


# we only keep users that have purchased at least ten items:
Bin_Purchase_matrix <- Bin_Purchase_matrix[rowCounts(Bin_Purchase_matrix) >= 10, ]
Bin_Purchase_matrix # 1345 x 716 rating matrix of class ‘binaryRatingMatrix’ with 67551 ratings.





# -------------------------------------- BUILDING MODEL -----------------------

# DATA SPLITTING:
set.seed(123)
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(Bin_Purchase_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))

data_train <- Bin_Purchase_matrix[which_train, ]
data_test  <- Bin_Purchase_matrix[!which_train, ]



# building an IBCF model using Recommender
# The Jaccard Index is a statistical measure that is frequently used to compare the similarity of binary variable sets.
# It is the length of the union divided by the size of the intersection between the sets.
# It compares members of the 2 sets to see which members are shared and which are distinct

recc_model <- Recommender(data = data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))

# The distances between similiarity of items are computed from the purchases.
#  The more the number of items purchased by the same users, the more similar they are.


class(recc_model@model$sim) #matrix
dim(recc_model@model$sim) # 716 716
image(recc_model@model$sim) # viewing the matrix

range(recc_model@model$sim) # distances are between 0 and 1

# -------------------------------------similarity matrix based on purchases----------------
# Starting from recc_model, we can define the purchases similarity matrix. 
sim_purchases<- as(recc_model@model$sim, "matrix") 

View(sim_purchases)

#--------------------------------item description based matrix--------------------------------

# Let's compare the dimensions of sim_prices,sim_purchases and sim_Indcode :
dim(sim_prices) # [1] 2013 2013
dim(sim_purchases) # [1] 716 716
dim(sim_Indcode)
class(sim_prices) # [1] "matrix" "array" 


# The dist_domestic table has more rows and columns because it contains all the items, 
# whereas dist_ratings contains only the ones that have been purchased.

#rownames(sim_prices) <- table_users[,Products]
#colnames(sim_prices) <- table_items[, Products]

View(sim_prices)
# Now, it's sufficient to extract the names from sim_purchases and subset sim_prices:
vector_items <- rownames(sim_purchases)
sim_prices <- sim_prices[vector_items, vector_items]


# extract the names from sim_purchases and subset sim_prices:

sim_Indcode <- sim_prices[vector_items, vector_items]

# Let's check whether the two matrices match:

identical(dim(sim_prices), dim(sim_purchases),dim(sim_Indcode))

identical(rownames(sim_purchases),rownames(sim_Indcode))

identical(colnames(sim_prices),colnames(sim_Indcode))

identical(colnames(sim_purchases),colnames(sim_Indcode))

#Everything is identical, so they match. Let's take a look at dist_domestic:
image(sim_prices)
image(sim_purchases)
image(sim_Indcode)



# We need to combine the two tables, and we can do it with a weighted average.
# we can set the weight of dist_domestic to 25 percent:

dist_tot <- sim_prices * 0.30 + sim_purchases * 0.40 + sim_Indcode*0.30


# Let's take a look at the dist_tot matrix using image:
image(dist_tot)
View(dist_tot)


# Now, we can include the new matrix within recc_model. 
#For this purpose, it'ssufficient to convert dist_tot into dgCMatrix and insert it in recc_model:
recc_model@model$sim <- as(dist_tot, "dgCMatrix")



n_recommended <- 10
recc_predicted <- predict(object = recc_model,
                          newdata = data_test,
                          n = n_recommended)


head(recc_predicted@itemLabels) # The itemLabels slot of recc_predicted contains the item names, that is, their code

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(Bin_Purchase_matrix)[x]})

View(recc_matrix)

recc_matrix[,"100612776"]

