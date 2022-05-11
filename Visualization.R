
# -----------------------------------EDA: VISUALIZATION--------------------------

# looking  at the matrix using image:
image(Bin_Purchase_matrix[1:100, 1:100], main = "Binary Purchase matrix")
# shows the first 100, we have the customers on each row and products on each column


# Plotting most popular products
n_users <- colCounts(Bin_Purchase_matrix)
n_users

dt <- data.table(names(n_users), n_users )

colnames(dt) <- c("Products", "Purchases")
View(dt)
library(plotly)

plt <- plot_ly(data=dt, x=~Products, y=~Purchases, type="bar") %>%
  layout(xaxis = list(categoryorder="total descending"))

plt 

#There are many items that have been purchased by a few users only, and we won't recommend them. Since they increase the computational time, we can just remove them 
# by defining a minimum number of purchases, for example, 10:
Bin_Purchase_matrix <- Bin_Purchase_matrix[, colCounts(Bin_Purchase_matrix) >= 10]
Bin_Purchase_matrix

image(Bin_Purchase_matrix[1:100, 1:100], main = "Binary Purchase matrix")


# However, there might be users(empty rows) that have purchased only items that we removed.

sum(rowCounts(Bin_Purchase_matrix) == 0) # 18


# we only keep users that have purchased at least ten items:
Bin_Purchase_matrix <- Bin_Purchase_matrix[rowCounts(Bin_Purchase_matrix) >= 10, ]
Bin_Purchase_matrix
