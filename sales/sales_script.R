# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
all_sales <- list.files(path = "./sales", pattern = ".csv")

# use a loop to load all datasets 
i <- 1
for (salesdata in all_sales){
  datafile <- read_csv(paste0("./sales/", salesdata))
  assign(paste0("dataset_", i), datafile)
  i <- i + 1
}

############## JOIN ###############
# Use a tidyverse join to join all the data together into one file
# called sales_data, then run the rest of the code

## Not the best way I believe but it worked.\
# I could not join more then three data frame at the time, that is why
# I did these many steps. 
sales_data1 <- full_join(dataset_1, dataset_2, dataset_3,
                       by = c("day", "month", "year", "pizza", "number"))

sales_data2 <- full_join(dataset_4, dataset_5, dataset_6,
                         by = c("day", "month", "year", "pizza", "number"))

sales_data3 <- full_join(dataset_7, dataset_8, dataset_9,
                         by = c("day", "month", "year", "pizza", "number"))


sales_data4 <- full_join(dataset_10, dataset_11, dataset_12,
                         by = c("day", "month", "year", "pizza", "number"))


sales_data5 <- full_join(dataset_13, dataset_14, dataset_15,
                         by = c("day", "month", "year", "pizza", "number"))

sales_data6 <- full_join(dataset_15, datafile,
                         by = c("day", "month", "year", "pizza", "number"))

sales_data7 <- full_join(sales_data1, sales_data2, sales_data3, 
                        by = c("day", "month", "year", "pizza", "number"))

sales_data8 <- full_join(sales_data4, sales_data5, sales_data6, 
                         by = c("day", "month", "year", "pizza", "number"))

sales_data  <- full_join(sales_data7, sales_data8, 
                         by = c("day", "month", "year", "pizza", "number"))


?get
?full_join

# Loop, easier and more pratical way to do it. 
sales_data <-dataset_1 
for (i in 2:length(all_sales)) {
  get(paste("dataset_", i, sep = "")) -> newdata
  sales_data <- full_join(sales_data, newdata)
}


########################################

##### 3. Create summaries #####
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")

