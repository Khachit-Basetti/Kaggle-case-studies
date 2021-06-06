# LIBRARIES

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)


# Load Data
sales_data = read.csv("C:/DataScience/DSP13/Hackthon/hack2/sales_train_v2.csv")
item_data = read.csv("C:/DataScience/DSP13/Hackthon/hack2/items.csv")
test_data = read.csv("C:/DataScience/DSP13/Hackthon/hack2/test.csv")
dim(sales_data)

head(sales_data)
head(item_data)
head(test_data)

View(sales_data)
View(item_data)
glimpse(test_data)

##Manipulating data### 
?merge
# get the item category details in the sales data
sales_data = merge(sales_data, item_data[, c("item_id", "item_category_id")], 
                   by = "item_id", all.x = TRUE)
head(sales_data)

View(sales_data)
attach(sales_data)
dim(sales_data)
colSums(is.na(sales_data))
colSums(sales_data == '') #luckily there are no NA or spaces in the date

sales_data$shop_id = as.numeric(sales_data$shop_id)
sales_data$item_id =  as.numeric(sales_data$item_id)
sales_data$item_category_id =  as.factor(sales_data$item_category_id) #since there are 84 categories

sales_data$date_block_num = as.numeric(sales_data$date_block_num)

str(sales_data)

sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")

sales_data$year = year(sales_data$date)
sales_data$day = day(sales_data$date)
sales_data$month = month(sales_data$date)
sales_data$weekdays =weekdays(sales_data$date)



sales_data$year =  as.factor(sales_data$year)
sales_data$month = as.factor(sales_data$month)
sales_data$day = as.factor(sales_data$day)
sales_data$weekdays = as.factor(sales_data$weekdays)

View(sales_data)
str(sales_data)

### EDA ####

# sales shop wise
sales_shopwise = sales_data %>%
  select(shop_id, item_cnt_day) %>%
  group_by(shop_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

#write.csv(sales_shopwise, "sales_shopwise.csv", row.names = F)
getwd()

str(sales_data)

ggplot(data =  sales_shopwise,aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = factor(shop_id))) +
  geom_histogram(stat = "identity") + #bins, identity
  xlab("Shop ID") + ylab("Sales Count")+
  ggtitle(label = "Shop wise sales") + coord_flip()



# sales item category wise
sales_categorywise = sales_data %>%
  select(item_category_id, item_cnt_day) %>%
  group_by(item_category_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))




#write.csv(sales_categorywise, "sales_categorywise.csv", row.names = F)
#setwd("F:/DSP13/Hackthon/hack2/Tableaudata")
#getwd()


ggplot(data =  sales_categorywise, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = factor(item_category_id))) +
  geom_histogram(stat = "identity", color = "black") +
  xlab("Item Category") + ylab("Sales Count") +
  ggtitle("Sale Item Category wise")


# which item is most popular and most sold in the each shop 

popularity =  sales_data %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  filter(sold_item_count == max(sold_item_count)) %>%
  arrange(desc(sold_item_count))

#write.csv(popularity, "popularity.csv", row.names = TRUE) #for tableau


# shop having most category of items 
shop_with_most_category = sales_data %>%
  select(shop_id, item_category_id) %>%
  group_by(shop_id) %>%
  summarise(category_count =  n_distinct(item_category_id)) %>%
  arrange(desc(category_count))

#write.csv(shop_with_most_category, "shop_with_most_category.csv", row.names = FALSE)

ggplot(data = shop_with_most_category,aes(x = reorder(shop_id, category_count),
                     y = category_count,
                     fill = factor(shop_id))) +
  geom_histogram(stat = "identity") +
  xlab("Shop ID") + ylab("Item Category Count") +
  ggtitle("Most Item category per shop")


# which item category is highest sales grossing in all shops
most_gross_category = sales_data %>%
  group_by(item_category_id) %>%
  summarise(total_gross = sum(item_cnt_day * item_price)) %>%
  arrange(desc(total_gross))

#getwd()
#setwd("F:/DSP13/Hackthon/hack2/Tableaudata")
#write.csv(most_gross_category, "Most_gross.csv", row.names = FALSE)

ggplot(most_gross_category, 
       aes(x = reorder(item_category_id, total_gross),
           y = total_gross,
           fill = factor(item_category_id))) +
  geom_histogram(stat = "identity", color = "black") +
  xlab("Category ID") + ylab("Total Gross")+
  ggtitle("Total Gross per Item category") +
  coord_flip()



# item categories available in each shop 
item_category_in_shops = sales_data %>%
  group_by(shop_id) %>%
  summarise(item_category = n_distinct(item_category_id)) %>%
  arrange(desc(item_category))

head(item_category_in_shops)



#**************************+*+*+*+*+*+*+*************************************

# day and month wise total sales 
month_daywise_total_sales =  sales_data %>%
  group_by(month, day) %>%
  summarise(total_sales =  sum(item_price * item_cnt_day))

ggplot(month_daywise_total_sales, 
       aes(x = day, y = total_sales, group =  month, color =  factor(month))) +
  geom_line() + geom_point() +
  labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Months") 


ggplot(month_daywise_total_sales, 
       aes(x = day, 
           y = total_sales, 
           fill =  factor(day))) +
  geom_histogram(stat = "identity", color = "black") +
  labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Days") +
  facet_wrap(~month, ncol = 2)


# year wise total sales
yearly_sales = sales_data %>%
  group_by(year) %>%
  summarise(yearly_sale = sum(item_price * item_cnt_day))

ggplot(yearly_sales, aes(x =  year, y = yearly_sale, fill = factor(year)))+
  geom_histogram(stat = "identity", color = "black")+
  labs(title = "Yearly Sales", x = "Year", y = "Total Sale", fill = "Year")

# year and month wise total sales 
ym_sales = sales_data %>%
  group_by(year, month) %>%
  summarise(ym_sale = sum(item_price*item_cnt_day)) %>%
  arrange(year)

ggplot(ym_sales, aes(x =  month, y = ym_sale, fill =  factor(year)))+
  geom_histogram(stat = "identity", color = "black") +
  labs(title = "Yearly-Monthly Sales", x = "Months", y =  "Total sales", fill = "Year")


# percent of items sold each month

# number of items sold each day 
daily_sale = sales_data %>%
  group_by(date) %>%
  summarise(items_sold =  sum(item_cnt_day))

ggplot(daily_sale, aes(x =  date, y = items_sold, color =  items_sold)) +
  geom_line() + geom_point()+
  labs(title = "Daily Item sold", x =  "Date", y = "Items sold")


# items sold on weekdays 
weekdays_item_sold = sales_data %>%
  group_by(weekdays) %>%
  summarise(item_sold = sum(item_cnt_day)) %>%
  arrange(desc(item_sold))

ggplot(weekdays_item_sold, aes(x =reorder(weekdays, item_sold),
                               y =  item_sold,
                               fill = factor(weekdays)))+
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Items sold on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
  coord_flip()

h# sale revenue on weekdays
weekdays_sales = sales_data %>%
  group_by(weekdays) %>%
  summarise(total_sale = sum(item_cnt_day * item_price)) %>%
  arrange(desc(total_sale))

ggplot(weekdays_sales, aes(x =reorder(weekdays, total_sale), y =  total_sale, fill = factor(weekdays)))+
  geom_bar(stat = "identity", color ="black") +
  labs(title = "Sales on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
  coord_flip()

## End of EDA ####

sales<- sales_data[sample(nrow(sales_data), nrow(sales_data)*0.2),]
View(sales)
attach(sales)

set.seed(123)
sample <- sample(nrow(sales), nrow(sales)*0.6)
train_data <- sales[sample,]
test_data <- sales[-sample,]


## Linear Regression Model ####
model = lm(item_cnt_day ~. -shop_id -date,
                  data = train_data) 

colnames(train_data)
summary(model)

#model Validation
prediction = predict(model, train_data)

plot(train_data$item_cnt_day, type="l", col= "green")
lines(prediction, type = "l", col = "blue")


#For Test Data
prediction = predict(model, test_data)

#with SVM

library(e1071)
#or alternate ggplot
m1 <- svm(item_cnt_day~ ., data = train_data, 
          kernel = "radial",
          cost = 2, gamma = 0.5)

summary(m1)

#prediction
p1 <- predict(m1, train)


#model tuning
set.seed(123)  #for tunning we do, else the values change
tune1 <- tune(svm, item_cnt_day~., data = train_data,
              ranges = list(cost = c(0.1,1,10,100,1000),
                            gamma = c(0.5, 1, 2, 3, 4)))
#best model
tune1$best.model

#for test data
test$num <- as.factor(test$num)

str(test)

p2 <- predict(m1, newdata = test_data)

#tuning
set.seed(123)  #for tunning we do, else the values change
tune1 <- tune(svm, num~., data = test,
              ranges = list(cost = c(0.1,1,10,100,1000),
                            gamma = c(0.5, 1, 2, 3, 4)))
#best model
tune1$best.model

## Linear Regression Model ####
linear_model = lm(formula = item_cnt_day ~ shop_id + item_id,
                  data = sales_data) 

result = predict(linear_model, test_data[,c("shop_id","item_id")]) 

submission =  data.frame(ID = test_data$ID,
                         item_cnt_month = result)
head(submission)
#write.csv(submission, file = "sub1.csv", row.names = F)

# GBM Model
library(tictoc)
tic("Time Taken to Run GBM Model ")
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id,
                  data = sales_data,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 5, 
                  bag.fraction = 0.5,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = -1,
                  verbose = T)

toc()
result2 = predict(gbm_model,newdata = test_data[,c("shop_id","item_id")], n.trees = 1000)

submission =  data.frame(ID = test$ID,
                         item_cnt_month = result2)
View(submission)
#write.csv(submission, file = "sub1.csv", row.names = F)

getwd()
setwd("F:/DSP13/Hackthon")
#***************************************

#time series

ts_data<-sales_data%>%
  mutate(year=year(date), month=month(date), day=day(date))%>%
  group_by(year,month)%>%
  summarise(item_cnt_month=sum(item_cnt_day))%>%
  select(item_cnt_month) %>%
  ts(frequency = 12,start = 2013 )

head(ts_data)
View(ts_data)
plot(ts_data)
