#Sangie Cheung, Jie (Joyce) Chen, Shirley Chan

csv_link = "https://guides.newman.baruch.cuny.edu/ld.php?content_id=39953204"
link_url = url(csv_link)
fb = read.csv(link_url,sep=";")

str(fb)
fb$Type = as.factor(fb$Type) #Set Type as a factor
fb$Post.Month = as.factor(fb$Post.Month) #Set Post.Month as a factor
fb$Post.Weekday = as.factor(fb$Post.Weekday) #Set Post.Weekday as a factor
fb$Post.Hour = as.factor(fb$Post.Hour) #Set Post.Hour as a factor

#Section 1, Question 3 
#Recoded the values in Category from 1 to "action", 2 to "product", and 3 to "inspiration"
fb$Category = as.character(fb$Category)
fb[fb$Category=="1","Category"] = "action"
fb[fb$Category=="2","Category"] = "product"
fb[fb$Category=="3","Category"] = "inspiration"
fb$Category = as.factor(fb$Category) #Set "Category" as a factor

#Section 1, Question 4
#Recoded the values in Paid from 0 to "non-paid" and 1 to "paid"
fb$Paid = as.character(fb$Paid)
fb$Paid[fb$Paid==0] = "non-paid"
fb$Paid[fb$Paid==1] = "paid"
fb$Paid = as.factor(fb$Paid) #Set "Paid" as a factor

#Section 1, Question 5
#Removed all entries with missing values
sum(is.na(fb)) #6 entries with missing values
fb = na.omit(fb)
sum(is.na(fb))

#Section 2, Question 1
summary(fb$share) #Returned the descriptive statistics for "shares"
sd(fb$share) #Returned the standard deviation for "shares"

#Section 2, Question 2
#Creates a histogram of the relationship between shares and posts.
hist(fb$share, xlab = "FB Shares", ylab = "# of Posts", 
     main = "Histogram of Shares", breaks = 100, xlim = c(0,200))

#Section 2, Question 3
cat_vars = c("Category", "Post.Month", "Paid")
summary(fb[,cat_vars]) #Returned the descriptive statistics for the variables: "Category", "Post.Month", and "Paid"
plot(fb$Post.Month, main = "Posts by Month", xlab = "Month", 
     ylab = "# of Posts", las = 2) #Creates a bar chart of posting frequency by month

#Section 3, Question 1
#Creates a scatter plot of the relationship between shares and likes
x = fb$like  
y = fb$share
plot(x,y, xlab = "Likes", ylab = "Shares", main = "Shares vs Likes",
     xlim = c(0,2000), ylim = c(0,225))

#Section 3, Question 2
#Creates a boxplot of the posts shares by month
box_x = fb$Post.Month
box_y = fb$share
plot(box_x,box_y, xlab = "Month", ylab = "Shares", ylim = c(0,80), main = "Boxplot of Shares by Month")

#Section 3, Question 3
#Compares performance (shares) between paid and non-paid posts using xtabs and aggregate
xtabs(fb$share ~ fb$Paid,data = fb)  
aggregate(fb$share ~ fb$Paid, data = fb, FUN = summary)

#Compares performance (shares) between paid and non-paid posts using boxplots
paid_x=fb$Paid
share_y=fb$share
plot(paid_x,share_y,main="Shares for Non-paid vs Paid posts",xlab="Non-paid and Paid Posts",
     ylab="Shares", ylim=c(0,70))

#Section 4, Question 1
par(mfrow=c(1,3)) #Creates 1x3 panel plot

#Creates a boxplot comparing non-paid and paid action posts
x=fb[fb$Category == "action", "Paid"]
y=fb[fb$Category == "action", "share"]
plot(x,y,main = "Action Posts Performance", ylab = "Shares", xlab = "Non-Paid and Paid Action Posts", ylim =  c(0,100), las = 1)

#Creates a boxplot comparing non-paid and paid inspiration posts
x=fb[fb$Category == "inspiration", "Paid"]
y=fb[fb$Category == "inspiration", "share"]
plot(x,y,main = "Inspiration Posts Performance", ylab = "Shares", xlab = "Non-Paid and Paid Inspiration Posts", ylim =  c(0,100), las = 1)

#Creates a boxplot comparing non-paid and paid product posts
x=fb[fb$Category == "product", "Paid"]
y=fb[fb$Category == "product", "share"]
plot(x,y,main = "Product Posts Performance", ylab = "Shares", xlab = "Non-Paid and Paid Product Posts", ylim =  c(0,100), las = 1)

