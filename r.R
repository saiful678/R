install.packages("tidyverse")
library(tidyverse)
df <- read.csv("C:/Users/Saifu/Downloads/Data Set- Inc5000 Company List_2014.csv")

head(df)
head(df,3)

tail(df)
tail(df,3)

str(df)

dim(df)

summary(df)

head(df["company"],5)

tail(df["state_l"],5)

sapply(df,class)

mean(df$workers)

mean(head(df$workers,5))

subset(df,subset = workers>1000)

filter(df,workers < 50)

max(df$workers)

min(df$workers)

subset(df,subset = workers==0)

max(df$growth)

subset(df,subset = growth>150000)

mean(df$revenue)

min(df$revenue)

max(df$revenue)

(filter(df,revenue >5000000000)

count(filter(df, revenue < mean(df$revenue)))

mean(df$"yrs_on_list")
min(df$"yrs_on_list")
max(df$"yrs_on_list")

duplicated(df)
sum(duplicated(df))

na.omit(df)

df$growth = round(df$growth, 0)

df[-2, ]

ggplot(df, aes(x=revenue, y=growth)) + 
  geom_boxplot(outlier.colour = "purple", outlier.shape = 1)+ 
  scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

mean(df$growth)
median(df$growth)

q1_growth = quantile(df$growth, 0.25)
q1_growth

IQR = q3_growth - q1_growth
IQR

IQR(df$growth)

q3_growth + 1.5 * IQR # upper boundary

upper_b = q3_growth + 1.5 * IQR # upper boundary
lower_b = q1_growth - 1.5*IQR ##lower boundary

min(df$growth)

#removing outliers
df_cleaned = subset(df, df$growth>lower_b & df$growth<upper_b)
dim(df)
dim(df_cleaned)

ggplot(df_cleaned, aes(x=rank, y=workers)) + 
  geom_point()+ scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim = c(0, 300))

ggplot(df_cleaned, aes(x=yrs_on_list)) + 
  geom_bar()+coord_cartesian(ylim = c(0, 1500), xlim=c(0,15))

#correlation between growth and revenue
cor(df_cleaned$growth, df_cleaned$revenue)


