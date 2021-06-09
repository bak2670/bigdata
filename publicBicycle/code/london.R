install.packages("readxl")
library(readxl)
london <- read_excel(path = "./london/tfl-daily-cycle-hires.xlsx", sheet = "Data")
View(london)

london_copy <- london

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

london_copy <- rename(london_copy,
  Number_of_Bicycle_Hires_Day = "Number of Bicycle Hires...2",
  NULL1 = "...3",
  Month = "Month...4",
  Number_of_Bicycle_Hires_Month = "Number of Bicycle Hires...5",
  NULL2 = "...6",
  Number_of_Bicycle_Hires_Year = "Number of Bicycle Hires...8",
  NULL3 = "...9",
  Month_Time = "Month...10"
)
View(london_copy)


# 일별 - 2018년 ~ 현재
london_copy_day <- london_copy %>% select(Day,Number_of_Bicycle_Hires_Day)
london_copy_day$Day <- as.Date(london_copy_day$Day)
class(london_copy_day$Day)
london_copy_day <- subset(london_copy_day, Day >= "2018/01/01")
View(london_copy_day)
ggplot(data=london_copy_day, aes(x=Day, y=Number_of_Bicycle_Hires_Day)) + geom_line()


# 요일별 - 평균 / 일요일 = 1
install.packages("tidyverse")
library(tidyverse)

install.packages("lubridate")
library(lubridate)

Weekday <- wday(london_copy_day$Day, label = TRUE)
class(Weekday)
london_copy_day$Weekday <- wday(london_copy_day$Day, label = TRUE)
View(london_copy_day)

Number_of_Bicycle_Hires_Day <- london_copy_day$Number_of_Bicycle_Hires_Day


Mon_filter <- london_copy_day %>% filter(Weekday == "Mon")
Mon_mean <- mean(Mon_filter$Number_of_Bicycle_Hires_Day)
Tue_filter <- london_copy_day %>% filter(Weekday == "Tue")
Tue_mean <- mean(Tue_filter$Number_of_Bicycle_Hires_Day)
Wed_filter <- london_copy_day %>% filter(Weekday == "Wed")
Wed_mean <- mean(Wed_filter$Number_of_Bicycle_Hires_Day)
Thu_filter <- london_copy_day %>% filter(Weekday == "Thu")
Thu_mean <- mean(Thu_filter$Number_of_Bicycle_Hires_Day)
Fri_filter <- london_copy_day %>% filter(Weekday == "Fri")
Fri_mean <- mean(Fri_filter$Number_of_Bicycle_Hires_Day)
Sat_filter <- london_copy_day %>% filter(Weekday == "Sat")
Sat_mean <- mean(Sat_filter$Number_of_Bicycle_Hires_Day)
Sun_filter <- london_copy_day %>% filter(Weekday == "Sun")
Sun_mean <- mean(Sun_filter$Number_of_Bicycle_Hires_Day)

Weekday <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
Weekday_mean <- c(Mon_mean, Tue_mean, Wed_mean, Thu_mean, Fri_mean, Sat_mean, Sun_mean)
df <- data.frame(Weekday, Weekday_mean)

ggplot(data=df, aes(x=Weekday, y=Weekday_mean)) + geom_col() + scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


# 월별 - 대여건수 총합
View(london_copy)
london_copy_month <- london_copy %>% select(Month,Number_of_Bicycle_Hires_Month)
london_copy_month$Month <- as.Date(london_copy_month$Month)
class(london_copy_month$Month)
london_copy_month <- subset(london_copy_month, Month >= "2018/01/01")
View(london_copy_month)

ggplot(data=london_copy_month, aes(x=Month, y=Number_of_Bicycle_Hires_Month)) + geom_col()


# 월별 - 대여건수 평균
london_copy_day$Month <- ifelse(month(london_copy_day$Day) == 01, "01", ifelse(month(london_copy_day$Day) == 02, "02", ifelse(month(london_copy_day$Day) == 03, "03", ifelse(month(london_copy_day$Day) == 04, "04", ifelse(month(london_copy_day$Day) == 05, "05", ifelse(month(london_copy_day$Day) == 06, "06", ifelse(month(london_copy_day$Day) == 07, "07", ifelse(month(london_copy_day$Day) == 08, "08", ifelse(month(london_copy_day$Day) == 09, "09", ifelse(month(london_copy_day$Day) == 10, "10", ifelse(month(london_copy_day$Day) == 11, "11", "12")))))))))))

View(london_copy_day)

month_mean_2018 <- data.frame(london_copy_day %>% filter(year(london_copy_day$Day) == 2018) %>% group_by(Month) %>% summarise(mean = mean(Number_of_Bicycle_Hires_Day)))

month_mean_2019 <- data.frame(london_copy_day %>% filter(year(london_copy_day$Day) == 2019) %>% group_by(Month) %>% summarise(mean = mean(Number_of_Bicycle_Hires_Day)))

month_mean_2020 <- data.frame(london_copy_day %>% filter(year(london_copy_day$Day) == 2020) %>% group_by(Month) %>% summarise(mean = mean(Number_of_Bicycle_Hires_Day)))

month_mean_2021 <- data.frame(london_copy_day %>% filter(year(london_copy_day$Day) == 2021) %>% group_by(Month) %>% summarise(mean = mean(Number_of_Bicycle_Hires_Day)))

month_mean <- rbind(month_mean_2018, month_mean_2019, month_mean_2020, month_mean_2021)

# ggplot(data = month_mean, aes(x=Month, y=mean)) + geom_col()  # 다 합쳐짐
# ggplot(data = month_mean_2018, aes(x=Month, y=mean)) + geom_col(fill="#30e291")
# ggplot(data = month_mean_2019, aes(x=Month, y=mean)) + geom_col()
# ggplot(data = month_mean_2020, aes(x=Month, y=mean)) + geom_col()
# ggplot(data = month_mean_2021, aes(x=Month, y=mean)) + geom_col()


# 연도별 월 평균
year_month_avg<- data.frame(london_copy_day %>% group_by(Month) %>% summarise(avg_month = mean(Number_of_Bicycle_Hires_Day)))
ggplot(data = year_month_avg, aes(x=Month, y=avg_month)) + geom_col()

write.csv(london_copy_day, "london_자전거_3년치.csv")
