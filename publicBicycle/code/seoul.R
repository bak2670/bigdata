# 서울 따릉이 데이터 전처리
library(dplyr)
data1 <- read.csv('D:/smithrowe/Rprogramming/project/data1.csv')
data2 <- read.csv('D:/smithrowe/Rprogramming/project/data2.csv')
data3 <- read.csv('D:/smithrowe/Rprogramming/project/data3.csv')
data4 <- read.csv('D:/smithrowe/Rprogramming/project/data4.csv')
data5 <- read.csv('D:/smithrowe/Rprogramming/project/data5.csv')
data6 <- read.csv('D:/smithrowe/Rprogramming/project/data6.csv')
colnames(data1) <- c('date', 'counts')
colnames(data2) <- c('date', 'counts')
colnames(data3) <- c('date', 'counts')
colnames(data4) <- c('date', 'counts')
colnames(data5) <- c('date', 'counts')
colnames(data6) <- c('date', 'counts')
data4 <- rbind(data4[1:31, 1:2], data4[180:331, 1:2])
data_test <- rbind(data1, data2, data3, data4, data5, data6)
data_test %>% tail(100)
data_test$counts <- as.numeric(gsub(",", "", data_test$counts))
data_test %>% arrange(date)
library(stringr)
class(data_test$count)
data_test$counts <- as.numeric(str_trim(data_test$counts))
data_test$month <- substr(data_test$date, 5, 6)
data_test$year <- substr(data_test$date, 1, 4)
write.csv(data_test, file='seoul_bicycle.csv', row.names = F)
temp_month <- data_test %>% group_by(month) %>% summarise(avg_month_count = mean(counts))

for (i in seq(1, 1127)){
  data_test$day[i] = ifelse(
    i%%7 == 0, 'SUN', ifelse(
      i%%7 == 1, 'MON', ifelse(
        i%%7 == 2, 'TUE', ifelse(
          i%%7 == 3, 'WED', ifelse(
            i%%7 == 4, 'TUR', ifelse(
              i%%7 == 5, 'FRI', 'SAT'
            )
          )
        )
      )
    ))
}

# 따릉이 월별, 요일별 사용량 그래프
library(ggplot2)
ggplot(data = temp_month, aes(month, avg_month_count)) + geom_col() + xlab('month') + ylab('counts')

temp_day <- data_test %>% group_by(day) %>% summarise(avg_month_count = mean(counts))
temp_123 <- data_test %>% filter(month == '04')
ggplot(data = temp_day, aes(day, avg_month_count)) + geom_col() + xlab('day') + ylab('counts') + scale_x_discrete(limit=c('MON', 'TUE', 'WED', 'TUR', 'FRI', 'SAT', 'SUN'))



# 서울 기상 데이터 전처리
# 미세먼지 합치고 자름
dust18 <- read.csv('D:/smithrowe/Rprogramming/project/dust/dust18.csv')
dust19 <- read.csv('D:/smithrowe/Rprogramming/project/dust/dust19.csv')
dust20 <- read.csv('D:/smithrowe/Rprogramming/project/dust/dust20.csv')
dust21 <- read.csv('D:/smithrowe/Rprogramming/project/dust/dust21.csv')
seoul_dust <- rbind(dust18, dust19, dust20, dust21[1:30, 1:4])
seoul_dust <- seoul_dust[3:4]
colnames(seoul_dust) <- c('date', 'dust')

# 날씨 합치고 자름
weather18 <- read.csv('D:/smithrowe/Rprogramming/project/weather/weather18.csv')[3:11]
weather19 <- read.csv('D:/smithrowe/Rprogramming/project/weather/weather19.csv')[3:11]
weather20 <- read.csv('D:/smithrowe/Rprogramming/project/weather/weather20.csv')[3:11]
weather21 <- read.csv('D:/smithrowe/Rprogramming/project/weather/weather21.csv')[3:11]
seoul_weather <- rbind(weather18, weather19, weather20, weather21[1:31, 1:9])
seoul_weather %>% head()
colnames(seoul_weather) <- c('date', 'avg_temp', 'min_temp', 'max_temp', 'rainfall', 'wind_speed', 'humidity', 'snowfall', 'ground_temp')

# 미세먼지 + 날씨
seoul_weather <- left_join(seoul_weather, seoul_dust, by='date')

# 따릉이 + 날씨
seoul_data <- left_join(data_test, seoul_weather, by='date')

# year, month
seoul_data$year <- substr(seoul_data$date, 1, 4)
seoul_data$month <- substr(seoul_data$date, 6, 7)
# NA처리
table(is.na(seoul_data))

# 강수량, 강설량량 NA
seoul_data$rainfall[is.na(seoul_data$rainfall)] <- 0
seoul_data$snowfall[is.na(seoul_data$snowfall)] <- 0

ggplot(data = seoul_data, aes(avg_temp, counts)) + geom_point()
write.csv(seoul_data, file='seoul_data.csv', row.names = F)
table(is.na(seoul_data))
seoul_data %>% filter(rainfall == 0.0 & snowfall == 0.0) %>% group_by(month) %>% summarise(avg_cnt = mean(counts))
table(!is.na(seoul_data$counts))
class(seoul_data$rainfall)
class(seoul_data$counts)
seoul_data %>% tail(100)
