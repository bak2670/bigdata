# 맥에서 한글을 보기 위한 코드
library(extrafont)
font_import() # y 해주기
theme_set(theme_grey(base_family='Poppins'))
theme_set(theme_grey(base_family='Noto Sans KR'))



library(dplyr)
library(ggplot2)
library(lubridate)
install.packages('stringr')
library(stringr)
install.packages('tidyverse')
library(tidyverse)


seoul <- read.csv(file = './seoul/seoul_data.csv')
seoul$day <- day(seoul$date)  # day행 추가
seoul <- subset(seoul, select = -c(date,min_temp,max_temp,humidity,snowfall,ground_temp)) # 필요없는 열 삭제
seoul$city <- 'Seoul' # city열 추가
seoul$date <- paste(as.character(seoul$year), as.character(seoul$month), as.character(seoul$day))  # year,month,day열 합침
seoul$date <- str_replace_all(seoul$date,' ','-') # 공백을 -로 치환
seoul$date <- as.Date(seoul$date) # date열 형식을 date로 변환
class(seoul$date)
seoul$weekday <- wday(seoul$date, label=TRUE) # 요일열 추가
View(seoul)

london <- read.csv(file = './london/london_data.csv')
london <- subset(london, select = -c(X)) # 필요없는 열 삭제
london <- select(london, bike_use,temp,rain,wind,dust,year,month,day)  # 열 순서 seoul과 동일하게 변경
View(london)
names(london) <- c('counts','avg_temp','rainfall','wind_speed','dust','year','month','day')  # 열 이름 서울과 동일하게 변경
london$city <- 'London' # city행 추가
london$date <- paste(as.character(london$year), as.character(london$month), as.character(london$day))  # year,month,day열 합침
london$date <- str_replace_all(london$date,' ','-') # 공백을 -로 치환
london$date <- as.Date(london$date) # date열 형식을 date로 변환
class(london$date)
london$weekday <- wday(london$date, label=TRUE) # 요일열 추가
london$wind_speed <- london$wind_speed * 0.44704  # 풍속 단위 통일
View(london)

seoul_london <- rbind(seoul, london)  # 두 데이터 합침
View(seoul_london)


# 1. 월별 서울+런던 라인그래프 - 전체
seoul_month <- data.frame(seoul %>% group_by(month, city) %>% summarise(counts = mean(counts)))
seoul_month

london_month <- data.frame(london %>% group_by(month, city) %>% summarise(counts = mean(counts)))
london_month

month_avg <- rbind(seoul_month, london_month)
ggplot(data = month_avg, aes(x=month, y=counts, color=city, group=city)) + geom_line() + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))
class(month_avg$month)


# 2. 월별 서울+런던 라인그래프 - 강수o
seoul_month_rainfall_o <- data.frame(seoul %>% filter(rainfall != 0.0) %>% group_by(month,city) %>% summarise(counts = mean(counts)))
seoul_month_rainfall_o

london_month_rainfall_o <- data.frame(london %>% filter(rainfall != 0.0) %>% group_by(month,city) %>% summarise(counts = mean(counts)))
london_month_rainfall_o

rainfall_o_avg <- rbind(seoul_month_rainfall_o, london_month_rainfall_o)
ggplot(data = rainfall_o_avg, aes(x=month, y=counts, color=city, group=city)) + geom_line() + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))


# 3. 월별 서울+런던 라인그래프 - 강수x
seoul_month_rainfall_x <- data.frame(seoul %>% filter(rainfall == 0.0) %>% group_by(month,city) %>% summarise(counts = mean(counts)))
seoul_month_rainfall_x

london_month_rainfall_x <- data.frame(london %>% filter(rainfall == 0.0) %>% group_by(month,city) %>% summarise(counts = mean(counts)))
london_month_rainfall_x

rainfall_x_avg <- rbind(seoul_month_rainfall_x, london_month_rainfall_x)
ggplot(data = rainfall_x_avg, aes(x=month, y=counts, color=city, group=city)) + geom_line() + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))


# 4. 월별 서울 막대 그래프 - 전체
seoul_month_1 <- data.frame(seoul %>% group_by(month, city) %>% summarise(counts = mean(counts)))
ggplot(data = seoul_month_1, aes(x=month, y=counts)) + geom_col(fill='#5ac48f') + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage")


# 5. 월별 서울 막대 그래프 - 강수o
seoul_month_1_rainfall_o <- data.frame(seoul %>% filter(rainfall != 0.0) %>% group_by(month) %>% summarise(counts = mean(counts)))
ggplot(data = seoul_month_1_rainfall_o, aes(x=month, y=counts)) + geom_col(fill='#5ac48f') + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage")


# 6. 월별 서울 막대 그래프 - 강수x
seoul_month_1_rainfall_x <- data.frame(seoul %>% filter(rainfall == 0.0) %>% group_by(month) %>% summarise(counts = mean(counts)))
ggplot(data = seoul_month_1_rainfall_x, aes(x=month, y=counts)) + geom_col(fill='#5ac48f') + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage")


# 7. 월별 런던 막대 그래프 - 전체
london_month_1 <- data.frame(london %>% group_by(month) %>% summarise(counts = mean(counts)))
ggplot(data = london_month_1, aes(x=month, y=counts)) + geom_col(fill='#e0503c') + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage")


# 8. 월별 런던 막대 그래프 - 강수o
london_month_1_rainfall_o <- data.frame(london %>% filter(rainfall != 0.0) %>% group_by(month) %>% summarise(counts = mean(counts)))
ggplot(data = london_month_1_rainfall_o, aes(x=month, y=counts)) + geom_col(fill='#e0503c') + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage")


# 9. 월별 런던 막대 그래프 - 강수x
london_month_1_rainfall_x <- data.frame(london %>% filter(rainfall == 0.0) %>% group_by(month) %>% summarise(counts = mean(counts)))
ggplot(data = london_month_1_rainfall_x, aes(x=month, y=counts)) + geom_col(fill='#e0503c') + scale_x_continuous(breaks=seq(0, 12, 1)) + ylim(0, 100000) + labs(x ="Month", y = "Public bike Usage")


# 10. 요일별 서울+런던 라인그래프 - 전체
weekday_df <- data.frame(seoul_london %>% group_by(weekday, city) %>% summarise(counts = mean(counts)))
weekday_df
ggplot(data = weekday_df, aes(x=weekday, y=counts, color=city, group=city)) + geom_line() + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))
class(weekday_df$weekday) # "ordered" "factor"


# 11. 요일별 서울+런던 라인그래프 - 강수o
weekday_df_rainfall_o <- data.frame(seoul_london %>% filter(rainfall != 0.0) %>% group_by(weekday, city) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_rainfall_o, aes(x=weekday, y=counts, color=city, group=city)) + geom_line() + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))


# 12. 요일별 서울+런던 라인그래프 - 강수x
weekday_df_rainfall_x <- data.frame(seoul_london %>% filter(rainfall == 0.0) %>% group_by(weekday, city) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_rainfall_x, aes(x=weekday, y=counts, color=city, group=city)) + geom_line() + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))


# 13. 요일별 서울 막대그래프 - 전체
weekday_df_seoul <- data.frame(seoul %>% group_by(weekday) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_seoul, aes(x=weekday, y=counts)) + geom_col(fill='#5ac48f') + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage")


# 14. 요일별 서울 막대그래프 - 강수o
weekday_df_rainfall_o_seoul <- data.frame(seoul %>% filter(rainfall != 0.0) %>% group_by(weekday) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_rainfall_o_seoul, aes(x=weekday, y=counts)) + geom_col(fill='#5ac48f') + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage")


# 15. 요일별 서울 막대그래프 - 강수x
weekday_df_rainfall_x_seoul <- data.frame(seoul %>% filter(rainfall == 0.0) %>% group_by(weekday) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_rainfall_x_seoul, aes(x=weekday, y=counts)) + geom_col(fill='#5ac48f') + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage")


# 16. 요일별 런던 막대그래프 - 전체
weekday_df_london <- data.frame(london %>% group_by(weekday) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_london, aes(x=weekday, y=counts)) + geom_col(fill='#e0503c') + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage")


# 17. 요일별 런던 막대그래프 - 강수o
weekday_df_rainfall_o_london <- data.frame(london %>% filter(rainfall != 0.0) %>% group_by(weekday) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_rainfall_o_london, aes(x=weekday, y=counts)) + geom_col(fill='#e0503c') + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage")


# 18. 요일별 런던 막대그래프 - 강수x
weekday_df_rainfall_x_london <- data.frame(london %>% filter(rainfall == 0.0) %>% group_by(weekday) %>% summarise(counts = mean(counts)))
ggplot(data = weekday_df_rainfall_x_london, aes(x=weekday, y=counts)) + geom_col(fill='#e0503c') + ylim(0, 60000) + labs(x ="Day of the Week", y = "Public bike Usage")



# 미세먼지
seoul_london_rmdust <- seoul_london %>% na.omit(seoul_london_rmdust$dust)  # 미세먼지 결측치 제거

# ggplot(data = seoul_london_rmdust, aes(x=dust, y=counts)) + geom_line()

seoul_london_rmdust$dustlevel <- ifelse(seoul_london_rmdust$dust >= 0 & seoul_london_rmdust$dust < 31, '1', ifelse(seoul_london_rmdust$dust >= 31 & seoul_london_rmdust$dust < 81, '2', ifelse(seoul_london_rmdust$dust >= 81 & seoul_london_rmdust$dust < 151, '3', '4')))
View(seoul_london_rmdust)
class(seoul_london_rmdust_df$dustlevel) # character


# 19. 미세먼지 서울+런던 라인그래프
seoul_london_rmdust_df <- data.frame(seoul_london_rmdust %>% group_by(dustlevel, city) %>% summarise(counts = mean(counts)))
seoul_london_rmdust_df
seoul_london_rmdust_df$dustlevel <- as.integer(seoul_london_rmdust_df$dustlevel)  # integer로 바꿔주지 않으면 라인그래프가 그려지지 않음
class(seoul_london_rmdust_df$dustlevel) # integer
ggplot(data = seoul_london_rmdust_df, aes(x=dustlevel, y=counts, color=city, group=city)) + geom_line() + ylim(0, 60000) + labs(x ="Fine dust Level", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))


# 20. 미세먼지 서울 막대그래프
seoul_london_rmdust_df <- seoul_london_rmdust %>% filter(city == 'Seoul') %>% group_by(dustlevel) %>% summarise(counts = mean(counts))
ggplot(data = seoul_london_rmdust_df, aes(x=dustlevel, y=counts)) + geom_col(fill='#5ac48f') + ylim(0, 60000) + labs(x ="Fine dust Level", y = "Public bike Usage")  # 범위 구분 막대그래프

seoul_london_rmdust_df2 <- seoul_london_rmdust %>% filter(city == 'Seoul') %>% group_by(dust, counts)
ggplot(data = seoul_london_rmdust_df2, aes(x=dust, y=counts)) + geom_point(color='#5ac48f') + xlim(0, 200) + ylim(0, 125000) + labs(x ="Fine dust Level", y = "Public bike Usage")  # 범위 구분없이 산점도


# 21. 미세먼지 런던 막대그래프
seoul_london_rmdust_df <- seoul_london_rmdust %>% filter(city == 'London') %>% group_by(dustlevel) %>% summarise(counts = mean(counts))
ggplot(data = seoul_london_rmdust_df, aes(x=dustlevel, y=counts)) + geom_col(fill='#e0503c') + ylim(0, 60000) + labs(x ="Fine dust Level", y = "Public bike Usage")  # 범위 구분 막대그래프

seoul_london_rmdust_df3 <- seoul_london_rmdust %>% filter(city == 'London') %>% group_by(dust, counts)
ggplot(data = seoul_london_rmdust_df3, aes(x=dust, y=counts)) + geom_point(color='#e0503c') + xlim(0, 200) + ylim(0, 125000) + labs(x ="Fine dust Level", y = "Public bike Usage")  # 미세먼지 구분없이 산점도


# 22. 풍속 서울+런던 라인그래프
# ggplot(data = seoul_london, aes(x=wind_speed, y=counts)) + geom_line()  # 풍속 범위 변경없이 전체 라인그래프
seoul_london_wind <- seoul_london %>% group_by(city)
ggplot(data = seoul_london_wind, aes(x=wind_speed, y=counts, color=city, group=city)) + geom_point() + xlim(0, 20) + ylim(0, 125000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))  # 풍속 범위 구분없이 전체 산점도

seoul_london_windspeed <- seoul_london
seoul_london_windspeed$wind_speedlevel <- as.integer(seoul_london_windspeed$wind_speed)  # 풍속 1단위로 범위 변경
View(seoul_london_windspeed)

seoul_london_windspeed <- data.frame(seoul_london_windspeed %>% group_by(wind_speedlevel, city) %>% summarise(counts = mean(counts)))
seoul_london_windspeed

ggplot(data = seoul_london_windspeed, aes(x=wind_speedlevel, y=counts, color=city, group=city)) + geom_line() + ylim(0, 60000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))  # 풍속 범위 1단위로 라인그래프
ggplot(data = seoul_london_windspeed, aes(x=wind_speedlevel, y=counts, color=city, group=city)) + geom_point() + ylim(0, 60000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London"))  # 풍속 범위 1단위로 산점도


# 23. 풍속 서울 막대그래프
seoul_windspeedlevel_df <- data.frame(seoul_london_windspeed %>% filter(city == 'Seoul') %>% group_by(wind_speedlevel) %>% summarise(counts = mean(counts)))
seoul_windspeedlevel_df
ggplot(data = seoul_windspeedlevel_df, aes(x=wind_speedlevel, y=counts)) + geom_col(fill='#5ac48f') + ylim(0, 60000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage")  # 풍속 서울 범위구분 산점도

seoul_wind <- seoul_london_wind %>% filter(city=='Seoul')
ggplot(data = seoul_wind, aes(x=wind_speed, y=counts)) + geom_point(color='#5ac48f') + xlim(0, 20) + ylim(0, 125000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage")  # 풍속 서울 범위구분 없이 산점도


# 24. 풍속 런던 막대그래프
london_windspeedlevel_df <- data.frame(seoul_london_windspeed %>% filter(city == 'London') %>% group_by(wind_speedlevel) %>% summarise(counts = mean(counts)))
london_windspeedlevel_df
ggplot(data = london_windspeedlevel_df, aes(x=wind_speedlevel, y=counts)) + geom_col(fill='#e0503c') + ylim(0, 60000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage")  # 풍속 런던 범위구분 산점도

london_wind <- seoul_london_wind %>% filter(city=='London')
ggplot(data = london_wind, aes(x=wind_speed, y=counts)) + geom_point(color='#e0503c') + xlim(0, 20) + ylim(0, 125000) + labs(x ="Wind speed (m/s)", y = "Public bike Usage")  # 풍속 런던 범위구분 없이 산점도


# 25. 기온 서울+런던 라인그래프
# ggplot(data = seoul_london, aes(x=avg_temp, y=counts)) + geom_line() # 기온 범위 구분없이 전체 라인그래프

seoul_london$templevel <- ifelse(seoul_london$avg_temp >= -15 & seoul_london$avg_temp < -10, '-15~-10', ifelse(seoul_london$avg_temp >= -10 & seoul_london$avg_temp < -5, '-10~-5', ifelse(seoul_london$avg_temp >= -5 & seoul_london$avg_temp < 0, '-5~0', ifelse(seoul_london$avg_temp >= 0 & seoul_london$avg_temp < 5, '0~5', ifelse(seoul_london$avg_temp >= 5 & seoul_london$avg_temp < 10, '5~10', ifelse(seoul_london$avg_temp >= 10 & seoul_london$avg_temp < 15, '10~15', ifelse(seoul_london$avg_temp >= 15 & seoul_london$avg_temp < 20, '15~20', ifelse(seoul_london$avg_temp >= 20 & seoul_london$avg_temp < 25, '20~25', ifelse(seoul_london$avg_temp >= 25 & seoul_london$avg_temp < 30, '25~30', '35~40')))))))))

seoul_london_temp <- data.frame(seoul_london %>% group_by(templevel, city) %>% summarise(counts = mean(counts)))
View(seoul_london_temp)

ggplot(data = seoul_london_temp, aes(x=templevel, y=counts, color=city, group=city)) + geom_line() + scale_x_discrete(limits = c("-15~-10", "-10~-5", "-5~0", "0~5", "5~10", "10~15", "15~20", "20~25", "25~30", "35~40")) + ylim(0, 70000) + labs(x ="Temperature", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London")) # 기온 범위 구분 서울+런던 라인그래프


# 26. 기온 서울 막대그래프
seoul_temp <- seoul_london_temp %>% filter(city == "Seoul") %>% group_by(templevel) %>% summarise(counts=mean(counts))
seoul_temp
ggplot(data = seoul_temp, aes(x=templevel, y=counts)) + geom_col(fill='#5ac48f') + scale_x_discrete(limits = c("-15~-10", "-10~-5", "-5~0", "0~5", "5~10", "10~15", "15~20", "20~25", "25~30", "35~40")) + ylim(0, 70000) + labs(x ="Temperature", y = "Public bike Usage")  # 범위 구분 막대그래프

seoul_temp2 <- data.frame(seoul %>% group_by(avg_temp, counts))
ggplot(data = seoul_temp2, aes(x=avg_temp, y=counts)) + geom_point(color='#5ac48f') + xlim(-15, 40) + ylim(0, 125000) + labs(x ="Temperature", y = "Public bike Usage")  # 범위 구분 없이 산점도


# 27. 기온 런던 막대그래프
london_temp <- seoul_london_temp %>% filter(city == "London") %>% group_by(templevel) %>% summarise(counts=mean(counts))
london_temp
ggplot(data = london_temp, aes(x=templevel, y=counts)) + geom_col(fill='#e0503c') + scale_x_discrete(limits = c("-15~-10", "-10~-5", "-5~0", "0~5", "5~10", "10~15", "15~20", "20~25", "25~30", "35~40")) + ylim(0, 70000) + labs(x ="Temperature", y = "Public bike Usage")  # 범위 구분 막대그래프

london_temp2 <- data.frame(london %>% group_by(avg_temp, counts))
ggplot(data = london_temp2, aes(x=avg_temp, y=counts)) + geom_point(color='#e0503c') + xlim(-15, 40) + ylim(0, 125000) + labs(x ="Temperature", y = "Public bike Usage")  # 범위 구분 없이 산점도


# 28. 강수량 서울+런던 라인그래프
sum(is.na(seoul_london$rainfall)) # 서울+런던 결측치 확인
class(seoul_london$rainfall)
# ggplot(data = seoul_london, aes(x=rainfall, y=counts)) + geom_line() # 강수량 범위 구분없이 전체 라인그래프
seoul_london_rainfall <- data.frame(seoul_london %>% group_by(rainfall, city) %>% summarise(counts = mean(counts)))

seoul_london_rainfall$rainfalllevel <- ifelse(seoul_london_rainfall$rainfall == 0, '0', ifelse(seoul_london_rainfall$rainfall > 0 & seoul_london_rainfall$rainfall <= 10, '0~10', ifelse(seoul_london_rainfall$rainfall > 10 & seoul_london_rainfall$rainfall <= 20, '10~20', ifelse(seoul_london_rainfall$rainfall > 20 & seoul_london_rainfall$rainfall <= 30, '20~30', ifelse(seoul_london_rainfall$rainfall > 30 & seoul_london_rainfall$rainfall <= 40, '30~40', ifelse(seoul_london_rainfall$rainfall > 40 & seoul_london_rainfall$rainfall <= 50, '40~50', ifelse(seoul_london_rainfall$rainfall > 50 & seoul_london_rainfall$rainfall <= 60, '50~60', ifelse(seoul_london_rainfall$rainfall > 60 & seoul_london_rainfall$rainfall <= 70, '60~70', ifelse(seoul_london_rainfall$rainfall > 70 & seoul_london_rainfall$rainfall <= 80, '70~80', ifelse(seoul_london_rainfall$rainfall > 80 & seoul_london_rainfall$rainfall <= 90, '80~90', ifelse(seoul_london_rainfall$rainfall > 90 & seoul_london_rainfall$rainfall <= 100, '90~100', '100~110')))))))))))
View(seoul_london_rainfall)

seoul_london_rainfall <- data.frame(seoul_london_rainfall %>% group_by(rainfalllevel, city) %>% summarise(counts = mean(counts)))
seoul_london_rainfall
ggplot(data = seoul_london_rainfall, aes(x=rainfalllevel, y=counts, color=city, group=city)) + geom_line() + scale_x_discrete(limits = c("0", "0~10", "10~20", "20~30", "30~40", "40~50", "50~60", "60~70", "70~80", "80~90", "90~100", "100~110")) + ylim(0, 70000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London")) + theme(axis.text.x=element_text(angle=45, hjust=1)) # 강수량 범위 구분 서울+런던 라인그래프

ggplot(data = seoul_london_rainfall, aes(x=rainfalllevel, y=counts, color=city, group=city)) + geom_point() + scale_x_discrete(limits = c("0", "0~10", "10~20", "20~30", "30~40", "40~50", "50~60", "60~70", "70~80", "80~90", "90~100", "100~110")) + ylim(0, 70000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") + scale_color_manual(values=c('Seoul'='#5ac48f', 'London'='#e0503c'), breaks=c("Seoul", "London")) + theme(axis.text.x=element_text(angle=45, hjust=1)) # 강수량 범위 구분 서울+런던 산점도


# 29. 강수량 서울 막대그래프
seoul_rainfall <- data.frame(seoul_london_rainfall %>% filter(city == "Seoul") %>% group_by(rainfalllevel) %>% summarise(counts = mean(counts)))
ggplot(data = seoul_rainfall, aes(x=rainfalllevel, y=counts)) + geom_col(fill='#5ac48f') + scale_x_discrete(limits = c("0", "0~10", "10~20", "20~30", "30~40", "40~50", "50~60", "60~70", "70~80", "80~90", "90~100", "100~110")) + ylim(0, 70000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") # 강수량 범위 구분 서울 막대그래프

seoul_rainfall2 <- data.frame(seoul_london_rainfall %>% filter(city == "Seoul") %>% group_by(rainfalllevel, counts))
ggplot(data = seoul_rainfall2, aes(x=rainfalllevel, y=counts)) + geom_point(color='#5ac48f') + scale_x_discrete(limits = c("0", "0~10", "10~20", "20~30", "30~40", "40~50", "50~60", "60~70", "70~80", "80~90", "90~100", "100~110")) + ylim(0, 70000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") # 강수량 범위 구분 서울 산점도

seoul_rainfall3 <- data.frame(seoul %>% group_by(rainfall, counts))
ggplot(data = seoul_rainfall3, aes(x=rainfall, y=counts)) + geom_point(color='#5ac48f') + xlim(0, 110) + ylim(0, 125000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") # 강수량 범위 구분 없이 서울 산점도


# 30. 강수량 런던 막대그래프
london_rainfall <- data.frame(seoul_london_rainfall %>% filter(city == "London") %>% group_by(rainfalllevel) %>% summarise(counts = mean(counts)))
ggplot(data = london_rainfall, aes(x=rainfalllevel, y=counts)) + geom_col(fill='#e0503c') + scale_x_discrete(limits = c("0", "0~10", "10~20", "20~30", "30~40", "40~50", "50~60", "60~70", "70~80", "80~90", "90~100", "100~110")) + ylim(0, 70000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") # 강수량 범위 구분 서울 막대그래프

london_rainfall2 <- data.frame(seoul_london_rainfall %>% filter(city == "London") %>% group_by(rainfalllevel, counts))
ggplot(data = london_rainfall2, aes(x=rainfalllevel, y=counts)) + geom_point(color='#e0503c') + scale_x_discrete(limits = c("0", "0~10", "10~20", "20~30", "30~40", "40~50", "50~60", "60~70", "70~80", "80~90", "90~100", "100~110")) + ylim(0, 70000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") # 강수량 범위 구분 서울 막대그래프

london_rainfall3 <- data.frame(london %>% group_by(rainfall, counts))
ggplot(data = london_rainfall3, aes(x=rainfall, y=counts)) + geom_point(color='#e0503c') + xlim(0, 110) + ylim(0, 125000) + labs(x ="Rainfall (mm)", y = "Public bike Usage") # 강수량 범위 구분 없이 런던 산점도
