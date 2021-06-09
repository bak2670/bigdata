library(dplyr)
bike <- read.csv('C:/Users/tongy/Programming/BigDataUI/R/london/london_bike.csv')
weather <- read.csv('C:/Users/tongy/Programming/BigDataUI/R/london/london_weather.csv')
# 2월데이터 남아있는 결측치 제거
bike
bike %>% arrange(desc(X))
table(is.na(bike$X))
bike_copy <- bike %>% filter(!is.na(bike$X))
bike_copy
# df로 읽어옴
length(bike_copy$X)
length(weather$year)


# Day를 쪼개서 year month day로 나누자

bike_copy$year <- as.numeric(substr(bike_copy$Day,1,4))
bike_copy$month <- bike_copy$Month
bike_copy$day <- as.numeric(substr(bike_copy$Day,9,10))

# 숫자형 확인작업
#bike_copy
#class(bike_copy$year)
#class(bike_copy$day)
bike_copy
bike_copy <- subset(bike_copy, select=-c(X,Day,Month))
bike_copy
weather

class(london$month)
class(bike_copy$month)


# london year month day에 맞추어 weather 와 bike_copy 데이터프레임 합치기

london <- cbind(weather, bike_copy$bike_use)
london <- rename(london, 'bike_use'='bike_copy$bike_use' )
london
write.csv(london, file='london.csv')






