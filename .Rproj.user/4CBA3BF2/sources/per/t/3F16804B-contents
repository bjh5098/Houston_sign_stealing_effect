install.packages("data.table")
install.packages(c("ggplot", "ggplot2", "plyr", "RColorBrewer", "dplyr"))
install.packages("ggthemes")
library(data.table)
library(dplyr)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(scales)
library(ggthemes)

setwd('D:\\workspace\\Rproject\\termproject')
##2017년 휴스턴 투구별 타자기록 불러오기
data <- read.csv("HOU17_pitch.csv")
str(data)
View(data)
##필요한 타구정보만 subset
hou2017 <- subset(data, select = c(pitch_type,game_date,player_name,
                                  batter,events,description,home_team,away_team,
                                  type,hit_location,bb_type,balls,strikes,
                                  hit_distance_sc,launch_speed,launch_angle,
                                  effective_speed,pitch_name))
hou2017$launch_angle <- as.vector(hou2017$launch_angle)
hou2017$launch_speed <- as.vector(hou2017$launch_speed)
hou2017$hit_location <- as.vector(hou2017$hit_location)
hou2017$hit_distance_sc <- as.vector(hou2017$hit_distance_sc)
hou2017$launch_speed <- as.vector(hou2017$launch_speed)
hou2017$launch_angle <- as.vector(hou2017$launch_angle)
hou2017$effective_speed <- as.vector(hou2017$effective_speed)
View(hou2017)

#############################
# 홈에서 직구 파워타구 비율 #
#############################
Homefb <- subset(hou2017, home_team == "HOU" & 
                   (pitch_type == "FF" | pitch_type == "FT")& events != "null")
Homefbbarrel <- subset(Homefb, launch_speed >= 90 & launch_angle >= 10 & launch_angle <= 40)

# 선수별 직구타격 개수(홈)
Homecount <- lapply( split(Homefb, Homefb$player_name), nrow)
# 선수별 파워타구 개수(홈)
Homebarrelcount <- lapply( split(Homefbbarrel, Homefbbarrel$player_name), nrow) 
class(Homecount) 
View(Homecount)

result <- cbind(Homecount, Homebarrelcount)
result <- subset(result, Homecount > 10)
result$Homebarrelcount <- as.vector(result$Homebarrelcount)
result$Homecount <- as.vector(result$Homecount)

#boxstats <- boxplot(Homecount)
n <- c(158, 86, 17, 130, 108, 48, 70, 175, 12, 64, 144, 118, 19, 131, 48, 11, 17, 147)
boxstats <- boxplot(n, col="gray", main="Houston batter Fastball hit in 2017", ylab="hit count")
boxstats 

result
as.data.frame(result)
write.csv(result, file="Homebarrelrate2017.csv",row.names = TRUE)
Homebarrelrate2017 <- read.csv("Homebarrelrate2017.csv")
View(Homebarrelrate2017)
Homebarrelrate2017$Homebarrelrate2017 <- as.vector(Homebarrelrate2017$Homebarrelrate2017)
Homebarrelrate2017

#################################
#직구 어웨이 배럴타구 비율
#################################
Awayfb <- subset(hou2017, home_team != "HOU" & (pitch_type == "FF"| pitch_type == "FT")& events != "null") 
Awayfbbarrel <- subset(Awayfb, launch_speed >= 90 & launch_angle >= 10 & launch_angle <= 40)
#Awaybarrelper <- nrow(Awayfbbarrel) / nrow(Awayfb)
#Awaybarrelper

# 선수별 배럴타구 개수(어웨이)
Awaycount <- lapply( split(Awayfb, Awayfb$player_name), nrow)
# 선수별 배럴타구 비율(어웨이)
Awaybarrelcount <- lapply( split(Awayfbbarrel, Awayfbbarrel$player_name), nrow)

result2 <- cbind(Awaycount, Awaybarrelcount)
result2 <- subset(result2, Awaycount > 10)
result2$Awaybarrelcount <- as.vector(result2$Awaybarrelcount)
result2$Awaycount <- as.vector(result2$Awaycount)

n <- c(141, 111, 20, 111, 118, 43, 80, 137, 20, 70, 153, 143, 116, 14, 63, 16, 19, 145)
boxstats <- boxplot(n, col="gray", main="Houston batter Fastball hit in 2017 Away", ylab="hit count")
boxstats 

as.data.frame(result2)
write.csv(result2, file="Homebarrelrate2017away.csv",row.names = TRUE)
Homebarrelrate2017away <- read.csv("Homebarrelrate2017away.csv")
Homebarrelrate2017away$Homebarrelrate2017away <- as.vector(Homebarrelrate2017away$Homebarrelrate2017away)
Homebarrelrate2017away

############################
# 홈과 어웨이 비교
#############################
intersect(Homebarrelrate2017, Homebarrelrate2017away)
comparebarrelper <- inner_join(Homebarrelrate2017, Homebarrelrate2017away)
View(comparebarrelper)
class(comparebarrelper)
write.csv(comparebarrelper, file="Comparebarrelrate2017FB.csv",row.names = TRUE)
Comparebarrelrate2017FB <- read.csv("Comparebarrelrate2017FB.csv")
View(Comparebarrelrate2017FB)
arrange(Comparebarrelrate2017FB,desc(Homecount),desc(Awaycount))
View(Comparebarrelrate2017FB)

###################################
## 2017 메이저리그 평균 직구 파워히팅 비율
## 홈, 어웨이
data2 <- read.csv("2017MLBhome.csv")
mlb2017home <- subset(data2, select = c(pitch_type,game_date,player_name,
                                   batter,events,description,home_team,away_team,
                                   type,hit_location,bb_type,balls,strikes,
                                   hit_distance_sc,launch_speed,launch_angle,effective_speed,
                                   pitch_name))
mlb2017home$launch_angle <- as.vector(mlb2017home$launch_angle)
mlb2017home$launch_speed <- as.vector(mlb2017home$launch_speed)
mlb2017home$hit_location <- as.vector(mlb2017home$hit_location)
mlb2017home$hit_distance_sc <- as.vector(mlb2017home$hit_distance_sc)
mlb2017home$launch_speed <- as.vector(mlb2017home$launch_speed)
mlb2017home$launch_angle <- as.vector(mlb2017home$launch_angle)
mlb2017home$effective_speed <- as.vector(mlb2017home$effective_speed)

mlbHomefb <- subset(mlb2017home, (pitch_type == "FF" | pitch_type == "FT")& events != "null")
mlbHomefbbarrel <- subset(mlbHomefb, launch_speed >= 90 & launch_angle >= 10 & launch_angle <= 40)

mlbHomefbbarrelrate <- nrow(mlbHomefbbarrel) / nrow(mlbHomefb)
mlbHomefbbarrelrate #0.1105027

####################
#어웨이 mlb2017 평균
data3 <- read.csv("2017MLBaway.csv")
mlb2017away <- subset(data3, select = c(pitch_type,game_date,player_name,
                                       batter,events,description,home_team,away_team,
                                       type,hit_location,bb_type,balls,strikes,
                                       hit_distance_sc,launch_speed,launch_angle,effective_speed,
                                       pitch_name))
mlb2017away$launch_angle <- as.vector(mlb2017away$launch_angle)
mlb2017away$launch_speed <- as.vector(mlb2017away$launch_speed)
mlb2017away$hit_location <- as.vector(mlb2017away$hit_location)
mlb2017away$hit_distance_sc <- as.vector(mlb2017away$hit_distance_sc)
mlb2017away$launch_speed <- as.vector(mlb2017away$launch_speed)
mlb2017away$launch_angle <- as.vector(mlb2017away$launch_angle)
mlb2017away$effective_speed <- as.vector(mlb2017away$effective_speed)

mlbawayfb <- subset(mlb2017away, (pitch_type == "FF" | pitch_type == "FT")& events != "null")
mlbawayfbbarrel <- subset(mlbawayfb, launch_speed >= 90 & launch_angle >= 10 & launch_angle <= 40)

mlbawayfbbarrelrate <- nrow(mlbawayfbbarrel) / nrow(mlbawayfb)
mlbawayfbbarrelrate #0.1120868


##########################################
# 17년도 HOU소속선수의 16년도 홈 파워타구 
data4 <- read.csv("2016HOUstat.csv")
hou2016 <- subset(data4, select = c(pitch_type,game_date,player_name,
                                   batter,events,description,home_team,away_team,
                                   type,hit_location,bb_type,balls,strikes,
                                   hit_distance_sc,launch_speed,launch_angle,effective_speed,
                                   pitch_name))
hou2016$launch_angle <- as.vector(hou2016$launch_angle)
hou2016$launch_speed <- as.vector(hou2016$launch_speed)
hou2016$hit_location <- as.vector(hou2016$hit_location)
hou2016$hit_distance_sc <- as.vector(hou2016$hit_distance_sc)
hou2016$launch_speed <- as.vector(hou2016$launch_speed)
hou2016$launch_angle <- as.vector(hou2016$launch_angle)
hou2016$effective_speed <- as.vector(hou2016$effective_speed)

View(hou2016)
# 홈에서 직구 파워타구 비율
Homefb16 <- subset(hou2016, (pitch_type == "FF" | pitch_type == "FT")& events != "null")
Homefbbarrel16 <- subset(Homefb16, launch_speed >= 90 & launch_angle >= 10 & launch_angle <= 40)
#Homebarrelper <- nrow(Homefbbarrel) / nrow(Homefb)
#Homebarrelper
View(Homefbbarrel16)
# 선수별 직구타격 개수(홈)
Homecount16 <- lapply( split(Homefb16, Homefb16$player_name), nrow)
# 선수별 파워타구 개수(홈)
Homebarrelcount16 <- lapply( split(Homefbbarrel16, Homefbbarrel16$player_name), nrow) 
class(Homecount16) 
class(Homebarrelcount16)
View(Homecount16)
View(Homebarrelcount16)

cbind(Homecount16, Homebarrelcount16)
result16 <- cbind(Homecount16, Homebarrelcount16)
View(result16)
result16
result16 <- subset(result16, Homecount16 > 10)
result16

result16$Homebarrelcount16 <- as.vector(result16$Homebarrelcount16)
result16$Homecount16 <- as.vector(result16$Homecount16)
View(result16)

as.data.frame(result16)
write.csv(result16, file="Homebarrelrate2016.csv",row.names = TRUE)
Homebarrelrate2016 <- read.csv("Homebarrelrate2016.csv")

View(Homebarrelrate2016)
Homebarrelrate2016$Homebarrelrate2016 <- as.vector(Homebarrelrate2016$Homebarrelrate2016)
Homebarrelrate2016
#평균 0.107007973

###Viz
hou1617 <- read.csv("Comparebarrelrate1617.csv")
hou1617
#View(hou1617)
g1 <- ggplot(data = hou1617, aes(x = reorder(name,-Hitcount), y = Home17_perc, size = Hitcount)) + 
  geom_point(colour='red') +
  geom_text(aes(label = Home17_perc),hjust = 0, vjust = -1, show_guide=F,  check_overlap=TRUE) + 
  labs(title = "HOU17 BarrelHit rate", subtitle="Compare with Home/Away", caption = "refer to Baseballsavant.mlb.com", x = "선수이름", y="BarrelHit %") +
  geom_hline(yintercept = mlbHomefbbarrelrate*100)

g1 
#geom_line(aes(y=mlbHomefbbarrelrate*100), color="blue", linetype=1)
g2 <- g1 + geom_point(aes(y=Away17_perc),colour='black') 
#+ geom_text(aes(label=name), hjust = 1.2, vjust = 1)
g3 <- g2 + geom_segment(aes(x = "Alex Bregman", y = 14.89, 
  xend = "Alex Bregman", yend = 11.39), 
  arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1.3) +
  geom_segment(aes(x = "Yuli Gurriel", y = 13.1, 
  xend = "Yuli Gurriel", yend = 14.29), 
  arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Jose Altuve", y = 10.46, 
  xend = "Jose Altuve", yend = 11.11), 
  arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Carlos Beltran", y = 6.31, 
  xend = "Carlos Beltran", yend = 13.85), 
  arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Josh Reddick", y = 13.99, 
  xend = "Josh Reddick", yend = 11.02), 
  arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1.3)+
  geom_segment(aes(x = "Carlos Correa", y = 6.78, 
  xend = "Carlos Correa", yend = 12.04), 
  arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Brian McCann", y = 12.61, 
                   xend = "Brian McCann", yend = 16.28), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Evan Gattis", y = 17.5, 
                   xend = "Evan Gattis", yend = 14.29), 
               arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1)+
  geom_segment(aes(x = "Jake Marisnick", y = 4.29, 
                   xend = "Jake Marisnick", yend = 7.81), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1)

opar <- par(mfrow=c(1,2))
opar

g3 + theme_classic()





####################16년도

g11 <- ggplot(data = hou1617, aes(x = reorder(name,-Home16count), y = Home17_perc, size = Hitcount)) + 
  geom_point(colour='red') +
  geom_text(aes(label = Home17_perc),hjust = 0, vjust = -1, show_guide=F,  check_overlap=TRUE) + 
  labs(title = "HOU17 BarrelHit rate", subtitle="Compare with 16/17 Home", caption = "refer to Baseballsavant.mlb.com", x = "선수이름", y="BarrelHit %") +
  geom_hline(yintercept = 0.107007973*100)

g11 
#geom_line(aes(y=mlbHomefbbarrelrate*100), color="blue", linetype=1)
g12 <- g11 + geom_point(aes(y=Home16_perc),colour='black',size = hou1617$Home16count/28) 
g12

g13 <- g12 + geom_segment(aes(x = "Yuli Gurriel", y = 16.22, 
                   xend = "Yuli Gurriel", yend = 14.29), 
               arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1.3)+
  geom_segment(aes(x = "Jose Altuve", y = 18.63, 
                   xend = "Jose Altuve", yend = 11.11), 
               arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1.3)+
  geom_segment(aes(x = "Carlos Beltran", y = 7.59, 
                   xend = "Carlos Beltran", yend = 13.85), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Josh Reddick", y = 15.85, 
                   xend = "Josh Reddick", yend = 11.02), 
               arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1.3)+
  geom_segment(aes(x = "Carlos Correa", y = 10.53, 
                   xend = "Carlos Correa", yend = 12.04), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Brian McCann", y = 7.44, 
                   xend = "Brian McCann", yend = 16.28), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Evan Gattis", y = 6.49, 
                   xend = "Evan Gattis", yend = 14.29), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "George Springer", y = 5.59, 
                   xend = "George Springer", yend = 10.29), 
               arrow=arrow(length=unit(0.4,'cm')), color='red',size=1.3)+
  geom_segment(aes(x = "Marwin Gonzalez", y = 17.24, 
                   xend = "Marwin Gonzalez", yend = 12.98), 
               arrow=arrow(length=unit(0.4,'cm')), color='blue',size=1.3)


g13 + theme_classic()
par(opar)
