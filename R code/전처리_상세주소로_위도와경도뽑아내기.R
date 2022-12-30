# install.packages("tidyverse")
# install.packages("ggmap")
library(tidyverse)
library(ggmap)
library(stringr)
library(dplyr)
library(reshape2)

### 4647,4661번 서울교통공사 역별 주소 현황 및 전화번호.csv
file <- file.choose()
raw.subway <- read.csv(file,header=T)
View(raw.subway)
head(raw.subway)

### 데이터 융합을 위한 이름 통일
locater3 <-str_locate(raw.subway$역명,"\\(")
num3 <- locater3[,1]
num3[which(is.na(num3))] <- str_length(raw.subway$역명[which(is.na(num3))])+1
num3
a=1
for(i in num3){
  raw.subway$역명[a] <- substr(raw.subway$역명[a],1,(i-1))
  a=a+1
}

### 필요한 변수만 선택
subway_use <- raw.subway[,2:6]

subway.add = subway_use %>%
  select(c(2,1,5)) %>%
  rename(line = 호선, station = 역명, address = 상세주소) %>%
  separate(address, c('address', 'etc'), sep = '[(]') %>%
  select(-etc)

### 구글맵스를 이용한 지하철역 위도와 경도
register_google(key='AIzaSyCUQwPaVddCgYEs0JcTJ4Q6QCFlcMh7fks')
coordinate = subway.add$address %>% enc2utf8() %>% geocode()

## 위도와 경도 추가
subway.lon.lat = bind_cols(subway.add, coordinate)
subway.lon.lat %>% head()
subway.lon.lat$station[which(subway.lon.lat$station=="구로디지털단지")]<-"구로디지털"
subway.lon.lat$line <- paste0(subway.lon.lat$line,"호선")
write.csv(subway.lon.lat,"subway.csv",row.names = F)